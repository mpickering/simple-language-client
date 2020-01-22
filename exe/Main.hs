{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where


{- Plan
-
- 1. Start the LSP server using reflex-process and provide an event for
- input/output
- 2. Watch for file changes in the current directory using fsnotify
- 3. Send a notification to the server on a file change
- 4. Display the diagnostics when ghcide reports them
-}

import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import Data.Default
import Reflex
import qualified System.Posix.Process
import System.Process (proc)
import Reflex.Process
import Control.Monad.IO.Class
import qualified Data.Text as T
import Language.Haskell.LSP.Types.Capabilities
import System.Directory
import Reflex.FSNotify (watchDirectory)
import qualified System.FSNotify as FS
import System.FilePath
import System.FilePath.Find

import Reflex.Vty

import qualified Graphics.Vty.Input as V
import qualified Data.Text.IO as T

import Reflex.Network


import Options.Applicative hiding (switch)
import Control.Monad(void)
import Control.Monad.Fix

import Types
import Process
import View

getCurrentProcessID :: Num b => IO b
getCurrentProcessID = fromIntegral <$> System.Posix.Process.getProcessID

getHSFiles ::  FilePath -> IO [FilePath]
getHSFiles = findWithHandler (\_ _ -> return []) always
              (extension ==? ".hs"
                ||? extension ==? ".lhs")


-- | If the path is a directory then find all files in that directory,
-- otherwise just load the single file
pathToFiles :: FilePath -> FilePath -> IO [FilePath]
pathToFiles root_dir spec = do
  let dpath = if isRelative spec
                then root_dir </> spec
                else spec
  dexist <- doesDirectoryExist dpath
  if dexist
    then getHSFiles dpath
    -- Probably a file
    else return [dpath]

watchHSDirectory :: (PostBuild t m, TriggerEvent t m, PerformEvent t m,
                       MonadIO (Performable m)) => FilePath -> m (Event t FS.Event)
watchHSDirectory absRootDir = do
  -- TODO: Separate the filesystem event logic into its own function
  -- Watch the project directory for changes
  pb <- getPostBuild
  fsEvents <- watchDirectory (noDebounce FS.defaultConfig) (absRootDir <$ pb)

  let filteredFsEvents = flip ffilter fsEvents $ \e ->
        takeExtension (FS.eventPath e) `elem` [".hs", ".lhs"]
  return filteredFsEvents


startSession :: (Reflex t
                , TriggerEvent t m
                , PerformEvent t m
                , MonadIO (Performable m)
                , MonadHold t m
                , MonadIO m
                , MonadIO (Performable m)
                , PostBuild t m
                , MonadFix m)
                => String
                -> [String]
                -> ClientCapabilities
                -> FilePath
                -> FilePath
                -> m (Session t)
startSession cmd args caps rootDir target = mdo
  pid <- liftIO $ getCurrentProcessID
  absRootDir <- liftIO $ canonicalizePath rootDir
  let initializeParams = InitializeParams (Just pid)
                                        (Just $ T.pack absRootDir)
                                        (Just $ filePathToUri absRootDir)
                                        Nothing
                                        caps
                                        (Just TraceOff)
                                        Nothing

  -- Start watching the root directory for changes
  hsEvents <- watchHSDirectory absRootDir
  docModify <- performEvent (liftIO . fsNotifyToRequest <$> hsEvents)
  -- Make an event which can be triggered manually
  (messageIn, sendMessage) <- mkMessageIn

  let in_message = leftmost [messageIn, docModify]

  let process =  proc cmd args
      processConfig =
        ProcessConfig
          (attachPromptlyDynWith (\b a -> (a (IdInt b))) (counter st) in_message)
          never

  -- Start language server
  p <- createLSPProcess process processConfig

  -- Send initialisation request
  liftIO $ sendMessage (mkInitialiseRequest initializeParams)

  -- Open all files as specified by the target
  liftIO $ do
    iniFiles <-  pathToFiles absRootDir target
    mapM_ (\iniFile -> sendMessage =<< (openFile iniFile)) iniFiles

  st <- mkClientState sendMessage in_message

  debug <- mkDebugOutput (_processConfig_stdin processConfig) p
  diags <- mkDiags p
  return (Session debug diags p)

mkDiags :: (Reflex t, MonadHold t m, MonadFix m) => LSPProcess t
        -> m (Dynamic t DiagMap)
mkDiags p = foldDyn update emptyDiagMap (_process_stdout p)
  where
    update :: FromServerMessage -> DiagMap -> DiagMap
    update (NotPublishDiagnostics (NotificationMessage _ _ (PublishDiagnosticsParams uri (List ds)))) d
      = case ds of
          [] -> deleteDiag uri d
          _ -> addDiag uri ds d

    update _ d = d

mkInitialiseRequest :: InitializeParams -> (LspId -> FromClientMessage)
mkInitialiseRequest p i = ReqInitialize (RequestMessage "2.0" i Initialize p)

fsNotifyToRequest :: FS.Event -> IO (LspId -> FromClientMessage)
fsNotifyToRequest e = do
  t <- T.readFile (FS.eventPath e)
  -- HACK, shouldn't use IdInt like this but need to increment the version
  -- each time.
  return $ \(IdInt i) -> NotDidChangeTextDocument (NotificationMessage "2.0" TextDocumentDidChange
    (DidChangeTextDocumentParams
      (VersionedTextDocumentIdentifier (filePathToUri (FS.eventPath e)) (Just i))
      (List [TextDocumentContentChangeEvent Nothing Nothing t])
      ))


openFile :: FilePath -> IO (LspId -> FromClientMessage)
openFile fp = do
  t <- T.readFile fp
  return $ \_ -> NotDidOpenTextDocument (NotificationMessage "2.0" TextDocumentDidOpen
    (DidOpenTextDocumentParams
      (TextDocumentItem (filePathToUri fp) "haskell" 0 t)))

mkMessageIn :: TriggerEvent t m
            => m (Event t (LspId -> FromClientMessage)
                 , (LspId -> FromClientMessage) -> IO () )
mkMessageIn = newTriggerEvent

mkClientState :: (Reflex t, MonadHold t m, MonadFix m)
              => ((LspId -> FromClientMessage) -> IO ())
              -> Event t a
              -> m (ClientState t)
mkClientState send messageIn = do
    c <- count messageIn
    return $ ClientState { sendRequest = send
                         , counter = c }


data ClientArg = ClientArg
  { _clientArg_serverCommand :: String
  , _clientArg_files :: FilePath
  , _clientArg_root_dir :: Maybe FilePath
  }

ghciArg :: Parser ClientArg
ghciArg = ClientArg
  <$> strOption
    ( long "server" <>
      short 's' <>
      help "Path to the language server" <>
      showDefault <>
      value "ghcide"
    )
  <*> argument str
    (
      help "The directory containing the source files to load"
      <> metavar "DIR"
    )
  <*> optional (strOption (long "root-dir" <> help "Path to root dir"))


main :: IO ()
main = do
  let opts = info (ghciArg <**> helper) $ mconcat
        [ fullDesc
        , progDesc "A language client powered by fsnotify"
       , header "Welcome to reflex-ghcide"
        ]
  ClientArg { _clientArg_serverCommand = cmd
            , _clientArg_files = file_dir
            , _clientArg_root_dir = mroot_dir } <- execParser opts
  root_dir <- maybe getCurrentDirectory return mroot_dir
  mainWidget $ mdo
    exit <- keyCombo (V.KChar 'c', [V.MCtrl])
    d <- key (V.KChar 'd')

    session <- startSession cmd ["--lsp"] def root_dir file_dir

    let home = col $ do
          stretch $ col $ do
            stretch $ diagnosticsPane session
            fixed 3 $ boxStatic def $ text "reflex-ghcide: C-c - quit; d - debug"
          return $ leftmost
            [ Left () <$ d
            ]
    rec out <- networkHold home $ ffor (switch (current out)) $ \case
          Left () -> escapable (debugView session)
          Right () -> home

    let escapable w = do
          void w
          i <- input
          return $ fforMaybe i $ \case
            V.EvKey V.KEsc [] -> Just $ Right ()
            _ -> Nothing

    return $ () <$ exit

noDebounce :: FS.WatchConfig -> FS.WatchConfig
noDebounce cfg = cfg { FS.confDebounce = FS.NoDebounce }
