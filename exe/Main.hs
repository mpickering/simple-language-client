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
startSession cmd args caps rootDir iniFile = mdo
  pid <- liftIO $ getCurrentProcessID
  absRootDir <- liftIO $ canonicalizePath rootDir
  let initializeParams = InitializeParams (Just pid)
                                        (Just $ T.pack absRootDir)
                                        (Just $ filePathToUri absRootDir)
                                        Nothing
                                        caps
                                        (Just TraceOff)
                                        Nothing
  -- TODO: Separate the filesystem event logic into its own function
  -- Watch the project directory for changes
  pb <- getPostBuild
  fsEvents <- watchDirectory (noDebounce FS.defaultConfig) (absRootDir <$ pb)

  let filteredFsEvents = flip ffilter fsEvents $ \e ->
        takeExtension (FS.eventPath e) `elem` [".hs", ".lhs"]
  docOpen <- performEvent (liftIO . fsNotifyToRequest <$> filteredFsEvents)
  (messageIn, sendMessage) <- mkMessageIn
  let in_message = leftmost [messageIn, docOpen]

  let process =  proc cmd args
      processConfig =
        ProcessConfig
          (attachPromptlyDynWith (\b a -> (a (IdInt b))) (counter st) in_message)
          never
  p <- createLSPProcess process processConfig

  liftIO $ sendMessage (mkInitialiseRequest initializeParams)
  liftIO $ sendMessage =<< (openFile iniFile)


  st <- mkClientState sendMessage in_message

  debug <- mkDebugOutput (_processConfig_stdin processConfig) p
  diags <- mkDiags p
  return (Session debug diags p)

mkDiags :: (Reflex t, MonadHold t m, MonadFix m) => Process t FromServerMessage -> m (Dynamic t DiagMap)
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
  -- HACK, shouldn't use IdInt like this
  return $ \(IdInt i) -> NotDidChangeTextDocument (NotificationMessage "2.0" TextDocumentDidChange
    (DidChangeTextDocumentParams
      (VersionedTextDocumentIdentifier (filePathToUri (FS.eventPath e)) (Just i))
      (List [TextDocumentContentChangeEvent Nothing Nothing t])
      ))

{-
  NotDidSaveTextDocument (NotificationMessage "2.0" TextDocumentDidSave
    (DidSaveTextDocumentParams
      (TextDocumentIdentifier (filePathToUri (FS.eventPath e)))))
      -}


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
  }

ghciArg :: Parser ClientArg
ghciArg = ClientArg
  <$> strOption
    ( long "path" <>
      short 'p' <>
      help "Path to the language server" <>
      showDefault <>
      value "ghcide"
    )
  <*> argument str
    (
      help "The directory containing the source files to load"
      <> metavar "DIR"
    )


main :: IO ()
main = do
    {-
  let opts = info (ghciArg <**> helper) $ mconcat
        [ fullDesc
        , progDesc "A language client powered by fsnotify"
       , header "Welcome to reflex-ghcide"
        ]
        -}
--  ClientArg { _clientArg_serverCommand = _cmd, _clientArg_files = _file_dir } <- execParser opts
  let root_dir = "/home/matt/ghcide" :: String
  mainWidget $ mdo
    exit <- keyCombo (V.KChar 'c', [V.MCtrl])
    d <- key (V.KChar 'd')

    let file_dir = "/home/matt/ghcide/src/Development/IDE/Core/Rules.hs"
        cmd = "/home/matt/ghcide/dist-newstyle/build/x86_64-linux/ghc-8.6.5/ghcide-0.0.6/x/ghcide/build/ghcide/ghcide"
    session <- startSession cmd ["--lsp"] def root_dir file_dir


    let home = col $ do
          stretch $ col $ do
            fixed 1 $ text "Esc will bring you back here."
            fixed 1 $ text "Ctrl+c to quit."
            fixed 1 $ text "d - debug panes"
            stretch $ diagnosticsPane session
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
