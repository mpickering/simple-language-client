{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where


{- Plan
-
- 1. Start the LSP server using reflex-process and provide an event for
- input/output
- 2. Watch for file changes in the current directory using fsnotify
- 3. Send a notification to the server on a file change
- 4. Display the diagnostics when ghcide reports them
-}
import Control.Monad.Extra
import Data.Maybe hiding (mapMaybe)
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types hiding (_workspace)
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
import System.Posix.Signals
import Control.Retry
import Control.Monad.Catch (Handler(..))


import Control.Exception hiding (Handler(..))

import Reflex.Vty

import qualified Graphics.Vty.Input as V
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Aeson as A
import qualified Data.Foldable as F

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
  -- Should use the watched files to perform the filtering
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
                => Event t ()
                -> String
                -> [String]
                -> ClientCapabilities
                -> FilePath
                -> [FilePath]
                -> m (Session t)
startSession exit cmd args caps rootDir targets = mdo
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
  docModify <- performEvent (liftIO . fsNotifyToRequest sendOpen <$> attachPromptlyDyn openFiles hsEvents)
  -- Make an event which can be triggered manually
  (messageIn, sendMessage) <- mkMessageIn

  (open_e, sendOpen) <- newTriggerEvent

  let in_message = leftmost [messageIn, mapMaybe id docModify]

  let process =  proc cmd args
      processConfig =
        ProcessConfig
          (attachPromptlyDynWith (\b a -> (a (IdInt b))) (counter st) in_message)
          (sigINT <$ exit)

  -- Start language server
  p <- createLSPProcess process processConfig

  -- Send initialisation request
  liftIO $ sendMessage (mkInitialiseRequest initializeParams)

  -- Open all files as specified by the target
  liftIO $ do
    iniFiles <-  concatMapM (pathToFiles absRootDir) targets
    -- Choose one to get things going, further files are opened when
    -- fsnotify says they have changed.
    case iniFiles of
      [] -> return ()
      (f:_) -> sendOpen f

  openFiles <- foldDyn Set.insert Set.empty opened_files

  let do_open f = openFile f >>= (f <$) . sendMessage
  opened_files <- performEvent (liftIO . do_open <$> open_e)

  st <- mkClientState sendMessage in_message

  debug <- mkDebugOutput (_processConfig_stdin processConfig) p
  diags <- mkDiags p
  status <- mkStatus p
  return (Session debug diags status p)

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

mkStatus :: (Reflex t, MonadHold t m, MonadFix m) => LSPProcess t
        -> m (Dynamic t ProgressStatus)
mkStatus p = foldDyn update (ProgressStatus "" M.empty) (_process_stdout p)
  where
    update :: FromServerMessage -> ProgressStatus -> ProgressStatus
    update (NotWorkDoneProgressBegin (NotificationMessage _ _ (ProgressParams tok (WorkDoneProgressBeginParams tit _mc mm mp)))) (ProgressStatus _t ps) =
      ProgressStatus (renderProgress tit mm mp) (M.insert tok tit ps)
    update (NotWorkDoneProgressReport (NotificationMessage _ _ (ProgressParams tok (WorkDoneProgressReportParams _mc mm mp)))) prog@(ProgressStatus _t ps) =
      let mtit = M.lookup tok ps
      in case mtit of
           -- This case should never happen
           Nothing -> prog
           Just tit -> ProgressStatus (renderProgress tit mm mp) ps

    update (NotWorkDoneProgressEnd (NotificationMessage _ _ (ProgressParams tok (WorkDoneProgressEndParams _mm)))) (ProgressStatus t ps) =
      let ps' = M.delete tok ps
      in if M.null ps'
          then ProgressStatus "" ps'
          else ProgressStatus t ps

    update _ d = d


getNewWatcher :: FromServerMessage -> [FileSystemWatcher]
getNewWatcher (ReqRegisterCapability (RequestMessage _t _i _m (RegistrationParams rs))) =
  concat $ mapMaybe processRegistration (F.toList rs)
  where
    processRegistration :: Registration -> Maybe [FileSystemWatcher]
    processRegistration (Registration _rid WorkspaceDidChangeWatchedFiles (Just args)) =
      case A.fromJSON args of
        A.Error _s -> Nothing
        A.Success (DidChangeWatchedFilesRegistrationOptions { watchers = List ws })
          -> Just ws
getNewWatcher _ = []

renderProgress :: T.Text -> Maybe T.Text -> Maybe Double -> T.Text
renderProgress rhead mm mp = p <> rhead <> m
  where
    p = fromMaybe "" ((<> " ") . T.pack . show <$> mp)
    m = fromMaybe "" ((": " <>) <$> mm)

mkInitialiseRequest :: InitializeParams -> (LspId -> FromClientMessage)
mkInitialiseRequest p i = ReqInitialize (RequestMessage "2.0" i Initialize p)

fsNotifyToRequest :: (FilePath -> IO ()) -> (Set.Set FilePath, FS.Event) -> IO (Maybe (LspId -> FromClientMessage))
fsNotifyToRequest open_not (os, e) = do
  let fp = FS.eventPath e
  mt <- readFileRetry fp
  case mt of
    Nothing -> return Nothing
    Just t
      | fp `Set.member` os ->
          -- HACK, shouldn't use IdInt like this but need to increment the version
          -- each time.
           return $ Just $ \(IdInt i) -> NotDidChangeTextDocument (NotificationMessage "2.0" TextDocumentDidChange
            (DidChangeTextDocumentParams
              (VersionedTextDocumentIdentifier (filePathToUri (FS.eventPath e)) (Just i))
              (List [TextDocumentContentChangeEvent Nothing Nothing t])
            ))
      | otherwise -> do
          open_not fp
          return $ Just $ openFileNot fp t


-- Try three times, for a timing issue but then return Nothing if it
-- doesn't exist still.
readFileRetry :: FilePath -> IO (Maybe T.Text)
readFileRetry fp =
  let policy = limitRetries 3
      retry_act = recovering policy
                    [\_ -> Handler (\(_e :: IOException) -> return True)]
                    (\_ -> T.readFile fp)
  in (Just <$> retry_act) `catch` (\(_ :: IOException) -> return Nothing)


openFile :: FilePath -> IO (LspId -> FromClientMessage)
openFile fp = do
  t <- T.readFile fp
  return $ openFileNot fp t

openFileNot :: FilePath -> T.Text -> LspId -> FromClientMessage
openFileNot fp t _ =
    NotDidOpenTextDocument (NotificationMessage "2.0" TextDocumentDidOpen
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
  , _clientArg_files :: [FilePath]
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
  <*> some (argument str
    (
      help "The directories containing the source files to load"
      <> metavar "DIRS"
    ))
  <*> optional (strOption (long "root-dir" <> help "Path to root dir"))


lcCaps :: ClientCapabilities
lcCaps = def { _window = Just (WindowClientCapabilities (Just True)) }

main :: IO ()
main = do
  let opts = info (ghciArg <**> helper) $ mconcat
        [ fullDesc
        , progDesc "A language client powered by fsnotify"
       , header "Welcome to simple-language-client"
        ]
  ClientArg { _clientArg_serverCommand = cmd
            , _clientArg_files = file_dir
            , _clientArg_root_dir = mroot_dir } <- execParser opts
  root_dir <- maybe getCurrentDirectory return mroot_dir
  mainWidget $ mdo
    exit <- (() <$) <$> keyCombo (V.KChar 'c', [V.MCtrl])
    d <- key (V.KChar 'd')

    session <- startSession exit cmd ["--lsp"] lcCaps root_dir file_dir

    let home = col $ do
          stretch $ col $ do
            stretch $ diagnosticsPane session
            fixed 3 $ boxStatic def $ text (current (currentMessage <$> status session))
            fixed 3 $ boxStatic def $ text "simple-language-client: C-c - quit; d - debug"
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

    return exit

noDebounce :: FS.WatchConfig -> FS.WatchConfig
noDebounce cfg = cfg { FS.confDebounce = FS.NoDebounce }


---
--------------------------------------------------------------------------------------------------
-- The message definitions below probably belong in haskell-lsp-types
-- Copied from ghcide

data DidChangeWatchedFilesRegistrationOptions = DidChangeWatchedFilesRegistrationOptions
    { watchers :: List FileSystemWatcher
    }

instance A.ToJSON DidChangeWatchedFilesRegistrationOptions where
  toJSON DidChangeWatchedFilesRegistrationOptions {..} =
    A.object ["watchers" A..= watchers]

instance A.FromJSON DidChangeWatchedFilesRegistrationOptions where
  parseJSON =
    A.withObject "didChange"
      (\o ->  DidChangeWatchedFilesRegistrationOptions <$> (o A..: "watchers"))

data FileSystemWatcher = FileSystemWatcher
    { -- | The glob pattern to watch.
      --   For details on glob pattern syntax, check the spec: https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/#workspace_didChangeWatchedFiles
      globPattern :: String
        -- | The kind of event to subscribe to. Defaults to all.
        --   Defined as a bitmap of Create(1), Change(2), and Delete(4)
    , kind        :: Maybe Int
    }

instance A.ToJSON FileSystemWatcher where
  toJSON FileSystemWatcher {..} =
    A.object
      $  ["globPattern" A..= globPattern]
      ++ [ "kind" A..= x | Just x <- [kind] ]

instance A.FromJSON FileSystemWatcher where
  parseJSON = A.withObject "watcher"  (\o -> FileSystemWatcher <$> o A..: "globPattern"
                                                               <*> o A..:? "kind" )


