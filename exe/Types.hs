{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Types where


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
import Reflex
import Reflex.Process
import Data.ByteString (ByteString)


import qualified Data.Map as M
import Data.Text (Text)



type MessageOut t = Event t FromServerMessage
type MessageIn t = Event t FromClientMessage

type LSPProcess t = Process t FromServerMessage ByteString

type DiagMap = M.Map Uri [Diagnostic]

data ProgressStatus = ProgressStatus
                        { currentMessage :: Text
                        , progresses :: M.Map ProgressToken Text
                        }

emptyDiagMap :: DiagMap
emptyDiagMap = M.empty

addDiag :: Uri -> [Diagnostic] -> DiagMap -> DiagMap
addDiag = M.insert

deleteDiag :: Uri -> DiagMap -> DiagMap
deleteDiag = M.delete

data Session t = Session { debugCollection :: DebugCollection t
                         , diagnostics :: Dynamic t DiagMap
                         , status :: Dynamic t ProgressStatus
                         , rawProcess :: LSPProcess t
                         }

-- Information displayed in the debug pane
data DebugCollection t = DebugCollection { debug_out, debug_in, debug_err :: Behavior t ByteString }

data ClientState t = ClientState { sendRequest :: (LspId -> FromClientMessage) -> IO ()
                                 , counter :: Dynamic t Int
                                 }

