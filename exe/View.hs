{-|
 - Module: Reflex.Vty.GHCi
 - Description: Vty widgets useful when building your own GHCi runner
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
module View where

import Control.Monad ((<=<))
import Control.Monad.Fix (MonadFix)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.UTF8 as BSU      -- from utf8-string
import qualified Data.Map as M
import Data.List

import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens (range, message)
import Control.Lens (view)

import Reflex.Process
import Reflex.Vty
import Types

-- | Display the overall status of the session
statusDisplay
  :: ( PostBuild t m
     , MonadHold t m
     )
  => Session t
  -> VtyWidget t m ()
statusDisplay g = do
  text <=< hold "" $ leftmost
    [ ("Command exited with " <>) . T.pack . show <$> _process_exit (rawProcess g)
    ]

-- | A scrollable widget that displays a message at the bottom of the widget
-- when there is additional content to view.
scrollableOutput
  :: ( Reflex t
     , MonadNodeId m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     )
  => Behavior t ByteString
  -> VtyWidget t m ()
scrollableOutput out = col $ do
  dh <- displayHeight
  scroll <- stretch $ scrollableText never $ T.decodeUtf8 <$> out
  fixed 1 $ text $
    let f h (ix, n) = if n - ix + 1 > h
          then "↓ More ↓"
          else ""
    in f <$> current dh <*> scroll

-- | A scrollable widget that scrolls down as output goes past the end of the widget
scrollingOutput
  :: ( Reflex t
     , Monad m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t ByteString
  -> VtyWidget t m ()
scrollingOutput out = do
  dh <- displayHeight
  let scrollBy h (ix, n) =
        if | ix == 0 && n <= h -> Nothing -- Scrolled to the top and we don't have to scroll down
           | n > h && n - ix - h == 0 -> Just 1
           | otherwise -> Nothing
  rec scroll <- scrollableText (tagMaybe (scrollBy <$> current dh <*> scroll) $ updated out) $
        T.decodeUtf8 <$> current out
  return ()

debugView
  :: ( MonadNodeId m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Session t
  -> VtyWidget t m ()
debugView g = col $ do
  let DebugCollection{debug_out, debug_in, debug_err} = debugCollection g
  fixed 3 $ boxStatic def $ statusDisplay g
  fixed 20 $ scrollableOutput $ debug_out
  fixed 20 $ scrollableOutput $ debug_in
  stretch $ scrollableOutput $ debug_err

collectOutput
  :: (Reflex t, MonadFix m, MonadHold t m)
  => Event t ()
  -- ^ Clear output
  -> Event t ByteString
  -- ^ Output to add
  -> m (Dynamic t ByteString)
collectOutput clear out = foldDyn ($) "" $ leftmost [ flip mappend <$> out
                                                    , const "" <$ clear ]

mkDebugOutput :: (Reflex t, MonadFix m, MonadHold t m, Show a)
              => MessageIn t -> Process t a
              -> m (DebugCollection t)
mkDebugOutput i p = do
  sout <- collectOutput never (BSU.fromString . show <$> _process_stdout p)
  iout <- collectOutput never (BSU.fromString . show <$> i)
  serr <- collectOutput never (_process_stderr p)
  return $ DebugCollection (current sout) (current iout) (current serr)

diagnosticsPane :: (PostBuild t m, MonadNodeId m, MonadHold t m,
                       MonadFix m) => Session t -> VtyWidget t m ()
diagnosticsPane s =
  let ds = BSU.fromString
            . unlines
            . intersperse "---"
            . map moduleString
            . M.toList
            <$> diagnostics s
  in col $ stretch $ scrollableOutput $ current ds

moduleString :: (Uri, [Diagnostic]) -> String
moduleString (uri, ds) =
  unlines [T.unpack (getUri uri), renderDiags ds]

renderDiags :: [Diagnostic] -> String
renderDiags = unlines . map renderDiag

renderDiag :: Diagnostic -> String
renderDiag d = renderRange (view range d) <> ":" <> show (view message d)

renderRange :: Range -> String
renderRange (Range s e) = renderPosition s ++ "-" ++ renderPosition e

renderPosition :: Position -> String
renderPosition (Position l c) = show l ++ ":" ++ show c
