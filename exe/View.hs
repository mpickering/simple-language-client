{-|
 - Module: Reflex.Vty.GHCi
 - Description: Vty widgets useful when building your own GHCi runner
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module View where

import Control.Monad ((<=<), void)
import Control.Monad.Fix (MonadFix)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.UTF8 as BSU      -- from utf8-string
import qualified Data.Map as M
import qualified Graphics.Vty as V
import qualified Data.Text.Zipper as TZ
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

scrollingOutputX
  :: ( Reflex t
     , Monad m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t [V.Image]
  -> VtyWidget t m ()
scrollingOutputX out = do
  dh <- displayHeight
  let scrollBy h (ix, n) =
        if | ix == 0 && n <= h -> Nothing -- Scrolled to the top and we don't have to scroll down
           | ix + h >= n -> Nothing -- Reached the end, no more scrolling
           | n > h && n - ix - h == 0 -> Just 1
           | otherwise -> Nothing
  rec scroll <- scrollable (tagMaybe (scrollBy <$> current dh <*> scroll) $ updated out) $ (current out)
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
              => MessageIn t -> Process t a ByteString
              -> m (DebugCollection t)
mkDebugOutput i p = do
  sout <- collectOutput never (BSU.fromString . show <$> _process_stdout p)
  iout <- collectOutput never (BSU.fromString . show <$> i)
  serr <- collectOutput never (_process_stderr p)
  return $ DebugCollection (current sout) (current iout) (current serr)

{-
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
renderDiags = unlines . intersperse "" . map renderDiag

renderDiag :: Diagnostic -> String
renderDiag d = renderRange (view range d) <> ":" <> T.unpack (view message d)

renderRange :: Range -> String
renderRange (Range s e) = renderPosition s ++ "-" ++ renderPosition e

renderPosition :: Position -> String
renderPosition (Position l c) = show l ++ ":" ++ show c
-}

diagnosticsPane :: (PostBuild t m, MonadNodeId m, MonadHold t m,
                       MonadFix m) => Session t -> VtyWidget t m ()
diagnosticsPane s = do
  dw <- displayWidth
  let ds = concatMap (\(k, v) -> map (k,) v) . M.toList <$> diagnostics s
  void $ col $ stretch $ scrollingOutputX $ (renderDiags <$> dw <*> ds)

{-
moduleString :: (Uri, [Diagnostic]) -> String
moduleString (uri, ds) =
  unlines [T.unpack (getUri uri), renderDiags ds]
  -}

renderDiags :: Int -> [(Uri, Diagnostic)] -> [V.Image]
renderDiags w = map (renderDiag w)

renderDiag :: Int -> (Uri, Diagnostic) -> V.Image
renderDiag w (u, d) =
  V.text V.defAttr (TL.fromStrict (getUri u))
  V.<->
  (V.text V.defAttr (renderRange (view range d))
  V.<|> V.text V.defAttr ":"
  V.<|> V.vertCat (wrap w (view message d)))

wrap :: Int -> T.Text -> [V.Image]
wrap maxWidth = concatMap (fmap (V.string V.defAttr . T.unpack) . TZ.wrapWithOffset maxWidth 0) . T.split (=='\n')

renderRange :: Range -> TL.Text
renderRange (Range s e) = renderPosition s <> "-" <> renderPosition e

renderPosition :: Position -> TL.Text
renderPosition (Position l c) = TL.pack (show l) <> ":" <> TL.pack (show c)

-- | Scrollable text widget. The output pair exposes the current scroll position and total number of lines (including those
-- that are hidden)
scrollable
  :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => Event t Int
  -- ^ Number of items to scroll by
  -> Behavior t ([V.Image])
  -> VtyWidget t m (Behavior t (Int, Int))
  -- ^ (Current scroll position, total number of lines)
scrollable scrollBy imgs = do
  dh <- displayHeight
--  let imgs = wrap <$> current dw <*> t
  kup <- key V.KUp
  kdown <- key V.KDown
  m <- mouseScroll
  let requestedScroll :: Event t Int
      requestedScroll = leftmost
        [ 1 <$ kdown
        , (-1) <$ kup
        , ffor m $ \case
            ScrollDirection_Up -> (-1)
            ScrollDirection_Down -> 1
        , scrollBy
        ]
      updateLine maxN delta ix = min (max 0 (ix + delta)) maxN
  lineIndex :: Dynamic t Int
    <- foldDyn (\(h, (maxN, delta)) ix -> updateLine (maxN - h) delta ix) 0 $
        attach (current dh) (attach (sum . map V.imageHeight <$> imgs) requestedScroll)
  tellImage $ (t <$> current lineIndex <*> (V.vertCat <$> imgs))
  return $ (,) <$> ((+ 1) <$> current lineIndex) <*> (sum . map V.imageHeight <$> imgs)
  where
--    addLen is = V.string V.defAttr (show (sum (map V.imageHeight is))) : is
    t ln i = V.translate 0 (negate ln) i

tellImage :: ImageWriter t m => Behavior t V.Image -> m ()
tellImage i = tellImages ((:[]) <$> i)
