{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecursiveDo #-}


module Main where

import           Control.Monad.Fix       (MonadFix)
import qualified Data.Text               as T
import           Diagrams.Backend.Reflex
import           Diagrams.Prelude        hiding (Time, el, text)
import           Reflex
import           Reflex.Dom
import           ValueSVG
import           Control.Monad.Trans (liftIO)
import           Data.Time
import           Control.DeepSeq
import           Util
import           Prelude hiding (log)

main :: IO ()
main =
  mainWidgetWithHead headWidget bodyElement

headWidget :: DomBuilder t m => m ()
headWidget = do
  elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
  el "title" $ text "Animazing"

bodyElement :: MonadWidget t m => m ()
bodyElement = tsvg

stoneButton :: DomBuilder t m => m (Event t ())
stoneButton = do
  let attr = ("style" =: "font-size: 200%;")
  clickEvent $ elAttr' "button" attr stone

stone :: DomBuilder t m => m ()
stone =
  text "🗿"

-- | Get the click event on an element
--
-- Use as:
--   clickEvent $ el' "a" ...
clickEvent ::
  ( DomBuilder t m,
    HasDomEvent t target 'ClickTag
  ) =>
  m (target, a) ->
  m (Event t ())
clickEvent w =
  fmap (fmap (const ()) . domEvent Click . fst) w


tsvg :: MonadWidget t m => m ()
tsvg = do 
  rec
    imgs <- mapM (\ (i, f) -> elDynAttr "div" ((\ a -> showImg i a) <$> dynFrameState) $ f) (zip [0 .. 50] gif)
    now <- liftIO getCurrentTime
    tick <- tickLossy (1 / 50) now
    dynFrameState <- foldDyn (player) 0 (1 <$ tick)
  return ()

showImg b fi = if b == fi then ("style" =: ("display: block")) else ("style" =: ("display: none")) 

player :: Int -> Int -> Int
player i c = if (log c) > 50 then 0 else c + 1


counter :: Int -> T.Text
counter i = T.concat ["The circle has been clicked ", T.pack (show i), " times" ]
