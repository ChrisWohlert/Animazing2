{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Signal where

import           Diagrams.Prelude    hiding (Line, duration, lineWidth, p3)
import           Diagrams.TwoD.Types
import           Prelude             hiding (log)
import           Util

type Signal = Double -> Double

instance Show Signal where
    show s = "Signal " ++ show (s 0.5)


linearSignal = id


cubicBezierS' :: (Double, Double, Double, Double) -> Signal
cubicBezierS' (x1, x2, x3, x4) s =
  let ms = 1-s
  in x1*ms^(3::Int) + 3*x2*ms^(2::Int)*s + 3*x3*ms*s^(2::Int) + x4*s^(3::Int)
