{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Image
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Importing external images into diagrams.  Usage example: To create
-- a diagram from an embedded image with width 1 and height set
-- according to the aspect ratio, use @image img # scaleUToX 1@, where
-- @img@ is a value of type @DImage n e@, created with a function like
-- 'loadImageEmb', 'loadImageExt', or 'raster'.
-----------------------------------------------------------------------------

module Diagrams.TwoD.Image where