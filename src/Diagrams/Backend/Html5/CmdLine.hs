{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE CPP                  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Html5.CmdLine
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the Html5 backend.
--
--
-- * 'defaultMain' creates an executable which can render a single
--   diagram at various options.
--
-- * 'multiMain' is like 'defaultMain' but allows for a list of
--   diagrams from which the user can choose one to render.
--
-- * 'mainWith' is a generic form that does all of the above but with
--   a slightly scarier type.  See "Diagrams.Backend.CmdLine".  This
--   form can also take a function type that has a suitable final result
--   (any of arguments to the above types) and 'Parseable' arguments.
--
-- If you want to generate diagrams programmatically---/i.e./ if you
-- want to do anything more complex than what the below functions
-- provide---you have several options.
--
-- * Use a function with 'mainWith'.  This may require making
--   'Parseable' instances for custom argument types.
--
-- * Make a new 'Mainable' instance.  This may require a newtype
--   wrapper on your diagram type to avoid the existing instances.
--   This gives you more control over argument parsing, intervening
--   steps, and diagram creation.
--
-- * Build option records and pass them along with a diagram to 'mainRender'
--   from "Diagrams.Backend.CmdLine".
--
-- For a tutorial on command-line diagram creation see
-- <http://projects.haskell.org/diagrams/doc/cmdline.html>.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Html5.CmdLine
       ( 
        -- * General form of @main@
        --  $mainWith
        mainWith

        -- * Supported froms of @main@
       , defaultMain
       , multiMain
       , Html5
       , B
       ) where

import Diagrams.Prelude                hiding (width, height, option, (<>), value)
import Diagrams.Backend.CmdLine        hiding (width, height)
import Diagrams.Backend.Html5
import qualified Graphics.Static        as H

import Data.Data
import Control.Lens                    (makeLenses, (^.))
import Options.Applicative

data DiaOpts = DiaOpts 
  { _width  :: Maybe Int -- ^ Final output width of diagram.
  , _height :: Maybe Int -- ^ Final height of diagram.
  , _port   :: Int       -- ^ Port on which to start web server.
  } deriving (Show, Data, Typeable)

makeLenses ''DiaOpts

diaOpts :: Parser DiaOpts
diaOpts = DiaOpts
  <$> (optional . option auto)
      (long "width" <> short 'w'
    <> metavar "WIDTH"
    <> help "Desired WIDTH of the output image")
  <*> (optional . option auto)
      (long "height" <> short 'h'
    <> metavar "HEIGHT"
    <> help "Desired HEIGHT of the output image")
  <*> option auto
      (long "port" <> short 'p' 
    <> value 3000
    <> metavar "PORT"
    <> help "Port on which to satrt the web server (default 3000)")

instance Parseable DiaOpts where
  parser = diaOpts
  
defaultMain :: QDiagram Html5 V2 Double Any -> IO ()
defaultMain = mainWith
    
instance Mainable (QDiagram Html5 V2 Double Any) where
  type MainOpts (QDiagram Html5 V2 Double Any) = DiaOpts
  mainRender = html5Render

html5Render :: DiaOpts -> QDiagram Html5 V2 Doubl3 Any -> IO ()
html5Render opts d =
  case splitOn "." (opts^.output) of
    [""] -> putStrLn "No output file given."
    ps | last ps `elem` ["html"] -> do
           let szSpec = fromIntegral <$> mkSizeSpec2D (opts^.width) (opts^.height)
               build  = renderDia Html5 (Html5Options szSpec [] "") d
           BS.writeFile (opts^.output) (renderBS build)
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps

multiMain :: [(String, QDiagram Html5 V2 Double Any)] -> IO ()
multiMain = mainWith

instance Mainable [(String, QDiagram Html5 V2 Double Any)] where
  type MainOpts [(String, QDiagram Html5 V2 Double Any)] = 
    (MainOpts (QDiagram Html5 V2 Double Any), DiagramMultiOpts)

  mainRender = defaultMultiMainRender
