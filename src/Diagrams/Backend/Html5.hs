{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Html5
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A full-featured rendering backend for diagrams using Canvas.
-- Implemented using the blank-canvas platform.
--
-- To invoke the Html5 backend, you have three options.
--
-- * You can use the "Diagrams.Backend.Html5.CmdLine" module to create
--   standalone executables which will display the diagram in a browser
--   using a web service.
--
-- * You can use the 'renderHtml5' function provided by this module,
--   which gives you more programmatic control over when and
--   how images are displayed (making it east to, for example, write a
--   single program that displays multiple images, or one that diaplays
--   images dynamically based on user input, and so on).
--
-- * For the most flexiblity you can invoke the 'renderDia' method from
--   'Diagrams.Core.Types.Backend' instance for @Html5@. In particular,
--   'Diagrams.Core.Types.renderDia' has the generic type
--
-- > renderDia :: b -> Options b v -> QDiagram b v m -> Result b v
--
-- (omitting a few type class contraints). @b@ represents the
-- backend type, @v@ the vector space, and @m@ the type of monoidal
-- query annotations on the diagram. 'Options' and 'Result' are
-- associated data and type families, respectively, which yield the
-- type of option records and rendering results specific to any
-- particular backend. For @b ~ Html5@ and @v ~ R2@, we have
--
-- > data Options Html5 V2 Double = Html5Options
-- >  { _size :: SizeSpec V2 -- ^^ The requested size
-- >  }
--
-- @
-- data family Render Html5 V2 Double = C (RenderM ())
-- @
--
-- @
-- type family Result Html5 V2 Double = Html5 ()
-- @
--
-- So the type of 'renderDia' resolves to
--
-- @
-- renderDia :: Html5 -> Options Html5 V2 Double -> QDiagram Html5 V2 Double m ->
-- Html5()
-- @
--
-- which you could call like @renderDia Html5 (Html5Options (width 250))
-- myDiagram@
--
------------------------------------------------------------------------------

module Diagrams.Backend.Html5

  ( Html5(..) -- rendering token
  , B
  , Options(..) -- for rendering options specific to Html5

  , renderHtml5

  ) where

import           Control.Lens                 hiding (transform, (#))
import           Control.Monad.State          (when, State, evalState)
import qualified Control.Monad.StateStack     as SS
import           Control.Monad.Trans          (lift)

import           Data.Default.Class
import qualified Data.Foldable                as F
import           Data.Maybe                   (catMaybes, isJust, fromJust, fromMaybe)
import           Data.NumInstances            ()
import qualified Data.Text                    as T
import           Data.Tree                    (Tree(Node))
import           Data.Typeable                (Typeable)

import           Diagrams.Attributes
import           Diagrams.Prelude             hiding (fillTexture, moveTo, stroke, size)
import           Diagrams.TwoD.Adjust         (adjustDia2D)
import           Diagrams.TwoD.Attributes     (splitTextureFills)
import           Diagrams.TwoD.Path           (Clip (Clip))
import           Diagrams.TwoD.Text

import           Diagrams.Core.Compile
import           Diagrams.Core.Transform      (matrixHomRep)
import           Diagrams.Core.Types          (Annotation (..))

import qualified Graphics.Static               as  H

-- | This data declaration is simply used as a token to distinguish
--   this rendering engine.
data Html5 = Html5
    deriving (Eq, Ord, Read, Show, Typeable)

type B = Html5

type instance V Html5 = V2
type instance N Html5 = Double

data Html5State = Html5State { _accumStyle :: Style V2 Double
                               , _csPos :: (Double, Double) }

makeLenses ''Html5State

instance Default Html5State where
  def = Html5State { _accumStyle = mempty
                    , _csPos = (0,0) }

type RenderM a = SS.StateStackT Html5State H.CanvasFree a

liftC :: H.CanvasFree a -> RenderM a
liftC = lift

runRenderM :: RenderM a -> H.CanvasFree a
runRenderM = flip SS.evalStateStackT def

instance Monoid (Render Html5 V2 Double) where
  mempty  = C $ return ()
  (C c1) `mappend` (C c2) = C (c1 >> c2)

instance Backend Html5 V2 Double where
  data Render  Html5 V2 Double = C (RenderM ())
  type Result  Html5 V2 Double = H.CanvasFree ()
  data Options Html5 V2 Double = Html5Options
          { _html5Size   :: SizeSpec V2 Double   -- ^ the requested size
          }

  renderRTree :: Html5 -> Options Html5 V2 Double -> RTree Html5 V2 Double Annotation
                        -> Result Html5 V2 Double
  renderRTree _ _ rt = evalState canvasOutput initialHtml5RenderState
    where
      canvasOutput :: State Html5RenderState (H.CanvasFree ())
      canvasOutput = do
        let C r = toRender rt
        return $ runRenderM $ r

  adjustDia c opts d = adjustDia2D size c opts (d # reflectY)

runC :: Render Html5 V2 Double -> RenderM ()
runC (C r) = r

toRender :: RTree Html5 V2 Double Annotation -> Render Html5 V2 Double
toRender = fromRTree
  . Node (RStyle (mempty # recommendFillColor (transparent :: AlphaColour Double)))
  . (:[])
  . splitTextureFills
    where
      fromRTree (Node (RPrim p) _) = render Html5 p
      fromRTree (Node (RStyle sty) rs) = C $ do
        save
        canvasStyle sty
        accumStyle %= (<> sty)
        runC $ F.foldMap fromRTree rs
        restore
      fromRTree (Node _ rs) = F.foldMap fromRTree rs

data Html5RenderState = Html5RenderState

initialHtml5RenderState :: Html5RenderState
initialHtml5RenderState = Html5RenderState

getSize :: Options Html5 V2 Double -> SizeSpec V2 Double
getSize (Html5Options {_html5Size = s}) = s

setSize :: Options Html5 V2 Double -> (SizeSpec V2 Double) -> Options Html5 V2 Double
setSize o s = o {_html5Size = s}

size :: Lens' (Options Html5 V2 Double)(SizeSpec V2 Double)
size = lens getSize setSize

move :: Double -> Double -> RenderM ()
move x y = do csPos .= (x, y)

save :: RenderM ()
save = SS.save >> liftC H.save

restore :: RenderM ()
restore = liftC H.restore >> SS.restore

newPath :: RenderM ()
newPath = liftC $ H.beginPath

closePath :: RenderM ()
closePath = liftC $ H.closePath

moveTo :: Double -> Double -> RenderM ()
moveTo x y = do
  liftC $ H.moveTo x y
  move x y

relLineTo :: Double -> Double -> RenderM ()
relLineTo x y = do
  (p, q) <- use csPos
  let x' = p + x
      y' = q + y
  liftC $ H.lineTo x' y'
  move x' y'

relCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> RenderM ()
relCurveTo ax ay bx by cx cy = do
  p <- use csPos
  let [(ax',ay'),(bx',by'),(cx',cy')] = map (p +) [(ax,ay),(bx,by),(cx,cy)]
  liftC $ H.bezierCurveTo ax' ay' bx' by' cx' cy'
  move cx' cy'

-- | Get an accumulated style attribute from the render monad state.
getStyleAttrib :: AttributeClass a => (a -> b) -> RenderM (Maybe b)
getStyleAttrib f = (fmap f . getAttr) <$> use accumStyle

-- | From the HTML5 canvas specification regarding line width:
--
--     "On setting, zero, negative, infinite, and NaN values must be
--     ignored, leaving the value unchanged; other values must change
--     the current value to the new value.
--
--   Hence we must implement a line width of zero by simply not
--   sending a stroke command.
stroke :: RenderM ()
stroke = do
  -- The default value of 0.5 is somewhat arbitary since lineWidth should never
  -- be 'Nothing'. 0.5 is choose since it is the lower bound of the
  -- default.
  w <- fromMaybe 0.5 <$> getStyleAttrib getLineWidth
  when (w > (0 :: Double)) (liftC H.stroke)

fill :: RenderM ()
fill = liftC $ H.fill

clip :: RenderM ()
clip = liftC $ H.clip

byteRange :: Double -> Int
byteRange d = floor (d * 255)

texture :: (H.Style -> H.CanvasFree ()) -> Texture Double -> Double -> RenderM ()
texture styleFn (SC (SomeColor c))  o = liftC . styleFn $ s
  where s = H.ColorStyle $ colorJS c o

texture styleFn (LG g) _ = liftC $ do
  grd <- H.createLinearGradient x0 y0 x1 y1
  mapM_ (addStop grd) stops
  styleFn grd
  where
    (x0, y0) = unp2 $ transform (g^.lGradTrans) (g^.lGradStart)
    (x1, y1) = unp2 $ transform (g^.lGradTrans) (g^.lGradEnd)
    stops = map (\s -> ( s^.stopFraction , colorJS (s^.stopColor) 1)) (g^.lGradStops)

texture styleFn (RG g) _ = liftC $ do
  grd <- H.createRadialGradient x0 y0 r0 x1 y1 r1
  mapM_ (addStop grd) stops
  styleFn grd
  where
    (r0, r1) = (s * g^.rGradRadius0, s * g^.rGradRadius1)
    (x0, y0) = unp2 $ transform (g^.rGradTrans) (g^.rGradCenter0)
    (x1, y1) = unp2 $ transform (g^.rGradTrans) (g^.rGradCenter1)
    stops = map (\st -> ( st^.stopFraction , colorJS (st^.stopColor) 1)) (g^.rGradStops)
    s = avgScale $ g^.rGradTrans

addStop :: H.Style -> (Double, H.Color) -> H.CanvasFree ()
addStop g (f, c) = H.addColorStop f c g

colorJS :: (Color c) => c -> Double  -> H.Color
colorJS c o = H.RGBA (byteRange r) (byteRange g) (byteRange b) (o * realToFrac a)
  where 
    (r,g,b,a) = colorToSRGBA . toAlphaColour $  c

canvasTransform :: T2 Double -> RenderM ()
canvasTransform tr = liftC $ H.transform ax ay bx by tx ty
  where
    [[ax, ay], [bx, by], [tx, ty]] = (map . map) realToFrac (matrixHomRep tr)

strokeTexture :: Texture Double -> Double -> RenderM ()
strokeTexture = texture H.strokeStyle

fillTexture :: Texture Double -> Double -> RenderM ()
fillTexture = texture H.fillStyle

fromLineCap :: LineCap -> H.LineCapStyle
fromLineCap LineCapRound  = H.LineCapRound
fromLineCap LineCapSquare = H.LineCapSquare
fromLineCap _             = H.LineCapButt

fromLineJoin :: LineJoin -> H.LineJoinStyle
fromLineJoin LineJoinRound = H.LineJoinRound
fromLineJoin LineJoinBevel = H.LineJoinBevel
fromLineJoin _             = H.LineJoinMiter

showFontJS :: FontWeight -> FontSlant -> Double -> String -> T.Text
showFontJS wgt slant sz fnt = T.concat [a, " ", b, " ", c, " ", d]
  where
    a = case wgt of
          FontWeightNormal -> ""
          FontWeightBold   -> "bold"
    b = case slant of
          FontSlantNormal  -> ""
          FontSlantItalic  -> "italic"
          FontSlantOblique -> "oblique"
    c = T.concat [T.pack $ show sz, "pt"]
    d = T.pack fnt

renderC :: (Renderable a Html5, V a ~ V2, N a ~ Double) => a -> RenderM ()
renderC a = case (render Html5 a) of C r -> r

canvasStyle :: Style v Double  -> RenderM ()
canvasStyle s = sequence_
              . catMaybes $ [ handle clip'
                            , handle lWidth
                            , handle lCap
                            , handle lJoin
                            ]
  where handle :: (AttributeClass a) => (a -> RenderM ()) -> Maybe (RenderM ())
        handle f = f `fmap` getAttr s
        clip'    = mapM_ (\p -> canvasPath p >> clip) . op Clip
        lWidth   = liftC . H.lineWidth . getLineWidth
        lCap     = liftC . H.lineCap . fromLineCap . getLineCap
        lJoin    = liftC . H.lineJoin . fromLineJoin . getLineJoin

instance Renderable (Segment Closed V2 Double) Html5 where
  render _ (Linear (OffsetClosed (V2 x y))) = C $ relLineTo x y
  render _ (Cubic (V2 x1 y1)
                  (V2 x2 y2)
                  (OffsetClosed (V2 x3 y3)))
    = C $ relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail V2 Double) Html5 where
  render _ = withTrail renderLine renderLoop
    where
      renderLine ln = C $ do
        mapM_ renderC (lineSegments ln)
      renderLoop lp = C $ do
        case loopSegments lp of
          (segs, Linear _) -> mapM_ renderC segs
          _ -> mapM_ renderC (lineSegments . cutLoop $ lp)
        closePath

instance Renderable (Path V2 Double) Html5 where
  render _ p = C $ do
    canvasPath p
    f <- getStyleAttrib getFillTexture
    s <- getStyleAttrib getLineTexture
    o <- fromMaybe 1 <$> getStyleAttrib getOpacity
    save
    when (isJust f) (fillTexture (fromJust f) (realToFrac o) >> fill)
    strokeTexture (fromMaybe (SC (SomeColor (black :: Colour Double))) s) (realToFrac o)
    stroke
    restore

-- Add a path to the Html5 context, without stroking or filling it.
canvasPath :: Path V2 Double -> RenderM ()
canvasPath (Path trs) = do
    newPath
    F.mapM_ renderTrail trs
  where
    renderTrail (viewLoc -> (unp2 -> p, tr)) = do
      uncurry moveTo p
      renderC tr

instance Renderable (Text Double) Html5 where
  render _ (Text tr al str) = C $ do
    tf      <- fromMaybe "Calibri" <$> getStyleAttrib getFont
    sz      <- fromMaybe 12 <$> getStyleAttrib getFontSize
    slant   <- fromMaybe FontSlantNormal <$> getStyleAttrib getFontSlant
    fw      <- fromMaybe FontWeightNormal <$> getStyleAttrib getFontWeight
    tx      <- fromMaybe (SC (SomeColor (black :: Colour Double)))
               <$> getStyleAttrib getFillTexture
    o       <- fromMaybe 1 <$> getStyleAttrib getOpacity
    let fSize = avgScale tr * sz
        fnt = showFontJS fw slant fSize tf
        vAlign = case al of
                   BaselineText -> H.TextBaselineIdeographic
                   BoxAlignedText _ h -> case h of
                     h' | h' <= 0.25 -> H.TextBaselineBottom
                     h' | h' >= 0.75 -> H.TextBaselineTop
                     _ -> H.TextBaselineMiddle
        hAlign = case al of
                   BaselineText -> H.TextAlignStart
                   BoxAlignedText w _ -> case w of
                     w' | w' <= 0.25 -> H.TextAlignStart
                     w' | w' >= 0.75 -> H.TextAlignEnd
                     _ -> H.TextAlignCenter
    save
    liftC $ H.textBaseline vAlign
    liftC $ H.textAlign hAlign
    liftC $ H.font fnt
    fillTexture tx (realToFrac o)
    canvasTransform (tr <> reflectionY)
    liftC $ H.fillText (T.pack str) 0 0
    restore

instance Renderable (DImage Double External) Html5 where
  render _ (DImage path w h tr) = C $ do
    let ImageRef file = path
    save
    canvasTransform (tr <> reflectionY)
    img <- liftC $ H.newImage (T.pack file)
    liftC $ H.drawImageSize img (fromIntegral (-w) / 2) (fromIntegral (-h) / 2)
                                (fromIntegral w) (fromIntegral h)
    restore

renderHtml5 :: Int -> SizeSpec V2 Double -> QDiagram Html5 V2 Double Any -> IO ()
-- renderHtml5 port sizeSpec d = H.blankHtml5 (fromIntegral port) . flip H.send $ img
--     where
--       img = renderDia Html5 (Html5Options sizeSpec) d
renderHtml5 = undefined

renderHtml5 :: FilePath -> T.Text -> SizeSpec V2 Double -> QDiagram SVG V2 Double Any -> IO ()
renderHtml5 outFile prefix spec
  = BS.writeFile outFile
  . renderBS
  . renderDia Html5 (Html5Options spec [] prefix)
