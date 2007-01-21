-- For (instance (Cairo cx r, Monoid m) => Monoid (cx m)):
{-# OPTIONS_GHC -fallow-undecidable-instances -fallow-incoherent-instances #-}
-- More, implied by the previous on GHC 6.6 but needed for earlier:
{-# OPTIONS_GHC -fallow-overlapping-instances #-}
module Cairo where

-- Copyright (c) 2006-2007, Benja Fallenstein, Tuukka Hastrup
-- This file is part of Fenfire.
-- 
-- Fenfire is free software; you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- Fenfire is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
-- Public License for more details.
-- 
-- You should have received a copy of the GNU General
-- Public License along with Fenfire; if not, write to the Free
-- Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
-- MA  02111-1307  USA

import Utils

import Control.Monad

import Data.Monoid (Monoid(mappend, mempty))

import Graphics.UI.Gtk hiding (Point, Size, Layout, Color, get, fill)
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo.Matrix (Matrix(Matrix))
import qualified Graphics.Rendering.Cairo.Matrix as Matrix
import Graphics.UI.Gtk.Cairo

data Color = Color Double Double Double Double
type Size  = (Double, Double)
type Point = (Double, Double)
type Rect  = (Matrix, Size)

type Render a = C.Render a
newtype Path  = Path { renderPath :: Render () }

class (Monoidal cx, Monoid r) => 
      Cairo cx r | cx -> r, r -> cx where
    cxAsk     :: cx Rect
    cxWrap    :: EndoM cx (Render ()) -> Endo r
    
    transform :: cx (Endo Matrix) -> Endo r

    cxRender :: cx (Render ()) -> r
    cxRender r = cxWrap (const r) mempty
    
instance Monoid Path where
    mempty = Path $ return ()
    mappend (Path p) (Path q) = Path (p >> q)

instance (Monoidal m, Monoid o) => Monoid (m o) where
    mempty = pure mempty
    mappend = fmap2 mappend

cxMatrix :: Cairo cx r => cx Matrix
cxMatrix = fmap fst cxAsk

cxSize :: Cairo cx r => cx Size
cxSize = fmap snd cxAsk


[black, gray, lightGray, white] = [Color x x x 1 | x <- [0, 0.5, 0.9, 1]]


transformPoint :: Matrix -> Endo Point     -- the gtk2hs impl is buggy
transformPoint (Matrix xx xy yx yy x0 y0) (dx,dy) =
    (xx*dx + yx*dy + x0, xy*dx + yy*dy + y0)
    

fill :: Cairo cx r => cx Path -> r
fill p = cxRender $ ffor p $ \p' -> do renderPath p'; C.fill

stroke :: Cairo cx r => cx Path -> r
stroke p = cxRender $ ffor p $ \p' -> do renderPath p'; C.stroke

paint :: Cairo cx r => r
paint = cxRender $ pure C.paint

clip :: Cairo cx r => cx Path -> Endo r
clip p = cxWrap $ \ren -> ffor p $ \p' -> do
    C.save; renderPath p'; C.clip; ren; C.restore
    
    
withColor :: Cairo cx r => cx Color -> Endo r
withColor c = cxWrap $ \ren -> ffor c $ \(Color r g b a) -> do
    C.save; C.setSourceRGBA r g b a; ren; C.restore
    
-- | Moves a renderable by x and y.
--
translate :: Cairo cx r => cx Double -> cx Double -> Endo r
translate x y = transform $ fmap2 Matrix.translate x y

-- | Moves a renderable to the specific point p.
--
translateTo :: Cairo cx r => cx Point -> Endo r
translateTo p = transform $ ffor p $ \(x0,y0) ->
    \(Matrix xx xy yx yy _ _) -> Matrix xx xy yx yy x0 y0

-- | Rotates a renderable by angle.
--
rotate :: Cairo cx r => cx Double -> Endo r
rotate angle = transform $ fmap Matrix.rotate angle

-- | Scales a renderable by sx and sy.
--
scale2 :: Cairo cx r => cx Double -> cx Double -> Endo r
scale2 sx sy = transform $ fmap2 Matrix.scale sx sy
    
-- | Scales a renderable by sc.
--
scale :: Cairo cx r => cx Double -> Endo r
scale sc = scale2 sc sc


between :: Cairo cx r => cx Point -> cx Point -> Endo r
between p1 p2 = translate x y . rotate angle where
    (x,y) = funzip $ ffor2 p1 p2 $ \(x1,y1) (x2,y2) -> ((x1+x2)/2, (y1+y2)/2)
    angle = ffor2 p1 p2 $ \(x1,y1) (x2,y2) -> atan2 (y2-y1) (x2-x1)


point :: Cairo cx r => Double -> Double -> cx Point
point x y = ffor cxMatrix $ \m -> transformPoint m (x,y)

anchor :: Cairo cx r => Double -> Double -> cx Point
anchor x y = ffor cxAsk $ \(m, (w,h)) -> transformPoint m (x*w, y*h)

center :: Cairo cx r => cx Point
center = anchor 0.5 0.5

closePath :: Cairo cx r => cx Path
closePath = pure $ Path $ C.closePath

arc :: Cairo cx r => cx Point -> Double -> Double -> Double -> cx Path
arc p a b c = ffor p $ \(x,y) -> Path $ C.arc x y a b c

arcNegative :: Cairo cx r => cx Point -> Double -> Double -> Double -> 
               cx Path
arcNegative p a b c = ffor p $ \(x,y) -> Path $ C.arcNegative x y a b c

curveTo :: Cairo cx r => cx Point -> cx Point -> cx Point -> cx Path
curveTo p1 p2 p3 = ffor3 p1 p2 p3 $ \(x1,y1) (x2,y2) (x3,y3) ->
                       Path $ C.curveTo x1 y1 x2 y2 x3 y3

moveTo :: Cairo cx r => cx Point -> cx Path
moveTo p = ffor p $ \(x,y) -> Path $ do C.moveTo x y

lineTo :: Cairo cx r => cx Point -> cx Path
lineTo p = ffor p $ \(x,y) -> Path $ do C.lineTo x y

line :: (Cairo cx r, Monoid (cx Path)) => cx Point -> cx Point -> cx Path
line p1 p2 = moveTo p1 & lineTo p2

extents :: (Cairo cx r, Monoid (cx Path)) => cx Path
extents = moveTo (anchor 0 0) & lineTo (anchor 0 1) & lineTo (anchor 1 1)
        & lineTo (anchor 1 0) & lineTo (anchor 0 0)
