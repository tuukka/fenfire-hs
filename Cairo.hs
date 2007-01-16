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
import Utils (Endo)

import Control.Monad

import Data.Monoid

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

class (Monad cx, Monoid r) => MonadCx cx r | cx -> r, r -> cx where
    cxAsk     :: cx Rect

    cxWrap    :: EndoM cx (Render ()) -> Endo r
    cxLocal   :: cx Rect -> Endo r

    cxRender :: cx (Render ()) -> r
    cxRender r = cxWrap (const r) mempty

instance Monoid Path where
    mempty      = Path $ return ()
    mappend p q = Path $ renderPath p >> renderPath q
    
cxMatrix :: MonadCx cx r => cx Matrix
cxMatrix = liftM fst cxAsk

cxSize :: MonadCx cx r => cx Size
cxSize = liftM snd cxAsk


[black, gray, lightGray, white] = [Color x x x 1 | x <- [0, 0.5, 0.9, 1]]


transformPoint :: Matrix -> Endo Point     -- the gtk2hs impl is buggy
transformPoint (Matrix xx xy yx yy x0 y0) (dx,dy) =
    (xx*dx + yx*dy + x0, xy*dx + yy*dy + y0)
    

fill :: MonadCx cx r => cx Path -> r
fill p = cxRender $ do p' <- p; return $ do renderPath p'; C.fill

stroke :: MonadCx cx r => cx Path -> r
stroke p = cxRender $ do p' <- p; return $ do renderPath p'; C.stroke

paint :: MonadCx cx r => r
paint = cxRender $ return C.paint

clip :: MonadCx cx r => cx Path -> Endo r
clip p = cxWrap $ \ren -> p >>= \p' -> return $ do
    C.save; renderPath p'; C.clip; ren; C.restore
    
    
withColor :: MonadCx cx r => cx Color -> Endo r
withColor c = cxWrap $ \ren -> c >>= \(Color r g b a) -> return $ do
    C.save; C.setSourceRGBA r g b a; ren; C.restore
    
-- | Applies a matrix transformation to a renderable.
--
transform :: MonadCx cx r => cx (Endo Matrix) -> Endo r
transform f = cxLocal $ do (m,s) <- cxAsk; f' <- f
                           return (f' Matrix.identity * m, s)

-- | Moves a renderable by x and y.
--
translate :: MonadCx cx r => cx (Double, Double) -> Endo r
translate = transform . liftM (\(x,y) -> Matrix.translate x y)

-- | Moves a renderable to the specific point p.
--
translateTo :: MonadCx cx r => cx Point -> Endo r
translateTo p = translate $ do p' <- p; m <- cxMatrix
                               return $ transformPoint (Matrix.invert m) p'

-- | Rotates a renderable by angle.
--
rotate :: MonadCx cx r => cx Double -> Endo r
rotate angle = transform $ liftM Matrix.rotate angle

-- | Scales a renderable by sx and sy.
--
scale2 :: MonadCx cx r => cx (Double, Double) -> Endo r
scale2 = transform . liftM (\(sx,sy) -> Matrix.scale sx sy)
    
scale :: MonadCx cx r => cx Double -> Endo r
scale = scale2 . liftM (\s -> (s,s))


between :: MonadCx cx r => cx Point -> cx Point -> Endo r
between p1 p2 = translate p . rotate a where
    p = do (x1,y1) <- p1; (x2,y2) <- p2; return ((x1+x2)/2, (y1+y2)/2)
    a = do (x1,y1) <- p1; (x2,y2) <- p2; return $ atan2 (y2-y1) (x2-x1)

                          
point :: MonadCx cx r => Double -> Double -> cx Point
point x y = do m <- cxMatrix; return $ transformPoint m (x,y)

anchor :: MonadCx cx r => Double -> Double -> cx Point
anchor x y = do (w,h) <- cxSize; point (x*w) (y*h)

center :: MonadCx cx r => cx Point
center = anchor 0.5 0.5

closePath :: MonadCx cx r => cx Path
closePath = return $ Path $ C.closePath

arc :: MonadCx cx r => cx Point -> Double -> Double -> Double -> cx Path
arc p a b c = do (x,y) <- p; return $ Path $ C.arc x y a b c

arcNegative :: MonadCx cx r => cx Point -> Double -> Double -> Double -> 
               cx Path
arcNegative p a b c = 
    do (x,y) <- p; return $ Path $ C.arcNegative x y a b c

curveTo :: MonadCx cx r => cx Point -> cx Point -> cx Point -> cx Path
curveTo p1 p2 p3 = do (x1,y1) <- p1; (x2,y2) <- p2; (x3,y3) <- p3
                      return $ Path $ C.curveTo x1 y1 x2 y2 x3 y3

moveTo :: MonadCx cx r => cx Point -> cx Path
moveTo p = do (x,y) <- p; return $ Path $ do C.moveTo x y

lineTo :: MonadCx cx r => cx Point -> cx Path
lineTo p = do (x,y) <- p; return $ Path $ do C.lineTo x y

line :: MonadCx cx r => cx Point -> cx Point -> cx Path
line p1 p2 = moveTo p1 ^&^ lineTo p2

extents :: MonadCx cx r => cx Path
extents = moveTo (anchor 0 0) ^&^ lineTo (anchor 0 1) ^&^ lineTo (anchor 1 1)
      ^&^ lineTo (anchor 1 0) ^&^ lineTo (anchor 0 0)