{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Math.FFT
import Data.Array.CArray.Base
import Foreign.Marshal.Array
import Foreign.Storable.Complex
import Data.Complex
import System.IO.Unsafe
import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
--import Diagrams.Backend.Rasterific.CmdLine
--import Diagrams.Backend.Rasterific
import Diagrams.Backend.Postscript
import Control.Monad
import Text.Printf

import Data.Complex

intersect' r0 r1 d = 
    let t = d*d-r1*r1+r0*r0
        x = t/(2*d)
        a = sqrt (4*d*d*r0*r0-t*t)/d
    in ((x, -a/2), (x, a/2))

intersect'' r0 r1 = 
    let t = 1-r1*r1+r0*r0
        x = 0.5*t
        a = sqrt (4*r0*r0-t*t)
    in ((x, -0.5*a), (x, 0.5*a))

intersect x0 y0 r0 x1 y1 r1 =
    let z0 = x0 :+ y0
        z1 = x1 :+ y1
        d = 1/magnitude (z1-z0)
        ((u0, v0), (u1, v1)) = intersect'' (d*r0) (d*r1)
    in ((u0 :+ v0)*(z1-z0)+z0, (u1 :+ v1)*(z1-z0)+z0)

angles w0@(x0 :+ y0) r0 w1@(x1 :+ y1) r1 =
    let (z0, z1) = intersect x0 x0 r0 x1 y1 r1
        theta0 = phase (z0-w0)
        theta1 = phase (z1-w0)
        phi0 = phase (z0-w1)
        phi1 = phase (z1-w1)
    in (theta0, theta1, phi0, phi1)

ccw a1 a2 a3 =
    a1 < a2 && (a2 < a3 || a3 < a1) || a2 < a3 && a3 < a1

{-
main = do
     let (u, v) = intersect (-1) 1 5 1 1 6
     print $ magnitude (u-((-1) :+ 1))
     print $ magnitude (v-(1 :+ 1))
 -}

onCArray f a = unsafePerformIO $ do
    let n = length a
    src <- createCArray (0, n-1) (flip pokeArray a)
    let dst = f src :: CArray Int (Complex Double)
    withCArray dst (peekArray n)

fft :: [Complex Double] -> [Complex Double]
fft = onCArray dft

ifft :: [Complex Double] -> [Complex Double]
ifft = onCArray idft

--myCircle :: Double -> Diagram B
--myCircle n = circle n # fc blue `atop` square 10

myarc :: Double -> Double -> Double -> Double -> Double -> [P2 Double]
myarc tx ty r a0 a1 =
    let a1' = if a1 < a0 then a1+2*pi else a1
        n = ceiling ((a1'-a0)/(2*pi)*64) :: Int
    in [p2 (tx+x, ty+y) | t <- [0 .. fromIntegral n],
                                     let t' = t/fromIntegral n,
                                     let theta = a0+t'*(a1-a0),
                                     let x = r*cos theta,
                                     let y = r*sin theta]

-- a is ω ⁿ
drawFFT :: Double -> Double -> Complex Double -> Complex Double -> [Complex Double] -> Diagram B
drawFFT tx ty a ω (c : cs) = 
    let z = c*a
        r = magnitude z
        x :+ y = z
        circle' tx ty r a0 a1 = fromVertices (myarc tx ty r a0 a1) # lwG 0.01
        wheel = if r > 1e-6
                        then circle' tx ty r 0 (2*pi) # lc white <>
                            (fromVertices [p2 (tx, ty), p2 (tx+x, ty+y)] # lc blue)
                        else mempty
    in wheel <> drawFFT (tx+x) (ty+y) (a*ω) ω cs
drawFFT _ _ _ _ [] = mempty

drawSignal :: Double -> [Complex Double] -> Diagram B
drawSignal t cs = 
    let n = length cs
        m = ceiling (t*fromIntegral n)
    in fromVertices (map (\(x :+ y) -> p2 (x, y)) (take m cs)) # lc green

main = do
    let i = 0:+1
    let signal = [realToFrac t :+ realToFrac t | t' <- [0 .. 255], let t = t'/256]
    --let signal = [cis (8*pi*t+0.5)+cis(16*pi*t) | t <- [0.00, 0.01 .. 1.00]]
    let n = length signal
    let fourierTransform = map (/ fromIntegral n) $ fft signal
    --print fourierTransform
    --let f = ifft fourierTransform
    --print f

    print fourierTransform
    let ω = cis (2*pi/8)
    print $ sum [ fourierTransform!!i * ω ^i| i <- [0 .. n-1]]
    forM_ [0 .. 127] $ \t' ->
        let t = t'/128
            filename = printf "data/test.%04d.eps" (floor t' :: Int)
        in renderDia Postscript (PostscriptOptions filename (mkWidth 400) EPS) $
                drawSignal t signal <>
                drawFFT 0 0 1 (cis (2*pi*t)) fourierTransform <>
                phantom (circle 2 :: Diagram B)
    {-
    animatedGif "test.gif" (mkSizeSpec2D (Just 512) (Just 512)) LoopingForever 25 $
        [
            drawSignal t signal <>
            drawFFT 1 (cis (2*pi*t)) fourierTransform <>
            phantom (circle 2 :: Diagram B)
            | t' <- [0 .. 127], let t = t'/128]
            -}
