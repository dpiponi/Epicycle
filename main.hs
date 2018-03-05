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
import Diagrams.Backend.Rasterific

onCArray f a = unsafePerformIO $ do
    let n = length a
    src <- createCArray (0, n-1) (flip pokeArray a)
    let dst = f src :: CArray Int (Complex Double)
    withCArray dst (peekArray n)

fft :: [Complex Double] -> [Complex Double]
fft = onCArray dft

ifft :: [Complex Double] -> [Complex Double]
ifft = onCArray idft

myCircle :: Double -> Diagram B
myCircle n = circle n # fc blue `atop` square 10

--main = mainWith myCircle

data Wheel = W { radius :: Double, frequency :: Double, phase :: Double } 

drawWheel :: Wheel -> Diagram B
drawWheel (W r f p) = circle r # lc blue

drawWheels :: V2 Double -> Double -> [Wheel] -> Diagram B
drawWheels p t (w@(W r f phase) : ws) =
    drawWheel w <>
    (fromVertices [p2 (0, 0), p2 (r*cos(t*f+phase), r*sin(t*f+phase))] # lc white) <>
    (drawWheels p t ws # translate (r2 (r*cos (t*f+phase), r*sin (t*f+phase))))
drawWheels _ _ [] = mempty

wheels :: Double -> Diagram B
wheels t =
    drawWheels (V2 0 0) t [
            W 1 0 (pi/4),
            W 0.5 1 (pi/2),
            W 0.7 2 (pi/6)
        ] <> phantom (circle 3 :: Diagram B)

drawFFT :: Complex Double -> Complex Double -> [Complex Double] -> Diagram B
drawFFT a b (c : cs) = 
    let z = a*c
        wheel = circle (magnitude z) # lc blue <>
                    fromVertices [p2 (0, 0), p2 (realPart z, imagPart z)]
    in wheel <> (drawFFT a (a*b) cs # translate (r2 (realPart z, imagPart z)))
drawFFT _ _ [] = mempty

main = do
    let g = fft [1, 1, 1, 1]
    print g
    let f = ifft g
    print f

    animatedGif "test.gif" (mkSizeSpec2D (Just 512) (Just 512)) LoopingForever 25 $
        [drawFFT 1 (cis (2*pi*t*0.01/4)) g | t <- [0.0, 0.01 .. 1.0]]
