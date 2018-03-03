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

data Wheel = W { radius :: Double, frequency :: Double } 

drawWheel :: Wheel -> Diagram B
drawWheel (W r p) = circle r # lc blue

drawWheels :: V2 Double -> Double -> [Wheel] -> Diagram B
drawWheels p t (w@(W r f) : ws) =
    drawWheel w <>
    (fromVertices [p2 (0, 0), p2 (r*cos(t*f), r*sin(t*f))] # lc white) <>
    (drawWheels p t ws # translate (r2 (r*cos (t*f), r*sin (t*f))))
drawWheels _ _ [] = mempty

wheels :: Double -> Diagram B
wheels t =
    drawWheels (V2 0 0) t [
            W 1 0,
            W 0.5 1,
            W 0.7 2
        ] <> phantom (circle 3 :: Diagram B)

main = do
    let g = fft [0, 1, 0, 0]
    print g
    let f = ifft g
    print f

    animatedGif "test.gif" (mkSizeSpec2D (Just 512) (Just 512)) LoopingForever 25 $
        map wheels (map ((0.01*2*pi) *) [0..99])
