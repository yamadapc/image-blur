{-# LANGUAGE FlexibleInstances #-}
module Main
  where

import Control.Monad (void)
import Data.Monoid
import qualified Data.Vector as V -- (Vector, (!), ifoldl', fromList)
import qualified Data.Vector.Storable as VS (Storable, Vector, (!), convert, imap, map,
                                             unsafeSlice, foldr)
import System.Environment (getArgs)
import Vision.Image hiding (map)
import Vision.Primitive

import Debug.Trace (trace)

main :: IO ()
main = do
    [inputFp, outputFp] <- getArgs
    ei <- load Nothing inputFp
    case ei of
        Left _ -> error "Fuck"
        Right i -> let i' = myBlur (convert i)
          in void $ save outputFp i'

myBlur :: RGB -> RGB
myBlur i = mapWithPoint fn i
  where
    (Z :. maxY :. maxX) = shape i
    vec = manifestVector i
    table = sumTable maxX vec
    fn (Z :. y :. x, _) = if inBounds
        --then i `index` (Z :. y :. x')
        then subsectionAverage table (dim * dim) maxX y' x' dim
        else RGBPixel maxBound maxBound maxBound
      where
        x' = x - dim `div` 2
        y' = y - dim `div` 2
        -- vec =  V.fromList (replicate 9 0)
        dim = 7
        inBounds = x' >= 0 && y' >= 0 && x' < maxX - dim && y' < maxY - dim

-- |
-- >>> sumTable 3 (VS.fromList (replicate (RGBPixel 1 1 1) 5)) :: VS.Vector RGBPixel
sumTable :: Int -> VS.Vector RGBPixel -> V.Vector (Int, Int, Int)
sumTable width vec = iconstructFoldN helper (VS.convert vec)
  where
    helper :: V.Vector (Int, Int, Int) -> Int -> RGBPixel -> (Int, Int, Int)
    helper m i px = let a = toTup px
                        b = toSum left
                        c = toSum above
                        d = toSum $ negateTup diag
                      in get (a <> b <> c <> d)
      where
        left = if i `mod` width >= 1
            then m V.! (i - 1)
            else (0, 0, 0)

        above = if i >= width
            then m V.! (i - width)
            else (0, 0, 0)

        diag = if i >= width && i `mod` width >= 1
            then m V.! (i - width - 1)
            else (0, 0, 0)

        get (x, y, z) = (getSum x, getSum y, getSum z)
        toSum (x, y, z) = (Sum x, Sum y, Sum z)
        toTup (RGBPixel r g b) = (fromIntegral r, fromIntegral g, fromIntegral b)
        negateTup (x, y, z) = (-x, -y, -z)

-- $setup
-- >>> import qualified Data.Vector.Storable as VS (fromList)

-- |
-- Calculates a subsection of a vector, given a size and a position
--
-- >>> subsection 10 (VS.fromList [0..100]) 0 0 3 :: VS.Vector Int
-- fromList [0,1,2,10,11,12,20,21,22]
subsectionAverage :: V.Vector (Int, Int, Int) -> Int -> Int
                  -> Int -> Int -> Int -> RGBPixel
subsectionAverage table len w y x dim = (c - d - b + a) `rgbDiv` len
  where
    a = table V.! (x + y * w)
    b = table V.! (x + (y + dim) * w)
    c = table V.! (x + dim + (y + dim) * w)
    d = table V.! (x + dim + y * w)

rgbDiv :: (Int, Int, Int) -> Int -> RGBPixel
rgbDiv (r, g, b) len = RGBPixel (fromIntegral (r `div` len))
                                (fromIntegral (g `div` len))
                                (fromIntegral (b `div` len))

sumRGB :: RGBPixel -> (Int, Int, Int) -> (Int, Int, Int)
sumRGB (RGBPixel r g b) (r', g', b') = (r' + fromIntegral r,
                                        g' + fromIntegral g,
                                        b' + fromIntegral b)

mapWithPoint :: (Image i1, FromFunction i2)
             => ((Point, ImagePixel i1) -> FromFunctionPixel i2)
             -> i1
             -> i2
mapWithPoint fn img = fromFunction (shape img) $ \p -> fn (p, img `index` p)

-- Data.Vector iconstructFoldrN
-------------------------------------------------------------------------------
iconstructFoldN :: (V.Vector a -> Int -> b -> a) -> V.Vector b -> V.Vector a
iconstructFoldN fn vec = V.constructN len helper
  where
    len = V.length vec
    helper v = let i = V.length v in fn v i (vec V.! i) 

-- RGB Int Tuple
-------------------------------------------------------------------------------

toPix :: (Int, Int, Int) -> RGBPixel
toPix (r, g, b) = RGBPixel (fromIntegral r) (fromIntegral g) (fromIntegral b)

instance Num (Int, Int, Int) where
    (+) (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
    (*) (x1, y1, z1) (x2, y2, z2) = (x1 * x2, y1 * y2, z1 * z2)
    (-) (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
    abs (x, y, z) = (abs x, abs y, abs z)
    fromInteger x = (fromInteger x, fromInteger x, fromInteger x)
