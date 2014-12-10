module Main
  where

import Control.Monad (void)
import Data.Monoid
import qualified Data.Vector as V (Vector, ifoldl', fromList)
import qualified Data.Vector.Storable as VS (Storable, Vector, (!), convert, imap, map,
                                             unsafeSlice, foldr)
import System.Environment (getArgs)
import Vision.Image hiding (map)
import Vision.Primitive

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
    fn (Z :. y :. x, _) = if inBounds
        --then i `index` (Z :. y :. x')
        then subsectionAverage maxX vec y' x' dim
        else RGBPixel maxBound maxBound maxBound
      where
        x' = x - dim `div` 2
        y' = y - dim `div` 2
        -- vec =  V.fromList (replicate 9 0)
        dim = 20
        inBounds = x' >= 0 && y' >= 0 && x' < maxX - dim && y' < maxY - dim

-- |
-- >>> sumTable 3 (VS.fromList (replicate (RGBPixel 1 1 1) 5)) :: VS.Vector RGBPixel
sumTable :: Int -> VS.Vector RGBPixel -> V.Vector (Int, Int, Int)
sumTable width vec = V.fromList $ reverse $ V.ifoldl' helper [] (VS.convert vec)
  where
    helper :: [(Int, Int, Int)] -> Int -> RGBPixel -> [(Int, Int, Int)]
    helper m i px = let a = toTup px
                        b = toSum left
                        c = toSum above
                        d = toSum $ negateTup diag
                      in get (a <> b <> c <> d) : m
      where
        left  = if i `mod` width >= 1
            then head m
            else (0, 0, 0)

        above = if i >= width
            then m !! (width - 1)
            else (0, 0, 0)

        diag = if i >= width && i `mod` width >= 1
            then m !! width
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
subsectionAverage :: Int -> VS.Vector RGBPixel -> Int -> Int -> Int -> RGBPixel
subsectionAverage w v y x dim =
    listAverage $ map (average dim . lineSlice) [y..y+dim-1]
  where
    listAverage = divideRGB dim . foldr sumRGB (0, 0, 0)
    lineSlice y' = let start = x + y'*w in VS.unsafeSlice start dim v

-- |
-- Calculates the average pixel in a subsection
--
-- >>> average 9 (VS.fromList )
average :: Int -> VS.Vector RGBPixel -> RGBPixel
average len vec = divideRGB len (VS.foldr sumRGB (0, 0, 0) vec)

divideRGB :: Int -> (Int, Int, Int) -> RGBPixel
divideRGB len (r, g, b) = RGBPixel (fromIntegral (r `div` len))
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
