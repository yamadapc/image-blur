module Main
  where

import Control.Monad (void)
import Data.Monoid
import qualified Data.Vector.Storable as VS (Storable, Vector, concat,
                                             unsafeSlice, foldr,)
import Vision.Image hiding (map)
import Vision.Primitive

main :: IO ()
main = do
    ei <- load Nothing "input.png"
    case ei of
        Left _ -> error "Fuck"
        Right i -> let i' = myBlur (convert i)
          in void $ save "out.png" i'

myBlur :: RGB -> RGB
myBlur i = mapWithPoint fn i
  where
    (Z :. maxY :. maxX) = shape i
    vec = manifestVector i
    fn (Z :. y :. x, _) = if inBounds
        --then i `index` (Z :. y :. x')
        then average (dim * dim) $ subsection maxX vec y' x' dim
        else RGBPixel maxBound maxBound maxBound
      where
        x' = x - dim `div` 2
        y' = y - dim `div` 2
        -- vec =  V.fromList (replicate 9 0)
        dim = 20
        inBounds = x' >= 0 && y' >= 0 && x' < maxX - dim && y' < maxY - dim

-- $setup
-- >>> import qualified Data.Vector.Storable as VS (fromList)

-- |
-- Calculates a subsection of a vector, given a size and a position
--
-- >>> subsection 10 (VS.fromList [0..100]) 0 0 3 :: VS.Vector Int
-- fromList [0,1,2,10,11,12,20,21,22]
subsection :: VS.Storable a => Int -> VS.Vector a -> Int -> Int -> Int -> VS.Vector a
subsection w v y x dim = VS.concat $ map lineSlice [y..y+dim-1]
  where
    lineSlice y' = let start = x + y'*w in VS.unsafeSlice start dim v

mapWithPoint :: (Image i1, FromFunction i2)
             => ((Point, ImagePixel i1) -> FromFunctionPixel i2)
             -> i1
             -> i2
mapWithPoint fn img = fromFunction (shape img) $ \p -> fn (p, img `index` p)


-- |
-- Calculates the average pixel in a subsection
--
-- >>> average 9 (VS.fromList )
average :: Int -> VS.Vector RGBPixel -> RGBPixel
average len vec = divideRGB (VS.foldr helper (0, 0, 0) vec )
  where
    divideRGB :: (Int, Int, Int) -> RGBPixel
    divideRGB (r, g, b) = RGBPixel (fromIntegral (r `div` len))
                                   (fromIntegral (g `div` len))
                                   (fromIntegral (b `div` len))

    helper :: RGBPixel -> (Int, Int, Int) -> (Int, Int, Int)
    helper (RGBPixel r g b) (r', g', b') = ( r' + fromIntegral r
                                           , g' + fromIntegral g
                                           , b' + fromIntegral b)
