module Main
  where

import Control.Monad (void)
import Data.Monoid
import qualified Data.Vector.Storable as VS (foldr)
import Vision.Image
import Vision.Primitive

main :: IO ()
main = do
    ei <- load Nothing "input.png"
    case ei of
        Left _ -> error "Fuck"
        Right i -> let i' = myBlur (convert i)
          in void $ save "out.png" i'

-- For the record, `minBound` is from the `Prelude.Bounded` type-class.

myBlur :: RGB -> RGB
myBlur i = mapWithPoint fn i
  where
    maxX = let (Z :. _ :. x) = shape i
             in x
    fn (Z :. y :. x, _) = if x' < maxX
        then i `index` (Z :. y :. x')
        else RGBPixel maxBound maxBound maxBound
      where
        x' = x + 10

mapWithPoint :: (Image i1, FromFunction i2)
             => ((Point, ImagePixel i1) -> FromFunctionPixel i2)
             -> i1
             -> i2
mapWithPoint fn img = fromFunction (shape img) $ \p -> fn (p, img `index` p)

average :: RGB -> RGBPixel
average i = VS.foldr (<>) mempty vec `divideRGB` fromIntegral len
  where
    vec = manifestVector i
    len = let (Z :. y :. x) = shape i in y * x

    divideRGB (RGBPixel r g b) l = RGBPixel (r `div` l) (g `div` l) (b `div` l)

instance Monoid RGBPixel
  where
    mappend (RGBPixel r1 g1 b1) (RGBPixel r2 g2 b2) =
        RGBPixel (r1 + r2) (g1 + g2) (b1 + b2)
    mempty = RGBPixel 0 0 0
