module Main
  where

import Control.Monad (void)
import Vision.Image
import Vision.Primitive

main :: IO ()
main = void $ save "out.png" i

sz :: Size
--   Z :. height :. width
sz = Z :. 1000   :. 500

fn :: Point -> GreyPixel
--  Z :. Y :. X
fn (Z :. _ :. x) | x < 100 = GreyPixel maxBound
fn _ = GreyPixel minBound
-- For the record, `minBound` is from the `Prelude.Bounded` type-class.

i :: Grey
i = fromFunction sz fn
