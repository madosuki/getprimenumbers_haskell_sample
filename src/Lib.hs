module Lib
    ( someFunc,
      getPrimeNumber
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getPrimeNumber :: Integer -> [Integer]
getPrimeNumber n = f [2..n] (round (sqrt $ fromInteger n)) []
  where f (p:xp) end result
          | p >= end = result ++ (p:xp)
          | otherwise =
            f [x | x <- xp, x `mod` p /= 0] end (result ++ [p])
