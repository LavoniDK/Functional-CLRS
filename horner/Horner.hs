module Horner (horner, hornerFold) where

horner :: Num a => [a] -> a -> a
horner [] _     = 0
horner coeffs x = horner' coeffs 0
  where
    horner' [] acc       = acc
    horner' coeffs acc   = horner' (init coeffs) (last coeffs + x * acc)

-- or more efficiently, reverse once, use tail recursion with pattern matching via foldl
hornerFold :: (Num a) => [a] -> a -> a
hornerFold coeffs x = foldl (\acc coeffs -> acc * x + coeffs) 0 (reverse coeffs)