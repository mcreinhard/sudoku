module Euler (readDecimal, hasDuplicates, (<==>)) where

import Numeric
import Data.Char
import Data.List
import Control.Applicative

readBase :: (Integral a, Integral b) => a -> String -> b
readBase i = fromIntegral . fst . head . readInt i isHexDigit digitToInt

readDecimal :: (Integral a) => String -> a
readDecimal = readBase 10

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates = length </=> (length . Data.List.nub)

infix 4 <==>
(<==>) :: (Applicative f, Eq a) => f a -> f a -> f Bool
(<==>) = liftA2 (==)

infix 4 </=>
(</=>) :: (Applicative f, Eq a) => f a -> f a -> f Bool
(</=>) = liftA2 (/=)

