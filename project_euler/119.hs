import Utils (mergeSorted)
import Data.List (nub, group)
import Math.NumberTheory.Factor

powers :: Integral a => [a]
powers = mergeSorted [iterate (*b) (b^2) | b <- [2..]]

isPower :: Integral a => a -> Bool
isPower 1 = True
isPower x = (>1) $ foldr1 gcd $ map length $ group $ pfactors $ fromIntegral x
