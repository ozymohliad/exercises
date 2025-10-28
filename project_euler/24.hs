import Data.List (uncons)

extract i xs = let (left, right) = splitAt i xs in fmap (left ++) <$> uncons right
