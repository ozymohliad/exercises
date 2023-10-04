import Utils (factors)

import qualified Data.Map as M

divisibleByAll :: [Integer] -> Integer
divisibleByAll = product . map (uncurry (^)) . M.toList . foldr (M.unionWith max . countFactors) M.empty
    where countFactors = foldr ((flip $ M.insertWith (+)) 1) M.empty . factors
