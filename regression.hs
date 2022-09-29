type XYpoint = (Double, Double)
type XYPoints = [XYpoint]
type RowVector = [Double]
type ColumnVector = [Double]
type Matrix = [RowVector]
type Polynomial = (Double -> Double)

-- instance Show Polynomial where

multiplyAppend :: Num a => a -> a -> [a] -> [a]
multiplyAppend x y z = x*y:z

listTimesDouble :: (Foldable t, Num a) => t a -> a -> [a]
listTimesDouble list factor = foldr f [] list
                    where f = multiplyAppend factor

polyFromList :: [Double] -> Polynomial
polyFromList [] = error "Empty list passed: no coefficients to define polynomial."
polyFromList [a] = const a
polyFromList (a:as) = \x -> a + polyFromList (listTimesDouble as x) x

vecInnerProduct :: RowVector -> ColumnVector -> Double
vecInnerProduct [] [] = error "Both arguments empty."
vecInnerProduct [] _ = error "Empty row vector passed."
vecInnerProduct _ [] = error "Empty column vector passed."
vecInnerProduct [r] [c] = r * c
vecInnerProduct (r:rs) (c:cs) = r * c + vecInnerProduct rs cs

nthCol :: Matrix -> Int -> ColumnVector


listOfPows :: Double -> Int -> RowVector
listOfPows seed 1 = [1]
listOfPows seed n = listOfPows seed (n-1) ++ [seed^(n-1)]