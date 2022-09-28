type XYpoint = (Double, Double)
type XYPoints = [XYpoint]
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

