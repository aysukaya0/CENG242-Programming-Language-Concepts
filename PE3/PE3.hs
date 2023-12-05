{-# LANGUAGE FlexibleInstances #-}

module PE3 where

import Data.List (sort, sortBy)
import Text.Printf (printf)

data Term = Const Integer | Pw Integer Power | Trig Integer Power Trigonometric | Exp Integer Power Exponential

data Power = Power Integer
data Polynomial = Polynomial [(Integer, Power)]
data Exponential = Exponential Polynomial
data Trigonometric = Sin Polynomial | Cos Polynomial

class Evaluable a where
    function :: a -> (Integer -> Double)

class Differentiable a where
    derivative :: a -> [Term]

-- You can use this as is
getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x

-- You don't have to follow the order the functions appear in the file
-- For example, you could first define all Show instances, then all Eq instances etc.
-- if that implementation order is more convenient for you.



-- INSTANCES FOR POWER

instance Show Power where
    show (Power 0) = ""
    show (Power 1) = "x"
    show (Power n) = "x^" ++ show n

instance Eq Power where
    (Power n) == (Power m) = if n == m then True else False

instance Ord Power where
    (Power n) <= (Power m) = if n <= m then True else False

instance Evaluable Power where
    function (Power 0) = \x -> 1.00
    function (Power 1) = \x -> getRounded $ fromInteger x
    function (Power n) = \x -> getRounded $ fromInteger x^n

instance Differentiable Power where
    derivative (Power 0) = [Const 0]
    derivative (Power 1) = [Const 1]
    derivative (Power n) = [Pw n (Power (n-1))]

coeff :: Integer -> String
coeff 1 = ""
coeff (-1) = "-"
coeff n = show n

-- INSTANCES FOR POLYNOMIAL

showPoly :: (Integer, Power) -> String
showPoly (0, _) = ""
showPoly (1, Power 0) = "1"
showPoly (n, Power 0) = show n
showPoly (n, Power 1) = coeff n ++ "x"
showPoly (n, Power m) = coeff n ++ "x^" ++ show m 

instance Show Polynomial where
    show (Polynomial [(coef,power)]) = showPoly (coef,power)
    show (Polynomial ((coef, power):rest)) = if rest /= [] then showPoly (coef, power) ++ " + " ++ show (Polynomial rest)
                                                           else showPoly (coef, power)             
instance Evaluable Polynomial where
    function (Polynomial polyn) = \x -> evalPoly x 0.00 polyn

evalPoly :: Integer -> Double -> [(Integer, Power)] -> Double
evalPoly 0 result _ = 0.00
evalPoly x result [] = getRounded result
evalPoly x result ((coef, (Power n)):rest) = let number = coef*(x^n)
                                                 newResult = result + fromInteger number in evalPoly x newResult rest

instance Differentiable Polynomial where
    derivative (Polynomial []) = []
    derivative (Polynomial ((coeff, (Power 0)):rest)) = [] ++ derivative (Polynomial rest)
    derivative (Polynomial ((coeff, (Power 1)):rest)) = [Const coeff] ++ derivative (Polynomial rest)
    derivative (Polynomial ((coeff, (Power n)):rest)) = let newCoeff = coeff*n in [Pw newCoeff (Power (n-1))] ++ derivative (Polynomial rest)
    

-- INSTANCES FOR TRIGONOMETRIC

instance Show Trigonometric where
    show (Sin (Polynomial [])) = ""
    show (Sin (Polynomial [(0, _)])) = "sin0"
    show (Sin (Polynomial [(1, (Power 0))])) = "sin1"
    show (Sin (Polynomial [(n, (Power 0))])) = if n > 0 then "sin" ++ coeff n else "sin" ++ "(" ++ coeff n ++ ")"
    show (Sin (Polynomial [(n, (Power 1))])) = if n > 0 then "sin" ++ coeff n ++ "x" else "sin" ++ "(" ++ coeff n ++ "x" ++ ")" 
    show (Sin polyn) = "sin" ++ "(" ++ show polyn ++ ")"
    show (Cos (Polynomial [])) = "cos0"
    show (Cos (Polynomial [(0, _)])) = "cos0"
    show (Cos (Polynomial [(1, (Power 0))])) = "cos1"
    show (Cos (Polynomial [(n, (Power 0))])) = if n > 0 then "cos" ++ coeff n else "cos" ++ "(" ++ coeff n ++ ")"
    show (Cos (Polynomial [(n, (Power 1))])) = if n > 0 then "cos" ++ coeff n ++ "x" else "cos" ++ "(" ++ coeff n ++ "x" ++ ")"
    show (Cos polyn) = "cos" ++ "(" ++ show polyn ++ ")"

instance Evaluable Trigonometric where
    function (Sin (Polynomial polyn)) = \x -> getRounded $ sin $ evalPoly x 0.00 polyn
    function (Cos (Polynomial polyn)) = \x -> getRounded $ cos $ evalPoly x 0.00 polyn

instance Differentiable Trigonometric where
    derivative (Sin (Polynomial [])) = []
    derivative (Sin (Polynomial ((coef, (Power 0)):rest))) = [] ++ derivative (Sin (Polynomial rest))
    derivative (Sin (Polynomial ((coef, (Power 1)):rest))) = [Trig coef (Power 0) (Cos (Polynomial [(coef,(Power 1))]))] ++ derivative (Sin (Polynomial rest))
    derivative (Sin (Polynomial ((coef, (Power n)):rest))) = let newCoef = coef*n in [Trig newCoef (Power (n-1)) (Cos (Polynomial [(coef,(Power n))]))] ++ derivative (Sin (Polynomial rest))
    derivative (Cos (Polynomial [])) = []
    derivative (Cos (Polynomial ((coef, (Power 0)):rest))) = [] ++ derivative (Cos (Polynomial rest))
    derivative (Cos (Polynomial ((coef, (Power 1)):rest))) = [Trig ((-1)*coef) (Power 0) (Sin (Polynomial [(coef,(Power 1))]))] ++ derivative (Cos (Polynomial rest))
    derivative (Cos (Polynomial ((coef, (Power n)):rest))) = let newCoef = (-1)*coef*n in [Trig newCoef (Power (n-1)) (Sin (Polynomial [(coef,(Power n))]))] ++ derivative (Cos (Polynomial rest))


-- INSTANCES FOR EXPONENTIAL

instance Show Exponential where
    show (Exponential (Polynomial [(0, _)])) = "1"
    show (Exponential (Polynomial [(1, (Power 0))])) = "e"
    show (Exponential (Polynomial [(n, (Power 0))])) = "e^" ++ show n
    show (Exponential (Polynomial [(n, (Power 1))])) = if n > 0 then "e^" ++ coeff n ++ "x" else "e^" ++ "(" ++ coeff n ++ "x" ++ ")"
    show (Exponential polyn) = "e^" ++ "(" ++ show polyn ++ ")"

instance Evaluable Exponential where
    function (Exponential (Polynomial polyn)) = \x -> getRounded $ exp $ evalPoly x 0.00 polyn

instance Differentiable Exponential where
    derivative (Exponential (Polynomial [])) = []
    derivative (Exponential (Polynomial ((coef, (Power 0)):rest))) = [] ++ derivative (Exponential (Polynomial rest))
    derivative (Exponential (Polynomial ((coef, (Power 1)):rest))) = [Exp coef (Power 0) (Exponential (Polynomial [(coef, (Power 1))]))] ++ derivative (Exponential (Polynomial rest))
    derivative (Exponential (Polynomial ((coef, (Power n)):rest))) = let newCoef = coef*n in [Exp newCoef (Power (n-1)) (Exponential (Polynomial [(coef, (Power n))]))] ++ derivative (Exponential (Polynomial rest))


-- INSTANCES FOR TERM

instance Show Term where
    show (Const 0) = ""
    show (Const n) = show n
    show (Pw 0 power) = ""
    show (Pw n power) = coeff n ++ show power
    show (Exp 0 power expo) = ""
    show (Exp n power expo) = coeff n ++ show power ++ show expo
    show (Trig 0 power trigo) = ""
    show (Trig n power trigo) = coeff n ++ show power ++ show trigo

instance Evaluable Term where
    function (Const n) = \x -> getRounded (fromInteger n)
    function (Pw n power) = \x ->  getRounded (fromInteger n * function power x)
    function (Exp n power expo) = \x -> getRounded (fromInteger n * (function power x) * (function expo x))
    function (Trig n power trigo) = \x -> getRounded (fromInteger n * (function power x) * (function trigo x))

instance Differentiable Term where
    derivative (Const n) = [Const 0]
    derivative (Pw coef (Power 0)) = [Const 0] 
    derivative (Pw coef (Power 1)) = [Const coef]
    derivative (Pw coef (Power n)) = let newCoef = coef*n in [Pw newCoef (Power (n-1))]
    derivative term@(Exp n power (Exponential polyn)) = deriveFirstExpo term ++ deriveSecondExpo term polyn
    derivative term@(Trig n power (Sin polyn)) = deriveFirstTrigo term ++ deriveSecondTrigo term polyn
    derivative term@(Trig n power (Cos polyn)) = deriveFirstTrigo term ++ deriveSecondTrigo term polyn


deriveSecondExpo :: Term -> Polynomial -> [Term]
deriveSecondExpo (Exp n (Power k) (Exponential (Polynomial []))) polyn = []
deriveSecondExpo (Exp n (Power k) (Exponential (Polynomial ((coef, (Power m)):rest)))) polyn = [Exp (n*m*coef) (Power (m+k-1)) (Exponential polyn)] ++ deriveSecondExpo (Exp n (Power k) (Exponential (Polynomial rest))) polyn

deriveFirstExpo :: Term -> [Term]
deriveFirstExpo (Exp n (Power k) expo) = if n*k == 0 then [Const 0]
                                                     else [Exp (n*k) (Power (k-1)) expo]

deriveFirstTrigo :: Term -> [Term]
deriveFirstTrigo (Trig n (Power k) trigo) = if n*k == 0 then [Const 0]
                                                        else [Trig (n*k) (Power (k-1)) trigo]

deriveSecondTrigo :: Term -> Polynomial -> [Term]
deriveSecondTrigo (Trig n (Power k) (Sin (Polynomial []))) polyn = []
deriveSecondTrigo (Trig n (Power k) (Sin (Polynomial ((coef, (Power m)):rest)))) polyn = [Trig (n*m*coef) (Power (m+k-1)) (Cos polyn)] ++ deriveSecondTrigo (Trig n (Power k) (Sin (Polynomial rest))) polyn
deriveSecondTrigo (Trig n (Power k) (Cos (Polynomial []))) polyn = []
deriveSecondTrigo (Trig n (Power k) (Cos (Polynomial ((coef, (Power m)):rest)))) polyn = [Trig ((-1)*n*m*coef) (Power (m+k-1)) (Sin polyn)] ++ deriveSecondTrigo (Trig n (Power k) (Cos (Polynomial rest))) polyn

-- INSTANCES FOR [TERM]

instance Evaluable [Term] where
    function terms = \x -> evalMultipleTerms x 0.00 terms 
    
evalMultipleTerms :: Integer -> Double -> [Term] -> Double
evalMultipleTerms x result [] = getRounded result
evalMultipleTerms x result (term:rest) = let number = function term x
                                             newResult = result + number in evalMultipleTerms x newResult rest

instance Differentiable [Term] where
    derivative terms = deriveTerms terms
    
deriveTerms :: [Term] -> [Term]
deriveTerms terms = let list = concat $ map derivative terms
                        newList = throwZeros $ makeList list []
                    in newList

isContains :: Term -> [Term] -> Bool
isContains term1 [] = False
isContains term1 (term2:rest) = if isEquals term1 term2 then True else isContains term1 rest

makeList :: [Term] -> [Term] -> [Term]
makeList [] newList = newList
makeList (term1:xs) newList = if isContains term1 newList then makeList xs newList
                                                          else let newTerm = makeTerm term1 xs
                                                                   newnewList = newList ++ [newTerm]
                                                               in makeList xs newnewList

makeTerm :: Term -> [Term] -> Term
makeTerm term [] = term
makeTerm term1 (term2:rest) = if isEquals term1 term2 then let newTerm = addTerms term1 term2
                                                           in makeTerm newTerm rest 
                                                      else makeTerm term1 rest
    
addTerms :: Term -> Term -> Term
addTerms (Const n) (Const m) = Const (m+n)
addTerms (Pw c1 p1) (Pw c2 p2) = Pw (c1+c2) p1 
addTerms (Exp c1 p1 (Exponential (Polynomial polyn1))) (Exp c2 p2 (Exponential (Polynomial polyn2))) = Exp (c1+c2) p1 (Exponential (Polynomial polyn1))
addTerms (Trig c1 p1 (Sin (Polynomial polyn1))) (Trig c2 p2 (Sin (Polynomial polyn2))) = Trig (c1+c1) p1 (Sin (Polynomial polyn1))
addTerms (Trig c1 p1 (Cos (Polynomial polyn1))) (Trig c2 p2 (Cos (Polynomial polyn2))) = Trig (c1+c1) p1 (Cos (Polynomial polyn1))

isEquals :: Term -> Term -> Bool
isEquals (Const n) (Const m) = True
isEquals (Const n) (Exp c p e) = False
isEquals (Const n) (Trig c p t) = False
isEquals (Const n) (Pw c p) = False
isEquals (Pw c1 p1) (Pw c2 p2) = if p1 == p2 then True else False
isEquals (Pw c p) (Const m) = False
isEquals (Pw c p) (Exp c1 p1 e) = False
isEquals (Pw c p) (Trig c1 p1 t) = False
isEquals (Exp c1 p1 (Exponential polyn1)) (Exp c2 p2 (Exponential polyn2)) = if p1 == p2 && isPolynomialEqual polyn1 polyn2 then True else False
isEquals (Exp c p e) (Const m) = False
isEquals (Exp c p e) (Pw c1 p1) = False
isEquals (Exp c p e) (Trig c1 p1 t) = False
isEquals (Trig c1 p1 (Sin polyn1)) (Trig c2 p2 (Sin polyn2)) = if p1 == p2 && isPolynomialEqual polyn1 polyn2 then True else False
isEquals (Trig c1 p1 (Cos polyn1)) (Trig c2 p2 (Cos polyn2)) = if p1 == p2 && isPolynomialEqual polyn1 polyn2 then True else False                                                                                                                                                                
isEquals (Trig c1 p1 (Sin polyn1)) (Trig c2 p2 (Cos polyn2)) = False
isEquals (Trig c1 p1 (Cos polyn1)) (Trig c2 p2 (Sin polyn2)) = False
isEquals (Trig c p t) (Const m) = False
isEquals (Trig c p t) (Pw c1 p1) = False
isEquals (Trig c p t) (Exp c1 p1 e)= False

isPolynomialEqual :: Polynomial -> Polynomial -> Bool
isPolynomialEqual (Polynomial []) (Polynomial []) = True
isPolynomialEqual (Polynomial ((x,p1):xs)) (Polynomial ((y,p2):ys)) = if x == y && p1 == p2 then True && isPolynomialEqual (Polynomial xs) (Polynomial ys) else False

throwZeros :: [Term] -> [Term]
throwZeros [] = []
throwZeros ((Const 0):rest) = throwZeros rest
throwZeros ((Const n):rest) = [(Const n)] ++ throwZeros rest
throwZeros (power@(Pw n p):rest) = [power] ++ throwZeros rest
throwZeros (expo@(Exp n p e):rest) = [expo] ++ throwZeros rest
throwZeros (trig@(Trig n p t):rest) = [trig] ++ throwZeros rest

