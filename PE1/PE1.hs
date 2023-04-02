{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use foldr" #-}
module PE1 where

import Text.Printf
import Distribution.Simple.Utils (xargs)
import System.Posix.Internals (lstat)

-- PE1: Recipe Calculator
-- The premise of this homework if to write a recipe calculator that
-- calculates: how much a recipe costs to make, what can be made with the
-- ingredients already available, and how much extra ingredients need to
-- be bought in order to make a recipe.

-- Recipe = Recipe Name [(Ingredient, Quantity)]
data Recipe = Recipe String [(String, Double)] deriving Show

-- Price = Price Ingredient Quantity Price
data Price = Price String Double Double deriving Show

-- You can use this as-is
getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x

-- Calculate how much the given amount of the given ingredient costs
getIngredientCost :: (String, Double) -> [Price] -> Double
getIngredientCost (x,y) ((Price ingredient quantity price):xs) = let name = ingredient
                                                                 in if x == name then let result = (price/quantity)*y
                                                                                      in getRounded result
                                                                    else getIngredientCost (x,y) xs

-- Calculate how much it costs to buy all the ingredients of a recipe
recipeCost :: Recipe -> [Price] -> Double
recipeCost (Recipe _ []) priceList = 0
recipeCost (Recipe name (x:xs)) priceList = let prices = priceList
                                            in getIngredientCost x prices + recipeCost (Recipe name xs) prices
                                            


getMissingAmount :: (String, Double) -> [(String,Double)] -> [(String, Double)]
getMissingAmount (x,y) [] = [(x,y)]
getMissingAmount (x,y) ((ingredient,amount):rest) = let name = ingredient
                                                    in if x == name then if amount < y then let result = y - amount
                                                                                            in [(x,getRounded result)]
                                                                         else []
                                                       else getMissingAmount (x,y) rest
                                                       
-- Given a list of how much you already have of each ingredient,
-- calculate how much of which ingredients are missing for a recipe
missingIngredients :: Recipe -> [(String, Double)] -> [(String, Double)]
missingIngredients (Recipe _ []) pantry = []
missingIngredients (Recipe name (x:xs)) pantry = let ingredient = x
                                                 in getMissingAmount x pantry ++ missingIngredients (Recipe name xs) pantry

doesExist :: (String, Double) -> [(String, Double)] -> Bool
doesExist (x,y) [] = False
doesExist (x,y) ((ingredient, amount):rest) = let name = ingredient
                                              in if x == name then if y > amount then False
                                                                   else True
                                                 else doesExist (x,y) rest

allIngredients :: [(String, Double)] -> [(String,Double)] -> Bool
allIngredients [] stock = True
allIngredients (x:xs) stock = if doesExist x stock then allIngredients xs stock
                              else False

leftAmount :: (String, Double) -> [(String, Double)] -> [(String, Double)]
leftAmount (x,y) [] = [(x, getRounded y)]
leftAmount (x,y) ((ingredient, amount):rest) = if x == ingredient then let result = getRounded (y - amount)
                                                                       in [(x, getRounded result)]
                                               else leftAmount (x,y) rest

makeList :: [(String, Double)] -> Recipe -> [(String, Double)]
makeList [] recipe = []
makeList stock recipe = let firstIng = head stock
                            lastIngs = tail stock
                            Recipe name list = recipe
                        in leftAmount firstIng list ++ makeList lastIngs recipe
                
-- Given a list of ingredients in your kitchen, calculate what you would
-- have left after making the given recipe. If there isn't enough of an
-- ingredient, the recipe cannot be made! You shouldn't change the amount
-- of ingredient in that case.
makeRecipe :: [(String, Double)] -> Recipe -> [(String, Double)]
makeRecipe [] recipe = []
makeRecipe stock recipe = let Recipe name list = recipe
                          in if allIngredients list stock then makeList stock recipe
                             else stock

recipeList :: Recipe -> [(String, Double)]
recipeList (Recipe recipeName []) = []
recipeList (Recipe recipeName ((ingredient, amount):xs)) = [(ingredient, amount)] ++ recipeList (Recipe recipeName xs)

shoppingListWithDup :: [Recipe] -> [(String, Double)]
shoppingListWithDup [] = []
shoppingListWithDup (x:xs) = recipeList x ++ shoppingListWithDup xs

summing :: (String, Double) -> [(String, Double)] -> [(String, Double)]
summing (x,y) [] = [(x,y)]
summing (x,y) ((ingredient, amount):rest) = if x == ingredient then let result = y + amount
                                                                    in summing (x, result) rest
                                            else summing (x, y) rest
                                            
missingPart :: [(String, Double)] -> [(String, Double)] -> [(String,Double)]
missingPart [] pantry = []
missingPart (x:xs) pantry = getMissingAmount x pantry ++ missingPart xs pantry
                                                 
                                                 
isContain :: (String, Double) -> [(String, Double)] -> Bool
isContain (x,y) [] = False
isContain (x,y) ((ingredient, amount):rest) = if x == ingredient then True
                                              else isContain (x,y) rest 

destroyDuplicates :: [(String, Double)] -> [(String, Double)]
destroyDuplicates [] = []
destroyDuplicates (x:xs) = if isContain x xs then destroyDuplicates xs
                           else x : destroyDuplicates xs
                           
resultingListWithoutPrices ::  [(String, Double)] -> [(String, Double)] -> [(String, Double)]
resultingListWithoutPrices [] duplicated = []
resultingListWithoutPrices ((x,y):xs) duplicated = summing (x,0) duplicated ++ resultingListWithoutPrices xs duplicated

listWithPrices :: [(String, Double)] -> [Price] -> [(String, Double, Double)]
listWithPrices [] prices = []
listWithPrices ((x,y):rest) prices = let cost = getRounded $ getIngredientCost (x,y) prices 
                              in [(x,y,cost)] ++ listWithPrices rest prices
-- Given a list of ingredients you already have, and a list of recipes,
-- make a shopping list showing how much of each ingredient you need
-- to buy, and its cost. Each ingredient mush appear in the shopping list
-- at most once (no duplicates!).
makeShoppingList :: [(String, Double)] -> [Recipe] -> [Price] -> [(String, Double, Double)]
makeShoppingList stock recipes prices = let shoppingList = shoppingListWithDup recipes
                                            withoutDuplicates = destroyDuplicates shoppingList
                                            needings = resultingListWithoutPrices withoutDuplicates shoppingList
                                            resultingList = missingPart needings stock
                                            shoppingListWithPrices = listWithPrices resultingList prices
                                        in shoppingListWithPrices
