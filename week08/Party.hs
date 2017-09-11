{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.List (sort)
import Data.Tree (Tree(Node))
import Employee (GuestList(GL), Employee(Emp, empFun, empName))
import System.IO (IOMode(ReadMode), Handle, withFile, hGetContents)

{- Exercise 1 -}

glCons :: Employee -> GuestList -> GuestList
glCons emp@Emp{empFun} (GL currEmps currFun) =
  GL (emp:currEmps) (currFun + empFun)

-- version without using NamedFieldPuns
glCons' :: Employee -> GuestList -> GuestList
glCons' emp@Emp{empFun = theEmpFun} (GL currEmps currFun) =
  GL (emp:currEmps) (currFun + theEmpFun)

  -- version without using records
glCons'' :: Employee -> GuestList -> GuestList
glCons'' emp@(Emp _ theEmpFun) (GL currEmps currFun) =
  GL (emp:currEmps) (currFun + theEmpFun)

instance Monoid GuestList where
  mempty = GL [] 0

  (GL leftEmps leftFun) `mappend` (GL rightEmps rightFun) =
    GL (leftEmps ++ rightEmps) (leftFun + rightFun)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

{- Exercise 2 -}
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node label children) = f label $ map (treeFold f) children

{- Exercise 3 -}

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (withThisBoss, withoutThisBoss)
  where
    withoutThisBoss :: GuestList
    withoutThisBoss = foldMap (uncurry moreFun) gls

    withThisBoss :: GuestList
    withThisBoss = glCons boss $ foldMap snd gls

    {-
    withThisBoss :: GuestList
    withThisBoss = glCons boss . mconcat . map withOrWithoutThisBoss $
      zip gls subTreesWithThisBoss
    
    withOrWithoutThisBoss :: ((GuestList, GuestList), GuestList) -> GuestList
    withOrWithoutThisBoss ((withSub, withoutSub), withoutSubWithThisBoss)
      | withSub > withoutSubWithThisBoss = withSub
      | otherwise = withoutSub

    subTreesWithThisBoss :: [GuestList]
    subTreesWithThisBoss = map (glCons boss . snd) gls
    -}

{- Exercise 4 -}
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

{- Exercise 5 -}
main :: IO ()
main = withFile "company.txt" ReadMode printGuestList

printGuestList :: Handle -> IO ()
printGuestList handle =
  putStrLn . formatGuestList . maxFun . read =<< hGetContents handle

formatGuestList :: GuestList -> String
formatGuestList (GL emps fun) =
  unlines $ ("Total fun: " ++ show fun) : sort (map empName emps)
