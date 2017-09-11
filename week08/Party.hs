{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Tree (Tree, flatten)
import Employee (GuestList(GL), Employee(Emp, empFun))

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
treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f acc = foldr f acc . flatten

{- Exercise 3 -}

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (withThisBoss, withoutThisBoss)
  where
    withoutThisBoss :: GuestList
    withoutThisBoss = mconcat $ map (uncurry moreFun) gls

    withThisBoss :: GuestList
    withThisBoss = glCons boss withoutThisBoss

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
