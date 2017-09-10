{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

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
