{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ConstrainedClassMethods   #-}
module Chops.AST where

import qualified Data.Map            as M
import           Data.Typeable

import Formatting

class VType t where
  extr :: VarType -> t
  extrE :: VarType -> Either VarType t
  injt :: t -> VarType 


data VarType where
  I :: Integer -> VarType
  S :: String -> VarType
  F :: Float -> VarType

instance VType Integer where
  extr (I i) = i
  extrE (I i) = Right i
  extrE (F i) = Right $ round i
  extrE x     = Left x
  injt = I

instance VType Float where
  extr (F f) = f
  extrE (F f) = Right f
  extrE (I f) = Right $ fromIntegral f
  extrE x     = Left x
  injt = F

instance VType String where
  extr (S s) = s
  extrE (S s) = Right s
  extrE x     = Left x
  injt = S

instance Show VarType where
  show (I x) = "I " ++ show x
  show (S x) = "S " ++ show x
  show (F x) = "F " ++ show x




type Env =  M.Map String VarType

envLookup :: (VType a,Typeable a) => Env -> String -> a
envLookup e x = case M.lookup x e of
                  Nothing -> error $ "Unknown Variable : " ++ x
                  Just v ->  case extrE v of
                              Left  t  -> error $ "Variable is of type : " ++ x 
                                                  ++ " :: " ++ show (typeOf t) 
                                                  ++ " = " ++ show t
                              Right t  -> t
                  




data Expr a where
  Con    :: VType a => a -> Expr a
  Var    :: (VType a,Typeable a) => String -> Expr a
  (:+:)  :: (VType a,Num a) => Expr a -> Expr a -> Expr a
  (:-:)  :: (VType a,Num a) => Expr a -> Expr a -> Expr a
  (:*:)  :: (VType a,Num a) => Expr a -> Expr a -> Expr a
  (:/:)  :: (Show a,Show b, Real a,Real b,VType a,VType b,VType c,Fractional c) => Expr a -> Expr b -> Expr c
  (://:) :: (Show a,Integral a,VType a) => Expr a -> Expr a -> Expr a


infixl 6 :+:,:-:
infixl 7 :*:,:/:,://:

prec :: Expr a -> Int
prec (Con _) = 10
prec (Var _) = 10
prec (_ :*: _) = 7
prec (_ :/: _) = 7
prec (_ ://: _) = 7
prec (_ :+: _) = 6
prec (_ :-: _) = 6


instance (Show a) => Show (Expr a) where
  showsPrec p (Con n) = shows n
  showsPrec p (Var n) = \s -> n ++ s
  showsPrec p e0 =
    case e0 of
     x :+: y -> showbin 6 " + " x y
     x :-: y -> showbin 6 " - " x y
     x :*: y -> showbin 7 " * " x y
     x :/: y -> showbin 7 " / " x y
     x ://: y -> showbin 7 " // " x y
    where 
      showbin :: (Show a,Show b) => Int -> String -> Expr a -> Expr b -> ShowS
      showbin pr s x y =
            showParen (p > pr) $
             showsPrec pr x . (s ++) .
             showParen (prec y == pr) (showsPrec pr y)

eval :: Env -> Expr a -> a
eval _ (Con a)   = a
eval e (Var s)   = envLookup e s
eval e (a :+: b) = eval e a + eval e b
eval e (a :*: b) = eval e a * eval e b
eval e (a :-: b) = eval e a - eval e b
eval e (a :/: b) = realToFrac (eval e a) / realToFrac (eval e b)
eval e (a ://: b) = eval e a `div`  eval e b


type Mrk = String
type Time = (Integer,Integer,Integer)
type Addr = Integer

data Stmt where
  BGN   :: String -> Stmt
--  PARA  :: String -> Expr a -> Stmt
  SEL   :: String -> Stmt
  LD    :: Expr String -> Stmt
  BPM   :: Float -> Stmt
  SIG   :: Int -> Int -> Stmt
  SRST  :: Stmt
  FIT   :: Expr Float -> Stmt
  MRK   :: Mrk -> Expr Float -> Stmt
  SET   :: (Show a,VType a) => String -> Expr a -> Stmt
  PLAYF :: Mrk -> Stmt
  PLAYB :: Mrk -> Stmt
  SEEK  :: Expr Float -> Stmt
  WAIT  :: Time -> Stmt
  WAITT :: Time -> Stmt
  JDNZ  :: String -> Addr -> Stmt
  JDGZ  :: String -> Addr -> Stmt
  JMP   :: Addr -> Stmt
  ONBT  :: Mrk -> Stmt
  STOP  :: Stmt
  HLT   :: Stmt

instance Show Stmt where
 show (JMP x) = "JMP " ++ show x 
 show (JDNZ x a) = "JDNZ " ++ x ++ "," ++ show a
 show (JDGZ x a) = "JDGZ " ++ x ++ "," ++ show a
 show (BGN s) = "BGN " ++ s
 show (LD s) = "LD " ++ show s
 show (BPM f) = "BPM " ++ show f
 show (SIG b r) = "SIG " ++ show b ++ "/" ++ show r
 show (SEL s) = "SEL " ++ s
 show (SET s v) = "SET " ++ s ++ " , " ++ show v  
 show (WAITT (nbars,nbts,npls)) = "WAITT " ++ formatToString ("[" % left 4 '0'  % ":" 
                                           % left 2 '0'  % ":" 
                                           % left 3 '0' %  "]") nbars nbts npls 
 show (WAIT (nbars,nbts,npls)) = "WAIT " ++ formatToString ("[" % left 4 '0'  % ":" 
                                           % left 2 '0'  % ":" 
                                           % left 3 '0' %  "]") nbars nbts npls 
 show (MRK m v) = "MRK " ++ show m ++ "," ++ show v
 show (FIT fac) = "FIT " ++ show fac
 show (SEEK m) = "SEEK " ++ show m
 show (PLAYF m) = "PLAYF " ++ show m
 show (PLAYB m) = "PLAYB " ++ show m
 show SRST = "SRST"
 show STOP = "STOP"
 show HLT = "HLT"
 show _       = "Stmt" 


