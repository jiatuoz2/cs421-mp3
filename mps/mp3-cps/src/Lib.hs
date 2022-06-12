--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = k (factk (n-1) (\x -> x * n))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk (a:[]) k1 k2 
    | even a = k1 a
    | otherwise = k2 a
evenoddk (a:as) k1 k2 = 
    case a `mod` 2 of
        0 -> evenoddk as (\x -> k1 (a + x)) k2
        _ -> evenoddk as k1 (\x -> k2 (a + x))

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (IntExp _) = True
isSimple (VarExp _) = True
isSimple (AppExp _ _) = False
isSimple (IfExp e1 e2 e3) = isSimple e1 && isSimple e2 && isSimple e3
isSimple (OpExp _ e1 e2) = isSimple e1 && isSimple e2

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k cnt = (AppExp k (IntExp i), cnt)
cpsExp (VarExp v) k cnt = (AppExp k (VarExp v), cnt)

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp e1 e2) k cnt = 
    case isSimple e2 of
        False -> 
            let v = fst $ gensym cnt
            in cpsExp e2 (LamExp v (AppExp (AppExp e1 (VarExp v)) k)) (cnt + 1)
        True -> (AppExp (AppExp e1 e2) k, cnt)

--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op e1 e2) k cnt = 
    case isSimple e1 of
        True -> 
            case isSimple e2 of
                True -> ((AppExp k (OpExp op e1 e2)), cnt)
                False -> 
                    let v = fst $ gensym cnt
                    in cpsExp e2 (LamExp v (AppExp k (OpExp op e1 (VarExp v)))) (cnt + 1)
        False -> 
            case isSimple e2 of 
                True -> 
                    let v = fst $ gensym cnt
                    in cpsExp e1 (LamExp v (AppExp k (OpExp op (VarExp v) e2))) (cnt + 1)
                False -> 
                    let v1 = fst $ gensym cnt
                        v2 = fst $ gensym (cnt + 1)
                    in cpsExp e1 (LamExp v1 (fst (cpsExp e2 (LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))) (cnt + 2)))) (cnt + 2)

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp e1 e2 e3) k cnt = 
    case isSimple e1 of
        True -> (IfExp e1 (fst $ cpsExp e2 k cnt) (fst $ cpsExp e3 k cnt), cnt)
        False -> 
            let v = fst $ gensym cnt
            in cpsExp e1 (LamExp v (IfExp (VarExp v) (fst $ cpsExp e2 k (cnt + 1)) (fst $ cpsExp e3 k (cnt + 1)))) (cnt + 1)

--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl name ps body) = 
    let k = "k"
    in Decl name (ps ++ [k]) (fst $ cpsExp body (VarExp k) 0)
