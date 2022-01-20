{-# LANGUAGE BangPatterns #-}

module CSharpCode where

import CSharpAlgebra
import CSharpGram
import CSharpLex
import Common
import qualified Data.Map as M
import SSM
import Prelude hiding (EQ, GT, LT)

{-
  This file contains a starting point for the code generation which should handle very simple programs.
-}

{- ORMOLU_DISABLE -}
-- The types that we generate for each datatype: Our type variables for the algebra
type Env = M.Map String Int
type C = Code                   -- Class
type M = Env -> (Code, Env)                   -- Member
type S = Env -> Int -> (Code, Env, Int)     -- Statement
type E = ValueOrAddress -> Env -> Code -- Expression
{- ORMOLU_ENABLE -}

codeAlgebra :: CSharpAlgebra C M S E
codeAlgebra =
  ( codeClass,
    codeMember,
    codeStatement,
    codeExpr
  )

codeClass :: String -> [M] -> C
codeClass c ms = [Bsr "main", HALT] ++ concatMap f ms
  where
    f :: M -> Code
    f x = fst (x M.empty)

codeMember :: (Decl -> M, Type -> String -> [Decl] -> S -> M)
codeMember = (fMembDecl, fMembMeth)
  where
    fMembDecl :: Decl -> M
    fMembDecl d env = trace "warning: member variables are not implemented!" ([], env)

    fMembMeth :: Type -> String -> [Decl] -> S -> M
    -- TODO: Implement local vars using LINK and UNLINK.
    fMembMeth t x ps s env = trace ("envMaxSize: " ++ show envMaxSize) ([LABEL x] ++ code ++ [RET], M.empty)
      where
        (code, newEnv, envMaxSize) = s env

codeStatement = (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
  where
    fStatDecl :: Decl -> S
    fStatDecl (Decl t var) env envSize = ([], newEnv, envSize + 1)
      where
        newEnv = M.insert var (M.size env) env
    fStatExpr :: E -> S
    fStatExpr e env = (e Value env ++ [pop], env)

    fStatIf :: E -> S -> S -> S
    fStatIf e s1 s2 env = (c ++ [BRF (n1 + 2)] ++ fst stat1 ++ [BRA n2] ++ fst stat2, env)
      where
        c = e Value env
        (n1, n2) = (codeSize (fst stat1), codeSize (fst stat2))
        stat1 = s1 env
        stat2 = s2 env

    fStatWhile :: E -> S -> S
    fStatWhile e s1 env = ([BRA n] ++ fst stat ++ c ++ [BRT (- (n + k + 2))], env, M.size (snd stat))
      where
        c = e Value env
        stat = s1 env
        (n, k) = (codeSize (fst stat), codeSize c)

    fStatReturn :: E -> S
    fStatReturn e env = (e Value env ++ [pop] ++ [RET], env)

    fStatBlock :: [Env -> (Code, Env)] -> Env -> (Code, Env)
    fStatBlock xs env = foldl f ([], env) xs
      where
        -- f :: (Code, Env) -> (Env -> (Code, Env)) -> (Code, Env)
        -- f = undefined

        -- f :: (Env -> (Code, Env)) -> (Code, Env) -> (Code, Env)
        f (inCode, inEnv) x =
          let !a = dbg "newEnv" newEnv
           in (inCode ++ newCode, a)
          where
            !(newCode, newEnv) = x (dbg "inEnv" inEnv)

codeExpr = (fExprCon, fExprVar, fExprOp)
  where
    fExprCon :: Int -> E
    fExprCon n va env = [LDC n]

    fExprVar :: String -> E
    fExprVar x va env =
      case va of
        Value -> [LDL loc]
        Address -> [LDLA loc]
      where
        loc = case M.lookup x env of
          Nothing -> error $ "No variable '" ++ x ++ "' exists."
          Just n -> n

    fExprOp :: String -> E -> E -> E
    fExprOp "=" e1 e2 va env = e2 Value env ++ [LDS 0] ++ e1 Address env ++ [STA 0]
    fExprOp op e1 e2 va env = e1 Value env ++ e2 Value env ++ [opCodes M.! op]
      where
        opCodes :: M.Map String Instr
        opCodes =
          M.fromList
            [ ("+", ADD),
              ("-", SUB),
              ("*", MUL),
              ("/", DIV),
              ("%", MOD),
              ("<=", LE),
              (">=", GE),
              ("<", LT),
              (">", GT),
              ("==", EQ),
              ("!=", NE),
              ("&&", AND),
              ("||", OR),
              ("^", XOR)
            ]

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving (Show)
