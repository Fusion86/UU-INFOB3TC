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
type C = Code                           -- Class
type M = Env -> (Code, Env)             -- Member
type S = Env -> (Code, Env)             -- Statement
type E = ValueOrAddress -> Env -> Code  -- Expression
{- ORMOLU_ENABLE -}

data Env = Env
  { maxSize :: Int,
    variables :: M.Map String Int
  }
  deriving (Show)

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving (Show)

addVar :: Env -> String -> Env
addVar (Env maxSize variables) var = case M.lookup var variables of
  Nothing -> Env newMaxSize (M.insert var (M.size variables) variables)
  --
  Just n -> error $ "Variable '" ++ var ++ "' is already defined in this local scope."
  where
    newMaxSize = max maxSize (M.size variables + 1)

emptyEnv :: Env
emptyEnv = Env 0 M.empty

getVar :: String -> Env -> Int
getVar name (Env _ vars) = case M.lookup name vars of
  Nothing -> error $ "No variable '" ++ name ++ "' exists."
  Just n -> n

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
    f x = fst (x emptyEnv)

codeMember :: (Decl -> M, Type -> String -> [Decl] -> S -> M)
codeMember = (fMembDecl, fMembMeth)
  where
    fMembDecl :: Decl -> M
    fMembDecl d env = trace "warning: member variables are not implemented!" ([], env)

    fMembMeth :: Type -> String -> [Decl] -> S -> M
    -- LINK and UNLINK should actually be implemented in fStatBlock, but we don't get paid enough to do so.
    fMembMeth t x ps s env = trace ("envMaxSize: " ++ show envMaxSize) ([LABEL x, LINK envMaxSize] ++ code ++ [UNLINK, RET], emptyEnv)
      where
        envMaxSize = maxSize newEnv
        innerEnv = foldl f env ps
        f (Env _ vars) (Decl _ name) = env {variables = M.insert name (- length vars - 2) vars}
        (code, newEnv) = s innerEnv

codeStatement ::
  ( Decl -> S,
    E -> S,
    E -> S -> S -> S,
    E -> S -> S,
    E -> S,
    [Env -> (Code, Env)] -> Env -> (Code, Env)
  )
codeStatement = (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
  where
    fStatDecl :: Decl -> S
    fStatDecl (Decl t var) env = ([], addVar env var)

    fStatExpr :: E -> S
    fStatExpr e env = (e Value env ++ [pop], env)

    fStatIf :: E -> S -> S -> S
    fStatIf e s1 s2 env = (c ++ [BRF (n1 + 2)] ++ fst stat1 ++ [BRA n2] ++ fst stat2, newEnv)
      where
        c = e Value env
        (n1, n2) = (codeSize (fst stat1), codeSize (fst stat2))
        stat1 = s1 env
        stat2 = s2 env

        -- Kinda shitty, maybe make a helper function for this or refactor the code so that it isn't needed?
        -- We take the maxSize of the largest branch, because the other branch will then fit as well.
        -- These two branches can NEVER access each others variables, meaning that is is fine if they overwrite each other.
        newEnv = env {maxSize = max (maxSize (snd stat1)) (maxSize (snd stat2))}

    fStatWhile :: E -> S -> S
    fStatWhile e s1 env = ([BRA n] ++ fst stat ++ c ++ [BRT (- (n + k + 2))], newEnv)
      where
        c = e Value env
        stat = s1 env
        newEnv = env {maxSize = maxSize (snd stat)}
        (n, k) = (codeSize (fst stat), codeSize c)

    fStatReturn :: E -> S
    fStatReturn e env = (e Value env ++ [STR R4, UNLINK] ++ [RET], env)

    fStatBlock :: [Env -> (Code, Env)] -> Env -> (Code, Env)
    fStatBlock xs env = foldl f ([], env) xs
      where
        -- f :: (Env -> (Code, Env)) -> (Code, Env) -> (Code, Env)
        f (inCode, inEnv) x = (inCode ++ newCode, newEnv)
          where
            !(newCode, newEnv) = x (dbg "inEnv" inEnv)

codeExpr = (fExprCon, fExprVar, fExprOp, fExprCall)
  where
    fExprCon :: Int -> E
    fExprCon n va env = [LDC n]

    fExprVar :: String -> E
    fExprVar x va env =
      case va of
        Value -> [LDL loc]
        Address -> [LDLA loc]
      where
        loc = getVar x env

    fExprOp :: String -> E -> E -> E
    fExprOp "=" e1 e2 va env = e2 Value env ++ [LDS 0] ++ e1 Address env ++ [STA 0]
    fExprOp "&&" e1 e2 va env =
      let stat2 = e2 Value env
       in e1 Value env ++ [LDS 0, BRF (codeSize stat2 + 1)] ++ stat2 ++ [AND]
    fExprOp "||" e1 e2 va env =
      let stat2 = e2 Value env
       in e1 Value env ++ [LDS 0, BRT (codeSize stat2 + 1)] ++ stat2 ++ [OR]
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

    fExprCall :: String -> [E] -> E
    fExprCall "print" xs va env = concatMap f xs
      where
        f :: E -> Code
        -- LDR R4 is not 100% correct, because print SHOULD return nothing.
        -- However, every statement is expected to return something, which means that print also has to return
        -- something. Even if that something is random garbage.
        f x = x Value env ++ [TRAP 0, LDR R4]
    fExprCall func xs va env = evalParams ++ [Bsr func] ++ [AJS (- (length xs)), LDR R4]
      where
        evalParams = concatMap (\x -> x Value env) xs
        paramCount = length xs
