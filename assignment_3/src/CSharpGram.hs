module CSharpGram where

import CSharpLex
import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import Prelude hiding (sequence, (*>), (<$), (<*), (<*>))

data Class = Class String [Member]
  deriving (Show)

data Member
  = MemberD Decl
  | MemberM Type String [Decl] Stat
  deriving (Show)

data Stat
  = StatDecl Decl
  | StatExpr Expr
  | StatIf Expr Stat Stat
  | StatWhile Expr Stat
  | StatReturn Expr
  | StatBlock [Stat]
  deriving (Show)

data Expr
  = ExprConst Int
  | ExprVar String
  | ExprOper String Expr Expr
  deriving (Show)

data Decl = Decl Type String
  deriving (Show)

data Type
  = TypeVoid
  | TypePrim String
  | TypeObj String
  deriving (Eq, Show)

pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

pMember :: Parser Token Member
pMember =
  MemberD <$> pDeclSemi
    <|> pMeth

pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
  where
    methRetType = pType <|> TypeVoid <$ symbol KeyVoid
    methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

pStatDecl :: Parser Token Stat
pStatDecl =
  pStat
    <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =
  StatExpr <$> pExpr <* sSemi
    <|> StatIf <$ symbol KeyIf <*> parenthesised pExpr <*> pStat <*> optionalElse
    <|> StatWhile <$ symbol KeyWhile <*> parenthesised pExpr <*> pStat
    <|> StatReturn <$ symbol KeyReturn <*> pExpr <* sSemi
    <|> pBlock
  where
    optionalElse = option (symbol KeyElse *> pStat) (StatBlock [])

pExprSimple :: Parser Token Expr
pExprSimple =
  ExprConst <$> sConst
    <|> ExprVar <$> sLowerId
    <|> parenthesised pExpr

pExpr :: Parser Token Expr
pExpr = pExprComplex operatorTable
  where
    -- Operators ordered by lowest precedence to highest precedence.
    operatorTable =
      [ -- Assignment
        ["="],
        -- Conditional OR
        ["||"],
        -- Conditional AND
        ["&&"],
        -- Boolean logical XOR or bitwise logical XOR???
        ["^"],
        -- Equality
        ["==", "!="],
        -- Relation
        ["<=", "<", ">=", ">"],
        -- Additive
        ["+", "-"],
        -- Multiplicative
        ["*", "/", "%"]
      ]

    pExprComplex :: [[String]] -> Parser Token Expr
    pExprComplex [] = pExprSimple
    pExprComplex (x : xs) = chain x (pExprComplex xs) (parseOps x)
      where
        -- Only assignment (=) is right associative.
        chain ["="] = chainr
        chain _ = chainl

        -- Parse any `xs` operator statement and return it as a ExprOper.
        parseOps :: [String] -> Parser Token (Expr -> Expr -> Expr)
        parseOps xs = ExprOper <$> sOp xs

        -- Parse any `xs` operator symbol and return its string value.
        sOp :: [String] -> Parser Token String
        sOp xs = (\(Operator x) -> x) <$> satisfy f
          where
            f (Operator x) = x `elem` xs
            f _ = False

pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = pDecl <* sSemi

pType :: Parser Token Type
pType =
  TypePrim <$> sStdType
    <|> TypeObj <$> sUpperId

-- The `Token` equivalents to some basic parser combinators
parenthesised, bracketed, braced :: Parser Token b -> Parser Token b
parenthesised p = pack (symbol POpen) p (symbol PClose) --(p)
bracketed p = pack (symbol SOpen) p (symbol SClose) --[p]
braced p = pack (symbol COpen) p (symbol CClose) --{p}
