module CSharpLex where

import Common
import Control.Monad (guard)
import Data.Char
import ParseLib.Abstract
import Prelude hiding (sequence, (*>), (<$), (<*))

data Token
  = POpen
  | PClose -- parentheses     ()
  | SOpen
  | SClose -- square brackets []
  | COpen
  | CClose -- curly braces    {}
  | Comma
  | Semicolon
  | KeyIf
  | KeyElse
  | KeyWhile
  | KeyReturn
  | KeyTry
  | KeyCatch
  | KeyClass
  | KeyVoid
  | StdType String -- the 8 standard types
  | Operator String -- the 15 operators
  | UpperId String -- uppercase identifiers
  | LowerId String -- lowercase identifiers
  | ConstInt Int
  | ConstBool Bool
  | ConstChar Char
  deriving (Eq, Show)

----- Begin Lexer -----
lexicalScanner :: Parser Char [Token]
lexicalScanner = lexDiscard *> greedy (lexToken <* lexDiscard) <* eof

lexToken :: Parser Char Token
lexToken =
  greedyChoice
    [ lexTerminal,
      lexEnum StdType stdTypes,
      lexEnum Operator operators,
      lexConstInt,
      lexConstBool,
      lexConstChar,
      lexLowerId,
      lexUpperId
    ]

lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t, s) <- terminals]
  where
    terminals :: [(Token, String)]
    terminals =
      [ (POpen, "("),
        (PClose, ")"),
        (SOpen, "["),
        (SClose, "]"),
        (COpen, "{"),
        (CClose, "}"),
        (Comma, ","),
        (Semicolon, ";"),
        (KeyIf, "if"),
        (KeyElse, "else"),
        (KeyWhile, "while"),
        (KeyReturn, "return"),
        (KeyTry, "try"),
        (KeyCatch, "catch"),
        (KeyClass, "class"),
        (KeyVoid, "void")
      ]

lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

stdTypes :: [String]
stdTypes = ["int", "long", "double", "float", "byte", "short", "bool", "char"]

operators :: [String]
operators = ["+", "-", "*", "/", "%", "&&", "||", "^", "<=", "<", ">=", ">", "==", "!=", "="]

lexConstInt :: Parser Char Token
lexConstInt = ConstInt . read <$> greedy1 (satisfy isDigit)

lexConstBool :: Parser Char Token
lexConstBool = ConstBool . parseBool <$> choice [token "true", token "false"]
  where
    parseBool "true" = True
    parseBool _ = False

-- Doesn't support escaped characters like `\0`.
lexConstChar :: Parser Char Token
lexConstChar = ConstChar <$> pack sep (satisfy isAlphaNum <|> symbol ' ') sep
  where
    sep = symbol '\''

lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x : xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x : xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)

lexDiscard :: Parser Char ()
lexDiscard = () <$ greedy (lexWhiteSpace <|> lexComment)

lexWhiteSpace :: Parser Char String
lexWhiteSpace = (: []) <$> satisfy isSpace

lexComment :: Parser Char String
lexComment = do
  token "//"
  x <- greedy $ satisfy ('\n' /=)
  symbol '\n'
  return x

keyword :: String -> Parser Char String
keyword [] = succeed ""
keyword xs@(x : _)
  | isLetter x = do
    ys <- greedy (satisfy isAlphaNum)
    guard (xs == ys)
    return ys
  | otherwise = token xs

greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty

----- End Lexer -----

----- Utilities for consuming tokens -----
sStdType :: Parser Token String
sStdType = (\(StdType x) -> x) <$> satisfy isStdType
  where
    isStdType (StdType _) = True
    isStdType _ = False

sUpperId :: Parser Token String
sUpperId = (\(UpperId x) -> x) <$> satisfy isUpperId
  where
    isUpperId (UpperId _) = True
    isUpperId _ = False

sLowerId :: Parser Token String
sLowerId = (\(LowerId x) -> x) <$> satisfy isLowerId
  where
    isLowerId (LowerId _) = True
    isLowerId _ = False

sConst :: Parser Token Int
sConst = getValue <$> satisfy isConst
  where
    isConst (ConstInt _) = True
    isConst (ConstBool _) = True
    isConst (ConstChar _) = True
    isConst _ = False

    getValue :: Token -> Int
    getValue (ConstInt x) = x
    getValue (ConstBool x) = if x then 1 else 0
    getValue (ConstChar x) = ord x
    getValue _ = error "Can't convert this value to an integer."

sOperator :: Parser Token String
sOperator = (\(Operator x) -> x) <$> satisfy isOperator
  where
    isOperator (Operator _) = True
    isOperator _ = False

sSemi :: Parser Token Token
sSemi = symbol Semicolon
