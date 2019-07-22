{-# LANGUAGE TupleSections #-}
module Expressions ( Expr
                   , tokenize
                   ) where

import Control.Monad (void, guard)
import Data.Functor.Identity(Identity)
import Data.Char(isSpace)
import Text.Parsec (ParseError, try, parse)
import Text.Parsec.String(Parser)
import Text.Parsec.Char (oneOf, digit, string, anyChar, char, letter, satisfy)
import Text.Parsec.Combinator (many1, manyTill, eof, choice, between, sepBy, optionMaybe)
import Control.Applicative (many, (<*), (<$>), (*>), (<|>), (<$), (<*>))
import qualified Text.Parsec.Expr as E

-- import qualified Test.HUnit as H

import Debug.Trace (trace)

type Operator = E.Operator String () Identity Expr

data Expr = StringLit       String         -- String literal

          | NumLit          Integer        -- Integer literal

          | Var             String         -- Variable name

          | Parens          Expr           -- Expression in parens

          | FuncApp         String         -- Function name
                            [Expr]         -- Parameter list

          | FuncDef         String         -- Function name
                            [Expr]         -- Parameter list
                            [Expr]         -- Function body

          | ModuleDef       String         -- Module name
                            [Expr]         -- Export list
                            [Expr]         -- Module body

          | IfElseStatement Expr           -- If   statement condition
                            [Expr]         -- If   statement body
                            (Maybe [Expr]) -- Else statement body

          | WhileStatement  Expr           -- While statement condition
                            [Expr]         -- While statement body
          deriving (Eq, Show)

tokenize :: String -> Either ParseError Expr
tokenize = parse (clear >> expr) ""

expr :: Parser Expr
expr = E.buildExpressionParser table term

term :: Parser Expr
term =  try (moduleDef expr)
    <|> try (funcDef expr)
    <|> ifElseStatement expr
    <|> whileStatement expr
    <|> try (funcApp expr)
    <|> numLit
    <|> stringLit
    <|> var
    <|> parensValue expr

table :: [[Operator]]
table = [ [ prefix "-", prefix "+"]
        , [ binary "*" E.AssocLeft
          , binary "/" E.AssocLeft]
        , [ binary "+" E.AssocLeft
          , binary "-" E.AssocLeft]
        , [ binary "<" E.AssocLeft
          , binary ">" E.AssocLeft
          , binary "==" E.AssocLeft]
        , [binary "&&" E.AssocLeft]
        , [binary "||" E.AssocLeft]
        , [binary "=" E.AssocLeft]]
  where
    binary     name assoc = E.Infix  (mkBinaryOp name <$ symbol  name) assoc
    binaryK    name assoc = E.Infix  (mkBinaryOp name <$ keyword name) assoc
    prefix     name       = E.Prefix (mkUnaryOp  name <$ symbol  name)
    prefixK    name       = E.Prefix (mkUnaryOp  name <$ keyword name)
    mkBinaryOp nm a b     = FuncApp nm [a, b]
    mkUnaryOp  nm a       = FuncApp nm [a]

lexeme :: Parser a -> Parser a
lexeme p = p <* clear

clear :: Parser ()
clear = choice [ whitespace   *> clear -- clear whitespace
               , lineComment  *> clear -- clear line comment (-- let a = 42)
               , blockComment *> clear -- clear block comment (/* let a = 42*/)
               , return ()]            -- bottom case
    where
        lineComment  =  try (string "--")
                     *> manyTill anyChar (void (char '\n') <|> eof)
        blockComment =  try (string "/*")
                     *> manyTill anyChar (try $ string "*/")
        whitespace   =  void $ many1 $ satisfy isSpace

integer :: Parser Integer
integer = read <$> lexeme (many1 digit)

numLit :: Parser Expr
numLit = NumLit <$> integer

stringToken :: Parser String
stringToken = lexeme ( char '\'' *> manyTill anyChar (char '\''))

stringLit :: Parser Expr
stringLit = StringLit <$> stringToken

iden :: Parser String
iden = lexeme ((:) <$> firstChar <*> many nonFirstChar)
    where
        firstChar    = letter <|> char '_'
        nonFirstChar = digit  <|> firstChar

var :: Parser Expr
var = Var <$> iden

parens :: Parser a -> Parser a
parens = between openParen closeParen
        where
            openParen  = (lexeme $ char '(')
            closeParen = (lexeme $ char ')')

parensValue :: Parser Expr -> Parser Expr
parensValue val = Parens <$> parens val

comma :: Parser Char
comma = lexeme $ char ','

commaSep :: Parser a -> Parser [a]
commaSep = (`sepBy` comma)

symbol :: String -> Parser String
symbol s = try $ lexeme $ do
    u <- many1 (oneOf "<>=+-^%/*!|")
    guard (s == u)
    return s

keyword :: String -> Parser String
keyword k = try $ do
    i <- iden
    guard (i == k)
    return k

funcApp :: Parser Expr -> Parser Expr
funcApp val = FuncApp <$> iden <*> parens (commaSep val)

funcDef :: Parser Expr -> Parser Expr
funcDef val =  keyword "fn"
            *> (FuncDef <$> iden
                        <*> parens (commaSep val)
                        <*> parens (commaSep val))

moduleDef :: Parser Expr -> Parser Expr
moduleDef val =  keyword "module"
              *> (ModuleDef <$> iden
                            <*> parens (commaSep val)
                            <*> parens (commaSep val))

ifElseStatement :: Parser Expr -> Parser Expr
ifElseStatement val =  keyword "if"
                    *> (IfElseStatement <$> parens val                             -- if statement condition
                                        <*> parens (commaSep val)                  -- if statement body
                                        <*> optionMaybe (  keyword "else"          -- optional else statement
                                                        *> parens (commaSep val)))

whileStatement :: Parser Expr -> Parser Expr
whileStatement val =  keyword "while"
                   *> (WhileStatement <$> parens val             -- while statement condition
                                      <*> parens (commaSep val)) -- while statement body