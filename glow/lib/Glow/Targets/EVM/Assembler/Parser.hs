module Glow.Targets.EVM.Assembler.Parser where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Data.Void (Void)
import Glow.Prelude hiding (many)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (read)

data Op
  = OpPush Int Expr
  | OpSuffix Text Int
  | OpOther Text
  deriving (Show, Read, Eq)

data Item
  = IOp Op
  | ILabel Text
  | IDirective Text [Expr]
  deriving (Show, Read, Eq)

data Expr
  = EInt Integer
  | ELabel Text
  deriving (Show, Read, Eq)

type Parser = Parsec Void Text

whitespace :: Parser ()
whitespace =
  L.space
    space1
    (L.skipLineComment "#")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

file :: Parser [Item]
file = many item

item :: Parser Item
item =
  choice
    [ try directive,
      try $ ILabel <$> ident <* lexeme (char ':'),
      IOp <$> op
    ]

directive :: Parser Item
directive =
  choice $
    (IDirective <$> directivePrefix "text" <*> pure []) :
      [ IDirective <$> directivePrefix p <*> args
        | p <- ["file", "globl", "type", "size"]
      ]
  where
    args = expr `sepBy1` lexeme (string ",")

directivePrefix :: Text -> Parser Text
directivePrefix prefix =
  lexeme (string ("." <> prefix)) *> pure prefix

-- identifiers
ident :: Parser Text
ident = lexeme $ do
  start <- letterChar <|> oneOf specials
  rest <- takeWhileP Nothing $ \c ->
    isAlphaNum c || c `elem` specials
  pure $ LT.pack [start] <> rest
  where
    specials :: [Char]
    specials = "_.$"

nat :: Parser Integer
nat = lexeme $ read . LT.unpack <$> takeWhile1P Nothing isDigit

expr :: Parser Expr
expr =
  choice
    [ EInt <$> nat,
      ELabel <$> ident
    ]

op :: Parser Op
op =
  choice
    [ OpPush <$> opSuffix "PUSH" <*> expr,
      opWithPrefix "DUP",
      opWithPrefix "SWAP",
      opWithPrefix "LOG",
      OpOther <$> ident
    ]

opWithPrefix :: Text -> Parser Op
opWithPrefix prefix =
  OpSuffix prefix <$> opSuffix prefix

opSuffix :: Text -> Parser Int
opSuffix prefix = do
  ds <- lexeme $ do
    void $ string prefix
    takeWhile1P Nothing isDigit
  pure $ read $ LT.unpack ds
