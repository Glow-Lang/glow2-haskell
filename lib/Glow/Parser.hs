module Glow.Parser where

import Glow.Ast
import Glow.Prelude hiding (many)

import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as LT
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

whitespace :: Parser ()
whitespace = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

symbol :: Text -> Parser Text
symbol = L.symbol whitespace

-- identifiers
ident :: Parser Symbol
ident = lexeme $ do
    start <- letterChar
    rest <- takeWhileP Nothing $ \c ->
        isAlphaNum c || c `elem` ("_$!" :: [Char])
    pure $ Symbol $ LT.pack [start] <> rest

-- booleans
bool :: Parser Bool
bool =
    (True <$ symbol "true") <|>
    (False <$ symbol "false")

-- Expressions
expr :: Parser Expr
expr = choice
    [ recordExpr <?> "record"
    , lambdaExpr <?> "lambda"
    ] <?> "expression"

lambdaExpr :: Parser Expr
lambdaExpr = ExLambda <$> lambda

lambda :: Parser Function
lambda = do
    params <- paramList
    -- TODO: optional return type annotation.
    symbol "=>"
    (bodyStmts, bodyExpr) <- fnBody
    pure Function
        { fParams = params
        , fBodyStmts = bodyStmts
        , fBodyExpr = bodyExpr
        }

fnBody :: Parser ([Stmt], Expr)
fnBody = do
    -- Parsing function bodies is finnicky; we expect a series of
    -- statements (terminated with ;) followed by an expression.
    -- But an expression is also always a syntactically valid
    -- statement. We have to go to the end of the body before
    -- we know which we should parse it as. Rather than just
    -- backtrack and having to re-parse it as an expression,
    -- we hang on to the already parsed expression so if
    -- parsing more statements fails we just return it as-is.
    s <- stmt
    let rest = try $ do
            symbol ";"
            (ss, e) <- fnBody
            pure (s:ss, e)
    case s of
        StExpr e -> rest <|> pure ([], e)
        _        -> rest


paramList :: Parser [Param]
paramList = between (symbol "(") (symbol ")") (fnParam `sepBy` symbol ",")

fnParam :: Parser Param
fnParam = ident

recordExpr :: Parser Expr
recordExpr = ExRecord <$>
    between
        (symbol "{")
        (symbol "}")
        (recordExprEntry `sepBy` symbol ",")

recordExprEntry :: Parser (Symbol, Expr)
recordExprEntry = do
    id <- ident
    symbol ":"
    ex <- expr
    pure (id, ex)

-- Statements
stmt :: Parser Stmt
stmt = choice
    [ letStmt
    , exprStmt
    ] <?> "statement"

letStmt :: Parser Stmt
letStmt = do
    symbol "let"
    id <- ident
    symbol "="
    val <- expr
    pure $ StLet id val

exprStmt :: Parser Stmt
exprStmt = StExpr <$> expr

stmtList :: Parser [Stmt]
stmtList = stmt `sepEndBy` symbol ";"

-- Entire source file
program :: Parser Program
program =
    -- TODO: permit "#lang glow" at the top.
    whitespace *> stmtList <* eof
