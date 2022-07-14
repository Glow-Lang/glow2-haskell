{-# LANGUAGE LambdaCase #-}

-- | Translate lurk ASTs into s-expression form.
module Glow.Translate.LurkToSExpr where

import qualified Data.Text.Lazy as LT
import Glow.Ast.Targets.Lurk
import Glow.Prelude
import qualified Text.SExpression as S

translateExpr :: Expr a -> S.SExpr
translateExpr = \case
  ExT _ -> S.Atom "t"
  ExNil _ -> S.Atom "nil"
  ExIf _ c t e ->
    S.List [S.Atom "if", translateExpr c, translateExpr t, translateExpr e]
  ExLambda _ params body ->
    S.List [S.Atom "lambda", S.List (map translateSymbol params), translateExpr body]
  ExLet _ l ->
    S.List (S.Atom "let" : translateLet l)
  ExLetRec _ l ->
    S.List (S.Atom "letrec" : translateLet l)
  ExBinary _ op l r ->
    S.List [S.Atom (translateBinOp op), translateExpr l, translateExpr r]
  ExUnary _ op arg ->
    S.List [S.Atom (translateUnaryOp op), translateExpr arg]
  ExBegin _ exs ex ->
    S.List (S.Atom "begin" : map translateExpr (exs <> [ex]))
  ExCurrentEnv _ ->
    S.List [S.Atom "current-env"]
  ExFieldElem _ k ->
    S.Atom (show k)
  ExEval _ exp Nothing ->
    S.List [S.Atom "eval", translateExpr exp]
  ExEval _ exp (Just env) ->
    S.List [S.Atom "eval", translateExpr exp, translateExpr env]
  ExSymbol _ sym ->
    translateSymbol sym
  ExApply _ f args ->
    S.List $ map translateExpr (f : args)
  ExQuote _ sexpr ->
    S.List [S.Atom "quote", sexpr]
  ExString _ s -> S.Atom (show s)

translateSymbol :: Symbol -> S.SExpr
translateSymbol (Symbol txt) =
  S.Atom (LT.unpack txt)

translateBinOp :: BinOp -> String
translateBinOp = \case
  BOpCons -> "cons"
  BOpPlus -> "+"
  BOpMinus -> "-"
  BOpTimes -> "*"
  BOpDiv -> "/"
  BOpNumEq -> "="
  BOpPtrEq -> "eq"

translateUnaryOp :: UnaryOp -> String
translateUnaryOp = \case
  UOpCar -> "car"
  UOpCdr -> "cdr"
  UOpEmit -> "emit"

translateLet :: Let a -> [S.SExpr]
translateLet l =
  [ S.List $ map translateBinding (letBindings l),
    translateExpr $ letBody l
  ]

translateBinding :: Binding a -> S.SExpr
translateBinding b =
  S.List
    [ translateSymbol (bKey b),
      translateExpr (bVal b)
    ]
