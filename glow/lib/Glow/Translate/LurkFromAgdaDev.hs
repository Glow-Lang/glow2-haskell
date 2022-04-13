{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Glow.Translate.LurkFromAgdaDev where


import Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Glow.Prelude
import Text.SExpression as S 

import qualified Data.List as L

import qualified Glow.Ast.Targets.Lurk as O

import Prelude (read)

import Glow.Translate.LurkToSExpr

data AList a = ALn | ALc a (AList a)
  deriving (Show, Read, Eq, Ord, Functor)

toList :: AList a -> [a]
toList ALn = []
toList (ALc x xs) = x : (toList xs)

data Expr a
  = ExNil a
  | ExT a
  | ExIf a (Expr a) (Expr a) (Expr a)
  | ExLambda a (AList Symbol) (Expr a)
  | ExLet a (Let a)
  | ExLetRec a (Let a)
  | ExBinary a O.BinOp (Expr a) (Expr a)
  | ExUnary a O.UnaryOp (Expr a)
  | ExBegin a (AList (Expr a)) (Expr a)
  | ExCurrentEnv a
  | ExFieldElem a Int
  | -- | @eval@ with an optional environment
    ExEval a (Expr a) (Maybe (Expr a))
  | ExSymbol a Symbol
  | ExApply a (Expr a) (AList (Expr a))
  | ExQuote a SExpr
  | ExQuotedName a String  
  deriving (Show, Read, Eq)

-- | A let binding. Wether this is a @let@ or a @letrec@ depends on
-- in which constructor of 'Expr' it is contained.
data Let a = LetC (AList (Binding a)) (Expr a)
  deriving (Show, Read, Eq)

-- | A binding, as in a let expression.
data Binding a = BindingC a Symbol (Expr a)
  -- { bInfo :: a,
  --   bKey :: Symbol,
  --   bVal :: Expr a
  -- }
  deriving (Show, Read, Eq)

-- | A variable
data Symbol = SymbolC Text
  deriving (Show, Read, Eq, Ord)


data TU = TU
  deriving (Show, Read, Eq, Ord)


-- test2 = ExLambda TU (ALc (SymbolC "d1") (ALc (SymbolC "d2") ALn)) (ExBinary TU O.BOpCons (ExSymbol TU (SymbolC "d")) (ExSymbol TU (SymbolC "b")))


toOriginalS :: Symbol -> O.Symbol
toOriginalS (SymbolC x) = O.Symbol x

toOriginalB :: Binding a -> O.Binding a
toOriginalB (BindingC x y z) = O.Binding x (toOriginalS y) (toOriginal z)

toOriginalL :: Let a -> O.Let a
toOriginalL (LetC x y) = O.Let (fmap toOriginalB (toList x)) (toOriginal y)


toOriginal :: Expr a -> O.Expr a
toOriginal = \case
  ExT x -> O.ExT x
  ExNil x -> O.ExNil x
  ExIf x c t e -> O.ExIf x (toOriginal c) (toOriginal t) (toOriginal e)
  ExLambda x params body -> O.ExLambda x (fmap toOriginalS (toList params)) (toOriginal body)
  ExLet x l -> O.ExLet x (toOriginalL l)

  ExLetRec x l -> O.ExLetRec x (toOriginalL l)

  ExBinary x op l r -> O.ExBinary x op (toOriginal l) (toOriginal r)

  ExUnary x op arg -> O.ExUnary x op (toOriginal arg)
  ExBegin x exs ex -> O.ExBegin x (toList (fmap toOriginal exs)) (toOriginal ex)
  ExCurrentEnv x -> O.ExCurrentEnv x
  ExFieldElem x k -> O.ExFieldElem x k 
  ExEval x y z -> O.ExEval x (toOriginal y) (fmap toOriginal z)
  ExSymbol x sym -> O.ExSymbol x (toOriginalS sym)
  ExApply x f args -> O.ExApply x (toOriginal f) (fmap toOriginal (toList args))

  ExQuote x sexpr -> O.ExQuote x sexpr
  ExQuotedName x y -> O.ExQuote x (S.Atom y)

  -- _ -> undefined

render :: SExpr -> Text
render = \case
  S.Atom x -> pack x
  S.List x -> "(" <> (intercalate " " (render <$> x)) <> ")"
  _ -> "not implemented"
  --ConsList
  -- Number x -> pack (show x)
  

translateAndPrintLurkCode :: Expr TU -> IO ()
translateAndPrintLurkCode x = do
  let y = translateExpr (toOriginal x)
  putStrLn (unpack (render y))


pickFromAgdaFile :: String -> IO () 
pickFromAgdaFile valLabel = do
  af <- TIO.readFile "/Users/Marcin/glow/agda/Glow/Simple/Lurk/ControlFlowTrans.agda"
  case L.take 2 $ T.splitOn ("labeledValue[ \"" <> (T.pack valLabel) <> "\" ][") af of
    [ _ , x] -> do
       case L.take 2 $ T.splitOn "]labeledValueEnd" x of
          [ x' , _] -> do
             let z' = unpack $ replace "Expr." "" x'
             translateAndPrintLurkCode $ read z'
          y ->  putStrLn $ ("endFailed ") L.++ (show $ L.length y)
    y -> putStrLn $ show $ L.length y

readRawString :: IO () 
readRawString = do
  z <- loop ""
  let z' = unpack $ replace "Expr." "" $ pack z
  translateAndPrintLurkCode $ read z'

  where
     loop :: String -> IO String
     loop y = do
         x <- getLine
         if (x == "end")
         then (pure y)
         else loop (y L.++ "\n" L.++ x)



readFromAgdaProcess :: IO ()
readFromAgdaProcess = do
  loop
  pure ()

  where
    loop :: IO ()
    loop = do       
       x <- getLine
       if (L.isPrefixOf "(agda2-info-action \"*Normal Form*\"" x)
         then let z = L.drop (L.length ("(agda2-info-action \"*Normal Form*\" " :: String)) x
                  z' = L.take (L.length z - 5) z

              in do --putStrLn z'
                    translateAndPrintLurkCode $ read ((read z') :: String) 
         else putStrLn "skip.."
       loop




       -- ~/glow2-haskell/dist-newstyle/build/x86_64-osx/ghc-8.10.6/glow-0.1.0.0/x/lurk-from-agda/build/lurk-from-agda/lurk-from-agda


-- echo "IOTCM \"IOHaskellInterface.agda\" None Indirect (Cmd_compute_toplevel DefaultCompute \"test2\")" | agda --interaction | ~/glow2-haskell/dist-newstyle/build/x86_64-osx/ghc-8.10.6/glow-0.1.0.0/x/lurk-from-agda/build/lurk-from-agda/lurk-from-agda


-- ExLambda TU
-- (ALc (SymbolC "wagerAmount") (ALc (SymbolC "escrowAmount") ALn))
-- (ExApply TU (ExSymbol TU (SymbolC "bind"))
--  (ALc
--   (ExApply TU (ExSymbol TU (SymbolC "publish"))
--    (ALc (ExFieldElem TU 0) (ALc (ExFieldElem TU 0) ALn)))
--   (ALc
--    (ExLambda TU (ALc (SymbolC "commitment") ALn)
--     (ExApply TU (ExSymbol TU (SymbolC "next"))
--      (ALc
--       (ExApply TU (ExSymbol TU (SymbolC "action"))
--        (ALc (ExFieldElem TU 1)
--         (ALc (ExFieldElem TU 0)
--          (ALc
--           (ExApply TU (ExSymbol TU (SymbolC "deposit"))
--            (ALc
--             (ExApply TU (ExSymbol TU (SymbolC "+ℕ"))
--              (ALc (ExSymbol TU (SymbolC "wagerAmount"))
--               (ALc (ExSymbol TU (SymbolC "escrowAmount")) ALn)))
--             ALn))
--           ALn))))
--       (ALc
--        (ExApply TU (ExSymbol TU (SymbolC "bind"))
--         (ALc
--          (ExApply TU (ExSymbol TU (SymbolC "publish"))
--           (ALc (ExFieldElem TU 2) (ALc (ExFieldElem TU 1) ALn)))
--          (ALc
--           (ExLambda TU (ALc (SymbolC "randB") ALn)
--            (ExApply TU (ExSymbol TU (SymbolC "next"))
--             (ALc
--              (ExApply TU (ExSymbol TU (SymbolC "action"))
--               (ALc (ExFieldElem TU 3)
--                (ALc (ExFieldElem TU 1)
--                 (ALc
--                  (ExApply TU (ExSymbol TU (SymbolC "deposit"))
--                   (ALc (ExSymbol TU (SymbolC "wagerAmount")) ALn))
--                  ALn))))
--              (ALc
--               (ExApply TU (ExSymbol TU (SymbolC "bind"))
--                (ALc
--                 (ExApply TU (ExSymbol TU (SymbolC "publish"))
--                  (ALc (ExFieldElem TU 4) (ALc (ExFieldElem TU 0) ALn)))
--                 (ALc
--                  (ExLambda TU (ALc (SymbolC "randA") ALn)
--                   (ExApply TU (ExSymbol TU (SymbolC "bind"))
--                    (ALc
--                     (ExApply TU (ExSymbol TU (SymbolC "mk-pure"))
--                      (ALc
--                       (ExApply TU (ExSymbol TU (SymbolC "digestNat"))
--                        (ALc (ExSymbol TU (SymbolC "randA")) ALn))
--                       ALn))
--                     (ALc
--                      (ExLambda TU (ALc (SymbolC "mbCommitment") ALn)
--                       (ExApply TU (ExSymbol TU (SymbolC "next"))
--                        (ALc
--                         (ExApply TU (ExSymbol TU (SymbolC "require"))
--                          (ALc
--                           (ExApply TU (ExSymbol TU (SymbolC "==Digest"))
--                            (ALc (ExSymbol TU (SymbolC "commitment"))
--                             (ALc (ExSymbol TU (SymbolC "mbCommitment")) ALn)))
--                           ALn))
--                         (ALc
--                          (ExApply TU (ExSymbol TU (SymbolC "bind"))
--                           (ALc
--                            (ExApply TU (ExSymbol TU (SymbolC "mk-pure"))
--                             (ALc
--                              (ExApply TU (ExSymbol TU (SymbolC "^^^"))
--                               (ALc (ExSymbol TU (SymbolC "randA"))
--                                (ALc (ExSymbol TU (SymbolC "randB")) ALn)))
--                              ALn))
--                            (ALc
--                             (ExLambda TU (ALc (SymbolC "n0") ALn)
--                              (ExApply TU (ExSymbol TU (SymbolC "bind"))
--                               (ALc
--                                (ExApply TU (ExSymbol TU (SymbolC "mk-pure"))
--                                 (ALc
--                                  (ExApply TU (ExSymbol TU (SymbolC "&&&"))
--                                   (ALc (ExSymbol TU (SymbolC "n0"))
--                                    (ALc
--                                     (ExApply TU (ExSymbol TU (SymbolC "cons"))
--                                      (ALc (ExT TU) (ALc (ExNil TU) ALn)))
--                                     ALn)))
--                                  ALn))
--                                (ALc
--                                 (ExLambda TU (ALc (SymbolC "n1") ALn)
--                                  (ExApply TU (ExSymbol TU (SymbolC "next"))
--                                   (ALc
--                                    (ExApply TU (ExSymbol TU (SymbolC "next"))
--                                     (ALc
--                                      (ExIf TU
--                                       (ExApply TU (ExSymbol TU (SymbolC "==Nat"))
--                                        (ALc (ExSymbol TU (SymbolC "n1"))
--                                         (ALc (ExNil TU) ALn)))
--                                       (ExApply TU (ExSymbol TU (SymbolC "bind"))
--                                        (ALc
--                                         (ExApply TU (ExSymbol TU (SymbolC "mk-pure"))
--                                          (ALc
--                                           (ExApply TU (ExSymbol TU (SymbolC "*ℕ"))
--                                            (ALc
--                                             (ExApply TU (ExSymbol TU (SymbolC "cons"))
--                                              (ALc (ExNil TU)
--                                               (ALc
--                                                (ExApply TU (ExSymbol TU (SymbolC "cons"))
--                                                 (ALc (ExT TU) (ALc (ExNil TU) ALn)))
--                                                ALn)))
--                                             (ALc (ExSymbol TU (SymbolC "wagerAmount")) ALn)))
--                                           ALn))
--                                         (ALc
--                                          (ExLambda TU (ALc (SymbolC "w1") ALn)
--                                           (ExApply TU (ExSymbol TU (SymbolC "bind"))
--                                            (ALc
--                                             (ExApply TU (ExSymbol TU (SymbolC "mk-pure"))
--                                              (ALc
--                                               (ExApply TU (ExSymbol TU (SymbolC "+ℕ"))
--                                                (ALc (ExSymbol TU (SymbolC "w1"))
--                                                 (ALc (ExSymbol TU (SymbolC "escrowAmount"))
--                                                  ALn)))
--                                               ALn))
--                                             (ALc
--                                              (ExLambda TU (ALc (SymbolC "w2") ALn)
--                                               (ExApply TU (ExSymbol TU (SymbolC "next"))
--                                                (ALc
--                                                 (ExApply TU
--                                                  (ExSymbol TU (SymbolC "action"))
--                                                  (ALc (ExFieldElem TU 5)
--                                                   (ALc (ExFieldElem TU 0)
--                                                    (ALc
--                                                     (ExApply TU
--                                                      (ExSymbol TU (SymbolC "withdraw"))
--                                                      (ALc (ExSymbol TU (SymbolC "w2")) ALn))
--                                                     ALn))))
--                                                 (ALc
--                                                  (ExApply TU
--                                                   (ExSymbol TU (SymbolC "mk-pure"))
--                                                   (ALc (ExQuotedName TU "glow-unit-lit") ALn))
--                                                  ALn))))
--                                              ALn))))
--                                          ALn)))
--                                       (ExApply TU (ExSymbol TU (SymbolC "bind"))
--                                        (ALc
--                                         (ExApply TU (ExSymbol TU (SymbolC "mk-pure"))
--                                          (ALc
--                                           (ExApply TU (ExSymbol TU (SymbolC "*ℕ"))
--                                            (ALc
--                                             (ExApply TU (ExSymbol TU (SymbolC "cons"))
--                                              (ALc (ExNil TU)
--                                               (ALc
--                                                (ExApply TU (ExSymbol TU (SymbolC "cons"))
--                                                 (ALc (ExT TU) (ALc (ExNil TU) ALn)))
--                                                ALn)))
--                                             (ALc (ExSymbol TU (SymbolC "wagerAmount")) ALn)))
--                                           ALn))
--                                         (ALc
--                                          (ExLambda TU (ALc (SymbolC "w1") ALn)
--                                           (ExApply TU (ExSymbol TU (SymbolC "next"))
--                                            (ALc
--                                             (ExApply TU (ExSymbol TU (SymbolC "action"))
--                                              (ALc (ExFieldElem TU 6)
--                                               (ALc (ExFieldElem TU 1)
--                                                (ALc
--                                                 (ExApply TU
--                                                  (ExSymbol TU (SymbolC "withdraw"))
--                                                  (ALc (ExSymbol TU (SymbolC "w1")) ALn))
--                                                 ALn))))
--                                             (ALc
--                                              (ExApply TU (ExSymbol TU (SymbolC "next"))
--                                               (ALc
--                                                (ExApply TU
--                                                 (ExSymbol TU (SymbolC "action"))
--                                                 (ALc (ExFieldElem TU 7)
--                                                  (ALc (ExFieldElem TU 0)
--                                                   (ALc
--                                                    (ExApply TU
--                                                     (ExSymbol TU (SymbolC "withdraw"))
--                                                     (ALc (ExSymbol TU (SymbolC "escrowAmount"))
--                                                      ALn))
--                                                    ALn))))
--                                                (ALc
--                                                 (ExApply TU
--                                                  (ExSymbol TU (SymbolC "mk-pure"))
--                                                  (ALc (ExQuotedName TU "glow-unit-lit") ALn))
--                                                 ALn)))
--                                              ALn))))
--                                          ALn))))
--                                      (ALc
--                                       (ExApply TU (ExSymbol TU (SymbolC "mk-pure"))
--                                        (ALc (ExQuotedName TU "glow-unit-lit") ALn))
--                                       ALn)))
--                                    (ALc
--                                     (ExApply TU (ExSymbol TU (SymbolC "mk-pure"))
--                                      (ALc (ExQuotedName TU "glow-unit-lit") ALn))
--                                     ALn))))
--                                 ALn))))
--                             ALn)))
--                          ALn))))
--                      ALn))))
--                  ALn)))
--               ALn))))
--           ALn)))
--        ALn))))
--    ALn)))
