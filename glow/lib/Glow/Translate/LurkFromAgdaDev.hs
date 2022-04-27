{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Glow.Translate.LurkFromAgdaDev where


import Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Glow.Prelude
import Text.SExpression as S 

import qualified Data.List as L

import qualified Glow.Ast.Targets.Lurk as O

import Prelude (read)

import Text.Read (readMaybe)

import Glow.Translate.LurkToSExpr
import Control.Lens (makeLenses)


type ParticipantId = Int

data GLType = GLNatT | GLBoolT | GLPFT | DigestT | GLUnitT 
  deriving (Show , Read , Eq)


data GLValue = GLNat Int | GLBool Bool | GLPF Int | DigestOf GLValue | GLUnit 
  deriving (Show , Read , Eq)

prettyGLValue :: GLValue -> String 
prettyGLValue = \case
  GLNat k -> show k
  GLBool k -> show k
  GLPF k -> show k L.++ "(PF)"
  DigestOf x -> "dig(" L.++ (prettyGLValue x) L.++  ")" 
  GLUnit -> "()" 


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


data Let a = LetC (AList (Binding a)) (Expr a)
  deriving (Show, Read, Eq)

-- | A binding, as in a let expression.
data Binding a = BindingC a Symbol (Expr a)
  deriving (Show, Read, Eq)

-- | A variable
data Symbol = SymbolC Text
  deriving (Show, Read, Eq, Ord)


data TU = TU
  deriving (Show, Read, Eq, Ord)


data GLContainerC = GLContainerC (AList String) (Expr TU) (AList (String , String))
  deriving (Show, Read, Eq)

data GLContainer = GLContainer ([String]) FilePath ([(String , GLType)])
  deriving (Show, Read, Eq)



data Action =
    Withdraw Int
  | Deposit Int
  | Publish GLValue
 deriving (Show , Read)


data CallC = CallC
  Int
  ParticipantId
  Action
  
 deriving (Show , Read)

data Call = Call
  { _desiredStateId :: Int
  , _caller :: ParticipantId
  , _action :: Action
  }
 deriving (Show , Read)
makeLenses ''Call



data GLInteractionTestC =
   GLInteractionTestC GLContainerC (AList (String , GLValue)) (AList CallC)
   deriving (Show, Read)

data GLInteractionTest =
   GLInteractionTest
   { _contract :: GLContainer
   , _parameters :: [(String , GLValue)]
   , _calls :: [Call]
   }
   deriving (Show, Read)
makeLenses ''GLInteractionTest

fromGLInteractionTestC :: GLInteractionTestC -> GLInteractionTest
fromGLInteractionTestC = undefined


toOriginalS :: Symbol -> O.Symbol
toOriginalS (SymbolC x) = O.Symbol x

toOriginalB :: Binding a -> O.Binding a
toOriginalB (BindingC x y z) = O.Binding x (toOriginalS y) (toOriginal z)

toOriginalL :: Let a -> O.Let a
toOriginalL (LetC x y) = O.Let (fmap toOriginalB (toList x)) (toOriginal y)


interationLibHead :: String
interationLibHead = "(let ((glow-code "

interationLibFooter :: String
interationLibFooter = "))(current-env))"



-- TODO :: intorduce relevant Typeclass, to avoid repettion here
fromRawContainer :: FilePath -> GLContainerC -> IO GLContainer
fromRawContainer fName (GLContainerC x y z) = do
   writeFile fName $ interationLibHead L.++ (render (translateExpr (toOriginal y))) L.++ interationLibFooter 
   pure $ GLContainer
     (toList x)
     fName
     (L.map (\(nm , tS ) ->
             (nm , case tS of
                        "Nat" -> GLNatT
                        "Bool" -> GLBoolT
                        _ -> GLUnitT

             ))
       (toList z))  

fromRawInteractionTest :: FilePath -> GLInteractionTestC -> IO GLInteractionTest
fromRawInteractionTest fName (GLInteractionTestC x y z) = do
  c <- fromRawContainer fName x
  pure $
    GLInteractionTest c (toList y) (fmap (\(CallC x' y' z') -> Call x' y' z') $ toList z) 
    
  

  
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

render :: SExpr -> String
render = \case
  S.Atom x -> x
  S.List x -> "(" L.++ (L.intercalate " " (render <$> x)) L.++ ")"
  _ -> "not implemented"
  --ConsList
  -- Number x -> pack (show x)
  

translateAndPrintLurkCode :: Expr TU -> IO ()
translateAndPrintLurkCode x = do
  let y = translateExpr (toOriginal x)
  putStrLn (render y)


pickContainerFromAgdaFile :: String -> IO  (GLContainer)
pickContainerFromAgdaFile valLabel = do
  af <- TIO.readFile "/Users/Marcin/glow/agda/Glow/Simple/Lurk/ControlFlowTrans.agda"
  case L.take 2 $ T.splitOn ("labeledValue[ \"" <> (T.pack valLabel) <> "\" ][") af of
    [ _ , x] -> do
       case L.take 2 $ T.splitOn "]labeledValueEnd" x of
          [ x' , _] -> do
             let z' = unpack $ replace "Expr." "" x'
             case readMaybe z' of
               Nothing -> do
                    putStrLn $ z'
                    error "error: Malformed Expression!"
                             
               Just xx -> (fromRawContainer ("/Users/Marcin/lurk-code/" L.++ valLabel L.++ ".lurk") xx)
          y ->  error $ ("endFailed ") L.++ (show $ L.length y)
                  
    y -> error $ show $ L.length y

pickTestCaseFromAgdaFile :: String -> IO  (GLInteractionTest)
pickTestCaseFromAgdaFile valLabel = do
  af <- TIO.readFile "/Users/Marcin/glow/agda/Glow/Simple/Lurk/ControlFlowTrans.agda"
  case L.take 2 $ T.splitOn ("labeledValue[ \"" <> (T.pack valLabel) <> "\" ][") af of
    [ _ , x] -> do
       case L.take 2 $ T.splitOn "]labeledValueEnd" x of
          [ x' , _] -> do
             let z' = unpack $ replace "Expr." "" x'
             case readMaybe z' of
               Nothing -> do
                    putStrLn $ z'
                    error "error: Malformed GLInteractionTest Expression!"
                             
               Just xx -> (fromRawInteractionTest ("/Users/Marcin/lurk-code/" L.++ valLabel L.++ ".lurk") xx)
          y ->  error $ ("endFailed ") L.++ (show $ L.length y)
                  
    y -> error $ show $ L.length y



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

