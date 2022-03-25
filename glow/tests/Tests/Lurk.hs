module Tests.Lurk where

import Glow.Ast.Targets.Lurk
import Glow.Prelude
import Glow.Translate.LurkToSExpr
import Test.Hspec
import Text.SExpression (SExpr (Atom, List))

tests = describe "Glow.Translate.LurkToSExpr" $ do
  describe "exmaples" $ do
    for_ examples $ \(input, want) ->
      it ("Should generate the correct ouptut input " <> show input) $ do
        translateExpr input `shouldBe` want

examples :: [(Expr (), SExpr)]
examples =
  [ (ExT (), Atom "t"),
    (ExNil (), Atom "nil"),
    ( ExIf () (ExT ()) (ExNil ()) (ExSymbol () "sym"),
      List [Atom "if", Atom "t", Atom "nil", Atom "sym"]
    ),
    ( ExLet () $
        Let
          [ Binding () "a" (ExBinary () BOpPlus (ExSymbol () "x") (ExSymbol () "y")),
            Binding () "b" (ExUnary () UOpCar (ExSymbol () "xyz"))
          ]
          ( ExBegin
              ()
              [ (ExUnary () UOpEmit (ExSymbol () "abc")),
                (ExUnary () UOpCdr (ExSymbol () "def"))
              ]
              (ExLambda () ["arg"] (ExSymbol () "result"))
          ),
      List
        [ Atom "let",
          List
            [ List [Atom "a", List [Atom "+", Atom "x", Atom "y"]],
              List [Atom "b", List [Atom "car", Atom "xyz"]]
            ],
          List
            [ Atom "begin",
              List [Atom "emit", Atom "abc"],
              List [Atom "cdr", Atom "def"],
              List [Atom "lambda", List [Atom "arg"], Atom "result"]
            ]
        ]
    )
  ]
