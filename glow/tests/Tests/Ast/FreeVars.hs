module Tests.Ast.FreeVars (tests) where

import Data.Set (Set)
import qualified Data.Set as Set
import Glow.Ast.Common
import Glow.Ast.LiftedFunctions
import Glow.Gerbil.Types (Pat (..))
import Glow.Prelude
import Test.Hspec

tests = describe "Free variables" $ do
  describe "Glow.Ast.Common" $ do
    describe "trivial expressions" $
      fvExamples
        trexFreeVars
        [ (TrexVar "x", ["x"]),
          (TrexConst (CBool True), [])
        ]
  describe "Glow.Ast.LiftedFunctions" $ do
    describe "expressions" $ do
      fvExamples
        exFreeVars
        [ ( ExTriv (TrexVar "x"),
            ["x"]
          ),
          ( ExDot (TrexVar "x") "y",
            ["x"]
          ),
          ( ExList [TrexVar "x", TrexVar "y", TrexConst (CBool True), TrexVar "z"],
            ["x", "y", "z"]
          ),
          ( ExTuple [TrexVar "x", TrexConst (CBool True), TrexVar "y"],
            ["x", "y"]
          ),
          ( ExEq (TrexVar "x") (TrexVar "y"),
            ["x", "y"]
          ),
          ( ExDigest [TrexVar "x"],
            ["x"]
          ),
          ( ExSign (TrexVar "x"),
            ["x"]
          ),
          ( ExCapture (TrexVar "f") [TrexVar "x", TrexVar "y"],
            ["f", "x", "y"]
          ),
          ( ExApp (TrexVar "f") [TrexVar "x", TrexVar "y"],
            ["f", "x", "y"]
          )
        ]
    describe "switch statements" $ do
      fvExamples (swMeta . switchWithFreeVars) $
        [ ( Switch () (TrexVar "x") [],
            ["x"]
          ),
          ( Switch () (TrexConst (CBool True)) $
              [ (PWild, [PsAssert () (TrexVar "x")])
              ],
            ["x"]
          ),
          ( Switch () (TrexConst (CBool True)) $
              [ (PVar "x", [PsAssert () (TrexVar "x")])
              ],
            []
          ),
          ( Switch () (TrexVar "x") $
              [ (PConst (CBool True), [PsAssert () (TrexVar "y")]),
                (PConst (CBool False), [PsAssert () (TrexVar "z")]),
                (PVar "w", [PsAssert () (TrexVar "w")])
              ],
            ["x", "y", "z"]
          )
        ]
    describe "PartStmts" $ do
      describe "Simple examples" $ do
        fvStmtsExamples
          [ ( [],
              []
            ),
            ( [PsLabel () "x"],
              []
            ),
            ( [PsDebugLabel () "x"],
              []
            ),
            ( [PsIgnore () (ExTriv (TrexVar "x"))],
              ["x"]
            ),
            ( [ PsIgnore () (ExTriv (TrexVar "x")),
                PsIgnore () (ExTriv (TrexVar "y"))
              ],
              ["x", "y"]
            ),
            ( [PsReturn () (ExTriv (TrexVar "x"))],
              ["x"]
            ),
            ( [PsRequire () (TrexVar "x")],
              ["x"]
            ),
            ( [PsAssert () (TrexVar "x")],
              ["x"]
            ),
            ( [PsSwitch () (Switch () (TrexVar "x") [])],
              ["x"]
            )
          ]
      describe "PsDef" $ do
        fvStmtsExamples
          [ ( [ PsDef () "x" (ExTriv (TrexVar "y")),
                PsIgnore () (ExTriv (TrexVar "x"))
              ],
              ["y"]
            ),
            ( [ PsDef () "x" (ExTriv (TrexConst (CBool True))),
                PsIgnore () (ExTriv (TrexVar "y"))
              ],
              ["y"]
            ),
            ( [ PsDef () "x" (ExTriv (TrexVar "y")),
                PsIgnore () (ExTriv (TrexVar "x")),
                PsIgnore () (ExTriv (TrexVar "z"))
              ],
              ["y", "z"]
            )
          ]
    describe "BodyStmts" $ do
      fvStmtsExamples
        [ ( [BsPublish () "x" "y"],
            ["x", "y"]
          )
        ]

fvStmtsExamples :: (IsStmt f, Show (f ())) => [([f ()], [Id])] -> Spec
fvStmtsExamples = fvExamples (stmtsFreeVars . stmtsWithFreeVars)

fvExamples :: Show a => (a -> Set Id) -> [(a, [Id])] -> Spec
fvExamples getFreeVars cases =
  for_ cases $ \(input, expected) ->
    it ("Should return the expected result for input " <> show input) $ do
      fvExample getFreeVars input expected

fvExample :: (a -> Set Id) -> a -> [Id] -> Expectation
fvExample getFreeVars item expected =
  getFreeVars item `shouldBe` Set.fromList expected
