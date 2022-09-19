module Tests.FunctionLift where

import Control.Monad.State (evalState, execState)
import qualified Data.Map.Strict as Map
import Glow.Ast.Common
import Glow.Ast.LiftedFunctions
import Glow.Gerbil.Fresh
import qualified Glow.Gerbil.Types as GGT
import Glow.Prelude
import Glow.Translate.FunctionLift
import Test.Hspec

tests = describe "Glow.Translate.FunctionLift" $ do
  it ("Should translate trivial statements") $
    do
      evalState
        ( functionLift
            [GGT.Define "x" (GGT.TrvExpr (TrexConst (cInteger 3)))]
        )
        Map.empty
      `shouldBe` Module
        [TsBodyStmt (BsPartStmt () Nothing (PsDef () "x" (ExTriv (TrexConst (cInteger 3)))))]
  it ("Should lift already-closed functions to the top without adding capture parameters") $
    do
      evalState
        ( functionLift
            [ GGT.DefineFunction
                "sumsqr"
                ["a", "b"]
                [ GGT.DefineFunction "sqr" ["x"] [GGT.Return (GGT.AppExpr (TrexVar "*") [TrexVar "x", TrexVar "x"])],
                  GGT.Define "tmp" (GGT.AppExpr (TrexVar "sqr") [TrexVar "a"]),
                  GGT.Define "tmp0" (GGT.AppExpr (TrexVar "sqr") [TrexVar "b"]),
                  GGT.Return (GGT.AppExpr (TrexVar "+") [TrexVar "tmp", TrexVar "tmp0"])
                ]
            ]
        )
        Map.empty
      `shouldBe` Module
        [ TsDefLambda () Nothing "sqr" (Lambda [] ["x"] [BsPartStmt () Nothing (PsReturn () (ExApp (TrexVar "*") [TrexVar "x", TrexVar "x"]))]),
          TsDefLambda
            ()
            Nothing
            "sumsqr"
            ( Lambda
                []
                ["a", "b"]
                [ BsPartStmt () Nothing (PsDef () "tmp" (ExApp (TrexVar "sqr") [TrexVar "a"])),
                  BsPartStmt () Nothing (PsDef () "tmp0" (ExApp (TrexVar "sqr") [TrexVar "b"])),
                  BsPartStmt () Nothing (PsReturn () (ExApp (TrexVar "+") [TrexVar "tmp", TrexVar "tmp0"]))
                ]
            )
        ]
  it ("Should lift closures with their capture parameters") $
    do
      evalState
        ( functionLift
            [ GGT.DefineFunction
                "adder"
                ["x"]
                [ GGT.DefineFunction "add-x" ["y"] [GGT.Return (GGT.AppExpr (TrexVar "+") [TrexVar "x", TrexVar "y"])],
                  GGT.Return (GGT.TrvExpr (TrexVar "add-x"))
                ]
            ]
        )
        (execState (traverse fresh ["adder", "x", "add-x", "y", "+"]) Map.empty)
      `shouldBe` Module
        [ TsDefLambda () Nothing "add-x0" (Lambda ["x"] ["y"] [BsPartStmt () Nothing (PsReturn () (ExApp (TrexVar "+") [TrexVar "x", TrexVar "y"]))]),
          TsDefLambda
            ()
            Nothing
            "adder"
            ( Lambda
                []
                ["x"]
                [ BsPartStmt () Nothing (PsDef () "add-x" (ExCapture (TrexVar "add-x0") [TrexVar "x"])),
                  BsPartStmt () Nothing (PsReturn () (ExTriv (TrexVar "add-x")))
                ]
            )
        ]
  it ("Should pass on asset_swap.glow") $
    do
      evalState
        ( functionLift
            [ GGT.DefineInteraction
                "swap"
                ( GGT.AnfInteractionDef
                    ["A", "B"]
                    ["T", "U"]
                    ["t", "u"]
                    [ GGT.Deposit "A" (Map.fromList [("T", TrexVar "t")]),
                      GGT.DebugLabel "dlb1",
                      GGT.Deposit "B" (Map.fromList [("U", TrexVar "u")]),
                      GGT.DebugLabel "dlb2",
                      GGT.Withdraw "B" (Map.fromList [("T", TrexVar "t")]),
                      GGT.DebugLabel "dlb3",
                      GGT.Withdraw "A" (Map.fromList [("U", TrexVar "u")]),
                      GGT.Return (GGT.TrvExpr (TrexConst CUnit))
                    ]
                )
            ]
        )
        (execState (traverse fresh ["swap", "A", "B", "T", "U", "t", "u", "dlb1", "dlb2", "dlb3"]) Map.empty)
      `shouldBe` Module
        [ TsDefInteraction
            ()
            "swap"
            ( InteractionDef
                ["A", "B"]
                ["T", "U"]
                ["t", "u"]
                [ BsDeposit () "A" (Map.fromList [("T", TrexVar "t")]),
                  BsDeposit () "B" (Map.fromList [("U", TrexVar "u")]),
                  BsWithdraw () "B" (Map.fromList [("T", TrexVar "t")]),
                  BsWithdraw () "A" (Map.fromList [("U", TrexVar "u")]),
                  BsPartStmt () Nothing (PsReturn () (ExTriv (TrexConst CUnit)))
                ]
            )
        ]
  it ("Should pass on buy_sig") $
    do
      evalState
        ( functionLift
            [ GGT.DefineInteraction
                "buySig"
                ( GGT.AnfInteractionDef
                    ["Buyer", "Seller"]
                    ["DefaultToken"]
                    ["digest0", "price"]
                    [ GGT.Deposit "Buyer" (Map.fromList [("DefaultToken", TrexVar "price")]),
                      GGT.DebugLabel "dlb1",
                      GGT.AtParticipant
                        "Seller"
                        (GGT.Define "signature" (GGT.Sign (TrexVar "digest0"))),
                      GGT.Publish "Seller" ["signature"],
                      GGT.Define "tmp" (GGT.AppExpr (TrexVar "isValidSignature") [TrexVar "Seller", TrexVar "digest0", TrexVar "signature"]),
                      GGT.Require (TrexVar "tmp"),
                      GGT.DebugLabel "dlb2",
                      GGT.Withdraw "Seller" (Map.fromList [("DefaultToken", TrexVar "price")]),
                      GGT.Return (GGT.TrvExpr (TrexConst CUnit))
                    ]
                )
            ]
        )
        (execState (traverse fresh ["buySig", "Buyer", "Seller", "DefaultToken", "digest0", "price", "dlb1", "signature", "tmp", "isValidSignature", "dlb2"]) Map.empty)
      `shouldBe` Module
        [ TsDefInteraction
            ()
            "buySig"
            ( InteractionDef
                ["Buyer", "Seller"]
                ["DefaultToken"]
                ["digest0", "price"]
                [ BsDeposit () "Buyer" (Map.fromList [("DefaultToken", TrexVar "price")]),
                  BsPartStmt () (Just "Seller") (PsDef () "signature" (ExSign (TrexVar "digest0"))),
                  BsPublish () "Seller" "signature",
                  BsPartStmt () Nothing (PsDef () "tmp" (ExApp (TrexVar "isValidSignature") [TrexVar "Seller", TrexVar "digest0", TrexVar "signature"])),
                  BsPartStmt () Nothing (PsRequire () (TrexVar "tmp")),
                  BsWithdraw () "Seller" (Map.fromList [("DefaultToken", TrexVar "price")]),
                  BsPartStmt () Nothing (PsReturn () (ExTriv (TrexConst CUnit)))
                ]
            )
        ]
