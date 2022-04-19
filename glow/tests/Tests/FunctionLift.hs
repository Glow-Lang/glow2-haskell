module Tests.FunctionLift where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Glow.Ast.Common
import Glow.Ast.LiftedFunctions
import Glow.Gerbil.Fresh
import qualified Glow.Gerbil.Types as GGT
import Glow.Translate.FunctionLift
import Glow.Prelude
import Test.Hspec

tests = describe "Glow.Translate.FunctionLift" $ do
  it ("Should translate trivial statements") $ do
    evalState
      (liftTopStmt Nothing []
        (GGT.Define "x" (GGT.TrvExpr (GGT.Explicit (GGT.Integer 3)))))
      Map.empty
    `shouldBe`
      [TsBodyStmt (BsPartStmt Nothing (PsDef "x" (ExArg (AEConst (cInteger 3)))))]
  it ("Should lift already-closed functions to the top without adding capture parameters") $ do
    evalState
      (liftTopStmt Nothing []
        (GGT.DefineFunction "sumsqr" ["a", "b"]
          [GGT.DefineFunction "sqr" ["x"] [GGT.Return (GGT.AppExpr (GGT.Variable "*") [GGT.Variable "x", GGT.Variable "x"])],
           GGT.Define "tmp" (GGT.AppExpr (GGT.Variable "sqr") [GGT.Variable "a"]),
           GGT.Define "tmp0" (GGT.AppExpr (GGT.Variable "sqr") [GGT.Variable "b"]),
           GGT.Return (GGT.AppExpr (GGT.Variable "+") [GGT.Variable "tmp", GGT.Variable "tmp0"])]))
      Map.empty
    `shouldBe`
      [TsDefLambda Nothing "sqr" (Lambda [] ["x"] [BsPartStmt Nothing (PsReturn (ExApp (AEVar "*") [AEVar "x", AEVar "x"]))]),
       TsDefLambda Nothing "sumsqr" (Lambda [] ["a", "b"]
         [BsPartStmt Nothing (PsDef "tmp" (ExApp (AEVar "sqr") [AEVar "a"])),
          BsPartStmt Nothing (PsDef "tmp0" (ExApp (AEVar "sqr") [AEVar "b"])),
          BsPartStmt Nothing (PsReturn (ExApp (AEVar "+") [AEVar "tmp", AEVar "tmp0"]))])]
  it ("Should lift closures with their capture parameters") $ do
    evalState
      (liftTopStmt Nothing []
        (GGT.DefineFunction "adder" ["x"]
          [GGT.DefineFunction "add-x" ["y"] [GGT.Return (GGT.AppExpr (GGT.Variable "+") [GGT.Variable "x", GGT.Variable "y"])],
           GGT.Return (GGT.TrvExpr (GGT.Variable "add-x"))]))
      (execState (mapM fresh ["adder", "x", "add-x", "y", "+"]) Map.empty)
    `shouldBe`
      [TsDefLambda Nothing "add-x0" (Lambda ["x"] ["y"] [BsPartStmt Nothing (PsReturn (ExApp (AEVar "+") [AEVar "x", AEVar "y"]))]),
       TsDefLambda Nothing "adder" (Lambda [] ["x"]
         [BsPartStmt Nothing (PsDef "add-x" (ExCapture (AEVar "add-x0") [AEVar "x"])),
          BsPartStmt Nothing (PsReturn (ExArg (AEVar "add-x")))])]
  it ("Should pass on asset_swap.glow") $ do
    evalState
      (liftTopStmt Nothing []
        (GGT.DefineInteraction
          "swap"
          (GGT.AnfInteractionDef ["A", "B"] ["T", "U"] ["t", "u"]
            [GGT.Deposit (GGT.Variable "A") (Map.fromList [ ( "T" , GGT.Variable "t" ) ]),
             GGT.DebugLabel "dlb1",
             GGT.Deposit (GGT.Variable "B") (Map.fromList [ ( "U" , GGT.Variable "u" ) ]),
             GGT.DebugLabel "dlb2",
             GGT.Withdraw (GGT.Variable "B") (Map.fromList [ ( "T" , GGT.Variable "t" ) ]),
             GGT.DebugLabel "dlb3",
             GGT.Withdraw (GGT.Variable "A") (Map.fromList [ ( "U" , GGT.Variable "u" ) ]),
             GGT.Return (GGT.TrvExpr (GGT.Explicit GGT.Unit))])))
      (execState (mapM fresh ["swap", "A", "B", "T", "U", "t", "u", "dlb1", "dlb2", "dlb3"]) Map.empty)
    `shouldBe`
      [TsDefInteraction "swap"
        (InteractionDef ["A", "B"] ["T", "U"] ["t", "u"]
          [BsDeposit "A" (Record (Map.fromList [("T", AEVar "t")])),
           BsDeposit "B" (Record (Map.fromList [("U", AEVar "u")])),
           BsWithdraw "B" (Record (Map.fromList [("T", AEVar "t")])),
           BsWithdraw "A" (Record (Map.fromList [("U", AEVar "u")])),
           BsPartStmt Nothing (PsReturn (ExArg AEEmptyTuple))])]
  it ("Should pass on buy_sig") $ do
    evalState
      (liftTopStmt Nothing []
        (GGT.DefineInteraction "buySig"
          (GGT.AnfInteractionDef ["Buyer", "Seller"] ["DefaultToken"] ["digest0", "price"]
            [GGT.Deposit (GGT.Variable "Buyer") (Map.fromList [ ( "DefaultToken" , GGT.Variable "price" ) ]),
             GGT.DebugLabel "dlb1",
             GGT.AtParticipant (GGT.Variable "Seller")
               (GGT.Define "signature" (GGT.Sign (GGT.Variable "digest0"))),
             GGT.Publish (GGT.Variable "Seller") [GGT.Variable "signature"],
             GGT.Define "tmp" (GGT.AppExpr (GGT.Variable "isValidSignature") [GGT.Variable "Seller", GGT.Variable "digest0", GGT.Variable "signature"]),
             GGT.Require (GGT.Variable "tmp"),
             GGT.DebugLabel "dlb2",
             GGT.Withdraw (GGT.Variable "Seller") (Map.fromList [ ( "DefaultToken" , GGT.Variable "price" ) ]),
             GGT.Return (GGT.TrvExpr (GGT.Explicit GGT.Unit))])))
      (execState (mapM fresh ["buySig", "Buyer", "Seller", "DefaultToken", "digest0", "price", "dlb1", "signature", "tmp", "isValidSignature", "dlb2"]) Map.empty)
    `shouldBe`
      [TsDefInteraction "buySig"
        (InteractionDef ["Buyer", "Seller"] ["DefaultToken"] ["digest0", "price"]
          [BsDeposit "Buyer" (Record (Map.fromList [("DefaultToken", AEVar "price")])),
           BsPartStmt (Just "Seller") (PsDef "signature" (ExSign (AEVar "digest0"))),
           BsPublish "Seller" "signature",
           BsPartStmt Nothing (PsDef "tmp" (ExApp (AEVar "isValidSignature") [AEVar "Seller", AEVar "digest0", AEVar "signature"])),
           BsPartStmt Nothing (PsRequire (AEVar "tmp")),
           BsWithdraw "Seller" (Record (Map.fromList [("DefaultToken", AEVar "price")])),
           BsPartStmt Nothing (PsReturn (ExArg AEEmptyTuple))])]
