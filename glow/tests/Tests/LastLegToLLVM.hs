module Tests.LastLegToLLVM where

import qualified Data.Map.Strict as M
import qualified Glow.Ast.LastLeg as Ast
import Glow.Prelude
import qualified Glow.Translate.LastLegToLLVM as Trans
import qualified LLVM.AST as LLVM
import qualified LLVM.IRBuilder as LLVM
import Test.Hspec

tests = describe "Tests for Glow.Translate.LastLegToLLVM" $ do
  it "Should compile an empty module correctly." $
    LLVM.buildModule
      "trivial"
      ( Trans.translateModule
          Ast.Module
            { Ast.modExterns = M.empty,
              Ast.modFunDefs = M.empty
            }
      )
      `shouldBe` LLVM.defaultModule
        { LLVM.moduleName = "trivial"
        }
