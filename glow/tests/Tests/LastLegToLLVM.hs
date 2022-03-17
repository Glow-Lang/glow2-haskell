{-# LANGUAGE DuplicateRecordFields #-}

module Tests.LastLegToLLVM where

import qualified Data.Map.Strict as M
import qualified Glow.Ast.LastLeg as Ast
import Glow.Prelude
import qualified Glow.Translate.LastLegToLLVM as Trans
import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.CallingConvention
import LLVM.AST.Global
import LLVM.AST.Linkage
import LLVM.AST.Visibility
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
      `shouldBe` defaultModule
        { moduleName = "trivial"
        }
  describe "Tests for type translation" $ do
    it "Should handle translating externs correctly." $ do
      -- This is just one example; test more externs.
      LLVM.buildModule
        "myModule"
        ( Trans.translateModule
            Ast.Module
              { Ast.modExterns =
                  M.fromList
                    [ ( "foo",
                        Ast.TFPtr
                          Ast.FPtrType
                            { Ast.fptCaptures = [],
                              Ast.fptFuncType =
                                Ast.FuncType
                                  { Ast.ftParams = [],
                                    Ast.ftRetType = Ast.TTuple [],
                                    Ast.ftEffectful = False
                                  }
                            }
                      )
                    ],
                Ast.modFunDefs = M.empty
              }
        )
        `shouldBe` defaultModule
          { moduleName = "myModule",
            moduleDefinitions =
              [ GlobalDefinition
                  Function
                    { linkage = External,
                      visibility = Default,
                      dllStorageClass = Nothing,
                      callingConvention = C,
                      returnAttributes = [],
                      returnType =
                        PointerType
                          { pointerReferent =
                              StructureType
                                { isPacked = False,
                                  elementTypes = []
                                },
                            pointerAddrSpace = AddrSpace 0
                          },
                      name = Name "foo",
                      parameters =
                        ( [ Parameter
                              PointerType
                                { pointerReferent =
                                    StructureType
                                      { isPacked = False,
                                        elementTypes = []
                                      },
                                  pointerAddrSpace = AddrSpace 0
                                }
                              (Name "")
                              []
                          ],
                          False
                        ),
                      functionAttributes = [],
                      section = Nothing,
                      comdat = Nothing,
                      alignment = 0,
                      garbageCollectorName = Nothing,
                      prefix = Nothing,
                      basicBlocks = [],
                      personalityFunction = Nothing,
                      metadata = []
                    }
              ]
          }
