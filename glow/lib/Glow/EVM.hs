module Glow.EVM where

import qualified EVM.Opcode as EO
import Glow.Prelude
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Attribute as LLVM
import qualified LLVM.IRBuilder as LLVM

-- | @evmOpWrapperBody op@ generates the body of a wrapper function that
-- wraps the opcode 'op'.
--
-- It expects to be called with the return address at the top of the stack
-- and the arguments underneath that, rotated from the order the instruction
-- takes them such that the topmost instruction becomes the bottom most, and
-- the others each move up one.
--
-- Upon return, the return values will be rotated in the same way.
--
-- TODO: Check what LLVM actually expects for e.g. struct returns, and make
-- sure it lines up.
evmOpWrapperBody :: EO.Opcode -> [EO.Opcode]
evmOpWrapperBody op =
  mconcat
    [ [EO.JUMPDEST ()],
      shuffle nargs,
      [op],
      shuffle nrets,
      [EO.JUMP ()]
    ]
  where
    nargs = EO.opcodeDelta spec
    nrets = EO.opcodeAlpha spec
    spec = EO.opcodeSpec op
    shuffle n =
      if n == 0
        then []
        else [EO.SWAP (toEnum (fromIntegral n))]

-- | @callEvmOp op args@ emits a call to the wrapper function for @op@,
-- with @args@ listed from top to bottom as inspected by the *instruction*
-- (rather than the wrapper function, which is shifted).
callEvmOp ::
  (LLVM.MonadModuleBuilder m, LLVM.MonadIRBuilder m) =>
  EO.Opcode ->
  [(LLVM.Operand, [LLVM.ParameterAttribute])] ->
  m LLVM.Operand
callEvmOp op args = LLVM.call (evmOpRef op) (shuffle args)
  where
    shuffle [] = []
    shuffle (x : xs) = xs <> [x]

evmOpRef :: EO.Opcode -> LLVM.Operand
evmOpRef _op =
  -- TODO: call evmOpName and convert it to a GlobalReference. Will need to
  -- also infer the type.
  undefined

evmOpName :: EO.Opcode -> LLVM.Name
evmOpName op = fromString $ "evmOp_" <> show op
