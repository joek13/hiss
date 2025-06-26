{-
  Collects constants and generates an initializer for some Hiss bytecode.
-}

module Codegen.Program (Program (..), mkProgram) where

import Codegen.Emit (Block (..))
import Codegen.Bytecode (Const (..), Instr(..))
import qualified Data.Set as Set
import Data.Set (Set)

data Program = Program { constants :: [Const], blocks :: [Block] }

-- | Finds unique constants in the provided blocks.
collectConstants :: [Block] -> Set Const
collectConstants = foldl f Set.empty
    where f consts block = consts `Set.union` collectConstants' (code block)

collectConstants' :: [Instr] -> Set Const
collectConstants' = foldl f Set.empty
    where f consts instr = consts `Set.union` collectConstant instr

collectConstant :: Instr -> Set Const
collectConstant (PushC c) = Set.singleton c
collectConstant _ = Set.empty

-- | Constant for the initializer function.
initConst :: Const
initConst = Func 0 "_init"

mkProgram :: [Block] -> Program
mkProgram bs = 
    let bs' = reverse bs
        -- The VM starts by calling the function represented by the first constant.
        -- Make sure the initializer block comes first.
        consts = initConst : (Set.toAscList . collectConstants) bs'
        in
            Program { constants = consts, blocks = bs' }