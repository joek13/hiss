{-
    Converts a Hiss bytecode program to binary format.
-}

module Assembler (assemble) where

import Codegen.Bytecode (Comparison (..), Const (..), Instr (..), Label)
import Codegen.Emit (Block (code, label))
import Codegen.Program (Program (blocks, constants))
import Control.Monad (forM_)
import Control.Monad.RWS (MonadReader (ask), RWS, execRWS, get, modify)
import Control.Monad.Writer.Lazy (MonadWriter (tell))
import Data.ByteString qualified as B
import Data.ByteString.Builder (int16BE, int64BE, toLazyByteString)
import Data.ByteString.Char8 (pack)
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word (Word8)
import Error (HissError)

opcode :: Instr -> Word8
opcode NoOp = 0x00
opcode (PushC _) = 0x11
opcode Pop = 0x12
opcode (LoadV _) = 0x13
opcode (StoreV _) = 0x14
opcode (LoadG _) = 0x15
opcode (StoreG _) = 0x16
opcode Halt = 0x20
opcode Call = 0x21
opcode Ret = 0x22
opcode (Br _) = 0x23
opcode (Jmp _) = 0x24
opcode IAdd = 0x30
opcode ISub = 0x31
opcode IMul = 0x32
opcode IDiv = 0x33
opcode IAnd = 0x34
opcode IOr = 0x35
opcode (ICmp _) = 0x36
opcode Print = 0xf0

cmpcode :: Comparison -> Word8
cmpcode Eq = 0x00
cmpcode NEq = 0x01
cmpcode Lt = 0x02
cmpcode LEq = 0x03
cmpcode Gt = 0x04
cmpcode GEq = 0x05

tag :: Const -> Word8
tag (Int _) = 0x01
tag (Func _ _) = 0x02

{-
    An initial implementation of the Hiss assembler ran in two passes:
    1. Generate bytecode, leaving placeholders for label references. Record label offsets as we go.
    2. Go back and overwrite placeholders with the now-known label offsets.

    This has been rewritten using laziness to be more idiomatic.
    The "Placeholder" type represents a value that can be materialized once the label map is known.
    E.g., "Placeholder ByteString" represents a not-yet-filled-in bytecode program.
-}

-- A map from labels to program offsets.
type LabelMap = Map Label Int

-- Represents a value that can be materialized once the label map is known.
type Placeholder a = LabelMap -> a

-- Assembly context.
data Ctx = Ctx {offset :: Int, labels :: LabelMap}

{-
    Represents a computation that can:
    - read the program
    - write some label info
    - read/modify the context
    - return a value of type a
-}
type Assemble a = RWS Program (Placeholder B.ByteString) Ctx a

-- Updates context by applying 'f' to the offset.
modifyOffset :: (Int -> Int) -> Assemble ()
modifyOffset f = modify $ \ctx -> ctx {offset = f (offset ctx)}

-- Emits immediate bytes and updates offset.
tellBytes :: B.ByteString -> Assemble ()
tellBytes bytes = do
  tell $ const bytes
  modifyOffset (+ B.length bytes)

-- Emits immediate byte and updates offset.
tellByte :: Word8 -> Assemble ()
tellByte = tellBytes . B.singleton

-- Emits placeholder for some bytes and updates offset.
-- We need to know the number of bytes ahead of time so we can update the offset.
tellPlaceholder :: Int -> Placeholder B.ByteString -> Assemble ()
tellPlaceholder n p = do
  tell p
  modifyOffset (+ n)

packInt64 :: Int -> B.ByteString
packInt64 = B.toStrict . toLazyByteString . int64BE . fromIntegral

packInt16 :: Int -> B.ByteString
packInt16 = B.toStrict . toLazyByteString . int16BE . fromIntegral

-- Resolves a label, or throws "compiler bug" message.
resolveLabel :: Label -> (Int -> a) -> LabelMap -> a
resolveLabel l f ls =
  case l `Map.lookup` ls of
    Just offset -> f offset
    Nothing -> error $ "compiler bug: failed to resolve label " <> l

-- Emits bytecode for a single constant.
assembleConstant :: Const -> Assemble ()
assembleConstant c = do
  -- Bytecode layout of a constant
  -- 1 byte: single u8 representing its type
  tellByte $ tag c
  case c of
    -- Bytecode layout of an int
    -- 8 bytes: single i64, big-endian
    Int i -> tellBytes (packInt64 i)
    -- Bytecode layout of a func
    -- 1 byte: single u8 representing function arity
    -- 8 bytes: single i64 representing function offset
    Func a l -> do
      tellByte $ fromIntegral a -- Emit function arity
      -- Emit a placeholder that will be filled when we know the label map
      tellPlaceholder 8 (resolveLabel l packInt64)

-- Resolves a constant or throws "compiler bug" message.
lookupConst :: Const -> Program -> Int
lookupConst c p = case c `elemIndex` constants p of
  Just i -> i
  Nothing -> error "compiler bug: constant not found"

relativeOffset :: Int -> Int -> Int
relativeOffset srcOffset dstOffset = dstOffset - srcOffset

-- Emits bytecode for a single instruction.
assembleInstr :: Instr -> Assemble ()
assembleInstr i = do
  -- All instructions start with an opcode.
  tellByte $ opcode i
  -- Some instructions have additional arguments.
  case i of
    -- pushc: additional byte for index into const table.
    PushC c -> do
      prog <- ask
      tellByte $ fromIntegral $ lookupConst c prog
    -- load/store: additional byte for index into variables.
    LoadV v -> do
      tellByte $ fromIntegral v
    StoreV v -> do
      tellByte $ fromIntegral v
    LoadG g -> do
      tellByte $ fromIntegral g
    StoreG g -> do
      tellByte $ fromIntegral g
    -- icmp: additional byte for the cmpcode.
    ICmp cmp -> do
      tellByte $ cmpcode cmp
    -- br/jmp: additional byte for a PC-relative offset.
    Br dst -> emitRelativeOffset dst
    Jmp dst -> emitRelativeOffset dst
    _ -> pure ()
  where
    emitRelativeOffset dst = do
      ctx <- get
      let currentOffset = offset ctx + 2
      tellPlaceholder 2 $ resolveLabel dst (packInt16 . relativeOffset currentOffset)

markLabel :: Label -> Assemble ()
markLabel l = modify $ \ctx -> ctx {labels = Map.insert l (offset ctx) (labels ctx)}

assembleBlock :: Block -> Assemble ()
assembleBlock b = do
  -- Record current offset for this block's label
  markLabel $ label b
  forM_ (code b) assembleInstr

assemble' :: Assemble ()
assemble' = do
  {-
    Hiss bytecode layout:
    - 4 bytes: literal bytes 'hiss'
    - 1 byte: single u8 representing number of module constants
    - One or more module constants
    - Module code
  -}
  tellBytes $ pack "hiss"

  prog <- ask
  let cs = constants prog
  -- Number of constants.
  tellByte $ fromIntegral $ length cs
  -- Module constants table.
  forM_ cs assembleConstant
  -- Module code.
  let bs = blocks prog
  -- Reset offset to zero.
  -- (Function offsets are relative to beginning of code section.)
  modifyOffset (const 0)
  forM_ bs assembleBlock

-- Converts a Program to Hiss bytecode.
assemble :: Program -> Either HissError B.ByteString
assemble prog =
  let initialCtx = Ctx {offset = 0, labels = Map.empty}
      (ctx, placeholder) = execRWS assemble' prog initialCtx
      ls = labels ctx
   in Right $ placeholder ls