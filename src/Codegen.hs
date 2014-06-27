{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import "mtl" Control.Monad.State
import Control.Applicative

import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.FloatingPointPredicate as FP

-- double :: Type
-- double = FloatingPointType 64 IEEE

type SymbolTable = [(String, Operand)]

type Names = Map.Map String Int
uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix + 1) ns)

instance IsString Name where 
  fromString = Name . fromString

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

data CodegenState = CodegenState {
    currentBlock    :: Name,
    blocks :: Map.Map Name BlockState, -- a hash map from a name to a block
    symtab :: SymbolTable,
    blockCount :: Int,
    count :: Word,
    names :: Names
} deriving Show

data BlockState = BlockState {
  idx :: Int,
  stack :: [Named Instruction],
  term :: Maybe (Named Terminator)
} deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
                  deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
                 deriving (Functor, Applicative, Monad, MonadState AST.Module)


runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body =
  addDefn $
  GlobalDefinition $ functionDefaults {
    name = Name label
    , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType = retty
    , basicBlocks = body
    }

external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys =
  addDefn $
  GlobalDefinition $ functionDefaults {
    name = Name label
    , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType = retty
    , basicBlocks = []
    }

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname}
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block " ++ show c


emptyBlock :: Int -> BlockState
emptyBlock idx = BlockState idx [] Nothing

local :: Type -> Name -> Operand
local = LocalReference

externf :: Type -> Name -> Operand
externf t n = ConstantOperand $ C.GlobalReference t n

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x -> return x
    Nothing -> error $ "Local variable not in scope : " ++ show var

instr :: Instruction -> Codegen Operand
instr inst = do
  n <- fresh
  blk <- current
  let i = stack blk
  let ref = (UnName n)
  let typ = type inst
  modifyBlock $ blk { stack = i ++ [ref := inst ]}
  return $ local typ ref

















