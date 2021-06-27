module Codegen where
import Relude

import Control.Monad.Writer.Strict
import qualified Data.HashMap.Strict as HM
import qualified LLVM.AST as A
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import Orphans ()
import Parse (Ident, QDecl(..), QExpr(..), QLit(..), QStmt(..), QType(..))

tailCC :: CC.CallingConvention
tailCC = CC.Numbered 18

addrSpace :: AS.AddrSpace
addrSpace = AS.AddrSpace 0

alignment :: Word32
alignment = 0 -- leave it up to the target

intWidth :: Word32
intWidth = 64

type_ :: QType -> A.Type
type_ (TStruct fs)   = A.StructureType False $ fmap type_ $ HM.elems fs
type_ (TFn args ret) = A.FunctionType (type_ ret) (fmap type_ args) False
type_ (TPtr t)       = A.PointerType (type_ t) addrSpace
type_ TInt           = A.IntegerType intWidth
type_ TBool          = A.IntegerType 1

literal :: QLit -> C.Constant
literal (LInt i)  = C.Int intWidth i
literal (LBool b) = C.Int 1 $ if b then 1 else 0

attrsFor :: QType -> [A.ParameterAttribute]
attrsFor (TPtr _) = [A.NonNull]
attrsFor _        = []

decl :: QDecl -> G.Global
decl (DFn n args ret body) = G.Function
  { G.linkage              = L.External
  , G.visibility           = V.Default
  , G.dllStorageClass      = Nothing
  , G.callingConvention    = tailCC
  , G.returnAttributes     = attrsFor ret
  , G.returnType           = type_ ret
  , G.name                 = toName n
  , G.parameters           = (fmap params args, False)
  , G.functionAttributes   = [Right A.NoUnwind]
  , G.section              = Nothing
  , G.comdat               = Nothing
  , G.alignment            = alignment
  , G.garbageCollectorName = Nothing
  , G.prefix               = Nothing
  , G.basicBlocks          = fnBody body
  , G.personalityFunction  = Nothing
  , G.metadata             = []
  }
decl (DVar n typ val) = G.GlobalVariable
  { G.name            = toName n
  , G.linkage         = L.External
  , G.visibility      = V.Default
  , G.dllStorageClass = Nothing
  , G.threadLocalMode = Nothing
  , G.unnamedAddr     = Nothing
  , G.isConstant      = False
  , G.type'           = type_ typ
  , G.addrSpace       = addrSpace
  , G.initializer     = Just $ literal val
  , G.section         = Nothing
  , G.comdat          = Nothing
  , G.alignment       = alignment
  , G.metadata        = []
  }

toName :: Ident -> A.Name
toName = A.Name . encodeUtf8

params :: (Ident, QType) -> G.Parameter
params (i, t) = G.Parameter (type_ t) (toName i) (attrsFor t)

data CodegenState = CodegenState
  { cgCurrentBlock     :: A.Name
  , cgUnfinishedBlocks :: HashMap A.Name [A.Named A.Instruction]
  , cgNextTemp         :: Word
  , cgEnv              :: [HashMap Ident A.Name]
  }

type CodegenM = StateT CodegenState (Writer [G.BasicBlock])

fnBody :: [QStmt] -> [G.BasicBlock]
fnBody = execWriter . flip evalStateT initCodegen . traverse_ stmt where
  initCodegen = CodegenState
    { cgCurrentBlock = "entry"
    , cgUnfinishedBlocks = HM.singleton "entry" []
    , cgNextTemp = 0
    , cgEnv = []
    }

stmt :: QStmt -> CodegenM ()
stmt (SExpr e)       = exprTo Nothing e
stmt (SWhile c ss)   = _
stmt (SDoWhile ss c) = _
stmt (SIf c t e)     = _
stmt (SVar n t e)    = _
stmt (SReturn e)     = _

expr :: QExpr -> CodegenM A.Operand
expr (EBinop l b r) = _
expr (EUnop u e)    = _
expr (f :$ xs)      = _
expr (EVar n)       = _
expr (EField e t)   = _
expr (ELit l)       = pure $ A.ConstantOperand $ literal l
expr (EStruct fs)   = _

exprTo :: Maybe A.Name -> QExpr -> CodegenM ()
exprTo dest (EBinop l b r) = _
exprTo dest (EUnop u e)    = _
exprTo dest (f :$ xs)      = _
exprTo dest (EVar n)       = _
exprTo dest (EField e t)   = _
exprTo dest (ELit l)       = _
exprTo dest (EStruct fs)   = _

terminator :: A.Terminator -> CodegenM ()
terminator term = do
  CodegenState this blks temp env <- get
  let blk = reverse (blks HM.! this)
  tell . pure $ A.BasicBlock this blk (A.Do term)
  put $ CodegenState
    { cgCurrentBlock     = error "CodegenM: no block selected"
    , cgUnfinishedBlocks = HM.delete this blks
    , cgNextTemp         = temp
    , cgEnv              = env
    }

writeInstr :: A.Named A.Instruction -> CodegenM ()
writeInstr ni =
  modify' $ \s@(CodegenState this blks _ _) -> s
    { cgUnfinishedBlocks = HM.adjust (ni :) this blks
    }

(<--) :: Maybe A.Name -> A.Instruction -> CodegenM ()
(<--) = (writeInstr .) . maybe A.Do (A.:=)

instr' :: A.Instruction -> CodegenM ()
instr' = writeInstr . A.Do

instr :: A.Instruction -> CodegenM A.Name
instr i = do
  n <- freshName
  writeInstr (n A.:= i)
  pure n

lookup :: Ident -> CodegenM A.Operand
lookup i = foldr step global <$> gets cgEnv where
  global = A.ConstantOperand $ C.GlobalReference _ $ toName i
  step scope next = maybe next (A.LocalReference _) $ scope HM.!? i

freshName :: CodegenM A.Name
freshName = do
  n <- gets cgNextTemp
  modify' $ \s -> s { cgNextTemp = succ n }
  pure $ A.UnName n
