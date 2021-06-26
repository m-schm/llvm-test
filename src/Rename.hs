module Rename
  ( rename
  ) where
import Relude
import Control.Arrow ((***))
import qualified Data.List.NonEmpty as NE
import Parse (Ident, QDecl(..), QExpr(..), QStmt(..), QType(..))

type Env = NonEmpty [(Ident, Ident)]
type M = State (Word, Env)

rename :: [QDecl] -> [QDecl]
rename = fmap decl

run :: M a -> a
run = flip evalState (0, pure [])

decl :: QDecl -> QDecl
decl (DFn n as ret body) = run $
  flip (DFn n) ret <$> args as <*> traverse stmt body
decl (DVar n typ val) = DVar n typ val

args :: [(Ident, QType)] -> M [(Ident, QType)]
args = traverse $ \(i, t) -> flip (,) t <$> bind i

scope :: [QStmt] -> M [QStmt]
scope ss = modifyEnv (NE.cons [])
  *> traverse stmt ss
  <* modifyEnv (NE.fromList . NE.tail)

stmt :: QStmt -> M QStmt
stmt (SExpr e)       = SExpr <$> expr e
stmt (SWhile c ss)   = SWhile <$> expr c <*> scope ss
stmt (SDoWhile ss c) = SDoWhile <$> scope ss <*> expr c
stmt (SIf c ts es)   = SIf <$> expr c <*> scope ts <*> scope es
stmt (SVar n t e)    = do e' <- expr e; n' <- bind n; pure $ SVar n' t e'
stmt (SReturn e)     = SReturn <$> expr e

expr :: QExpr -> M QExpr
expr (EBinop l b r) = flip EBinop b <$> expr l <*> expr r
expr (EUnop u e)    = EUnop u <$> expr e
expr (f :$ xs)      = (:$) <$> expr f <*> traverse expr xs
expr (EVar n)       = EVar <$> name n
expr (EField e n)   = flip EField n <$> expr e
expr e@(ELit _)     = pure e
expr (EStruct fs)   = EStruct <$> traverse expr fs

bind :: Ident -> M Ident
bind i = do
  -- HACK: treats the numbered idents as strings, instead of parameterizing the
  -- AST on variable types
  i' <- gets (show . fst)
  modify' $ (+ 1) *** onHead ((i, i') :)
  pure i'

name :: Ident -> M Ident
name i = gets $ lookup i . snd

-- returns itself in the case of missing ident - this only happens when
-- referring to another top-level declaration, which are passed through to llvm
-- directly
lookup :: Ident -> Env -> Ident
lookup i = foldr step i . fold where
  step (old, new) next = if i == old then new else next

onHead :: (a -> a) -> NonEmpty a -> NonEmpty a
onHead f (a :| as) = f a :| as

modifyEnv :: (Env -> Env) -> M ()
modifyEnv = modify' . second
