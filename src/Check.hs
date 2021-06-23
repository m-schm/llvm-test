module Check where
import Relude
import Parse
import qualified Data.HashMap.Strict as HM
import Data.List (lookup)

data TypeError
  = NotDefined Ident
  | WrongType QType QType
  | TopLevelTwice
  | NotComparable QType
  | NotPtr QType
  | NotStruct QType
  | NotFunction QType
  | WrongArity [QType] [QType]
  | DoesntHaveField (HashMap Ident QType) Ident

type M = ReaderT (Ident, QType) (Either (Ident, TypeError))

type Env = [(Ident, QType)]

check :: [QDecl] -> Either (Ident, TypeError) [QDecl]
check ds = do
  env <- getTypes ds
  traverse (decl env) ds

getTypes :: [QDecl] -> Either (Ident, TypeError) Env
getTypes = fmap HM.toList . foldlM addDecl HM.empty where
  addDecl hm (DVar name      ty  _) = tryInsert hm name ty
  addDecl hm (DFn  name args ret _) = tryInsert hm name (TFn (fmap snd args) ret)

  tryInsert hm name ty
    | HM.member name hm = Left (name, TopLevelTwice)
    | otherwise         = Right $ HM.insert name ty hm

decl :: Env -> QDecl -> Either (Ident, TypeError) QDecl
decl _ d@(DVar name ty val) =
  let lt = litType val
  in if ty == litType val
     then pure d
     else Left (name, WrongType ty lt)
decl env (DFn name args ret body) =
  runReaderT (block (args <> env) body) (name, ret)
  <&> DFn name args ret

block :: Env -> [QStmt] -> M [QStmt]
block env = flip evalStateT env . traverse stmt

stmt :: QStmt -> StateT Env M QStmt
stmt (SExpr e) = SExpr . fst <$> expr' e
stmt (SWhile cond blk) = do
  (c, ct) <- expr' cond
  lift $ ct =! TBool
  env <- get
  SWhile c <$> lift (block env blk)
stmt (SDoWhile blk cond) = do
  (c, ct) <- expr' cond
  lift $ ct =! TBool
  env <- get
  flip SDoWhile c <$> lift (block env blk)
stmt (SVar name ty body) = do
  (b, bt) <- expr' body
  lift $ bt =! ty
  modify ((name, bt) :)
  pure (SVar name ty b)
stmt (SReturn e) = do
  (e', et) <- expr' e
  lift $ (et =!) =<< returnType
  pure (SReturn e')

expr' :: QExpr -> StateT Env M (QExpr, QType)
expr' e = lift . runReaderT (expr e) =<< get

data OpType = Num | Equ | Ord | Assign
  deriving Eq

binopType :: Binop -> OpType
binopType Add = Num
binopType Sub = Num
binopType Mul = Num
binopType Div = Num
binopType Mod = Num
binopType Pow = Num
binopType Eq  = Equ
binopType Neq = Equ
binopType Lt  = Ord
binopType Leq = Ord
binopType Gt  = Ord
binopType Geq = Ord
binopType Set = Assign

typesMatch :: OpType -> QType -> QType -> M QType
typesMatch Num l r = do l =! TInt; r =! TInt; pure TInt
typesMatch Equ l r = (l =! r) $> TBool
typesMatch Ord l r = do
  unless (l == TInt || l == TBool) $ raise $ NotComparable l
  l =! r
  pure TBool
typesMatch Assign l r = (l =! r) $> r

checkUnop :: Unop -> QType -> M QType
checkUnop Ref    t = pure (TPtr t)
checkUnop Deref  t = case t of TPtr t' -> pure t'
                               _       -> raise $ NotPtr t
checkUnop Negate t = (t =! TInt) $> TInt
checkUnop Not    t = (t =! TBool) $> TBool

expr :: QExpr -> ReaderT Env M (QExpr, QType)
expr (EBinop l b r) = do
  (l', lt) <- expr l
  (r', rt) <- expr r
  let bt = binopType b
  when (bt == Assign) $ lift $ assertLvalue l'
  (,) (EBinop l' b r') <$> lift (typesMatch bt lt rt)
expr (EUnop u e) = do
  (e', et) <- expr e
  (,) (EUnop u e') <$> lift (checkUnop u et)
expr e@(EVar v) = asks (lookup v)
  >>= lift . maybe (raise $ NotDefined v) (pure . (,) e)
expr (EField s f) = lift . uncurry (doField f) =<< expr s
expr (f :$ xs) = do
  (f', ft) <- expr f
  (xs', xts) <- unzip <$> traverse expr xs
  lift $ doFunction (f' :$ xs') ft xts
expr e@(ELit l) = pure (e, litType l)
expr (EStruct fs) = traverse expr fs
  <&> EStruct . fmap fst &&& TStruct . fmap snd

doField :: Ident -> QExpr -> QType -> M (QExpr, QType)
doField f e (TStruct fs) = case HM.lookup f fs of
  Nothing           -> raise $ DoesntHaveField fs f
  Just ft           -> pure (e, ft)
doField f e (TPtr t) = doField f (EUnop Deref e) t
doField _ _ t        = raise $ NotStruct t

doFunction :: QExpr -> QType -> [QType] -> M (QExpr, QType)
doFunction e (TFn as r) xs
  | length as == length xs = raise $ WrongArity as xs
  | otherwise              = zipWithM_ (=!) as xs $> (e, r)
doFunction e (TPtr t)   xs = doFunction (EUnop Deref e) t xs
doFunction _ t          _  = raise $ NotFunction t

assertLvalue :: QExpr -> M ()
assertLvalue = error "not implemented"

(=!) :: QType -> QType -> M ()
t =! u
  | t == u    = pure ()
  | otherwise = raise $ WrongType t u

returnType :: M QType
returnType = asks snd

raise :: TypeError -> M a
raise a = do
  n <- asks fst
  lift $ Left (n, a)

litType :: QLit -> QType
litType (LInt _)  = TInt
litType (LBool _) = TBool
