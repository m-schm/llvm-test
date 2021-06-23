module Check (check, errorPretty) where
import Relude
import Parse
import qualified Data.HashMap.Strict as HM
import Data.List (lookup)
import qualified Data.Text as T

data TypeError
  = NotDefined Ident
  | WrongType QType QType
  | TopLevelTwice
  | NotComparable QType
  | NotPtr QType
  | NotFunction QType
  | NotLvalue
  | WrongArity [QType] [QType]
  | DoesntHaveField QType Ident

errorPretty :: (Ident, TypeError) -> String
errorPretty (i, t) = "Error in `" <> T.unpack i <> "`: " <> typeErrorPretty t

typeErrorPretty :: TypeError -> String
typeErrorPretty (NotDefined v)        = "Not in scope: " <> T.unpack v
typeErrorPretty (WrongType l r)       =
  "Type mismatch\n\tgot: " <> typePretty l <> "\n\texpected: " <> typePretty r
typeErrorPretty TopLevelTwice         = "Multiple definitions"
typeErrorPretty (NotComparable t)     = "Not a comparable type: " <> typePretty t
typeErrorPretty (NotPtr t)            = "Not a pointer type: " <> typePretty t
typeErrorPretty (NotFunction t)       = "Not a callable type: " <> typePretty t
typeErrorPretty NotLvalue             = "Attempt to assign to non-assignable expression"
typeErrorPretty (DoesntHaveField t f) =
  "Doesn't have field `" <> T.unpack f <> "`: " <> typePretty t
typeErrorPretty (WrongArity l r)      =
  "Wrong arity (got " <> show (length l) <> ", expected " <> show (length r)
  <> ")\n\tgot: " <> intercalate ", " (fmap show l)
  <> ")\n\texpected: " <> intercalate ", " (fmap show r)

typePretty :: QType -> String
typePretty (TStruct fs) =
  "{" <> intercalate ", " ((\(f, t) -> T.unpack f <> ": " <> typePretty t) <$> HM.toList fs) <> "}"
typePretty (TFn as r)   =
  "(" <> intercalate ", " (fmap typePretty as) <> ") -> " <> typePretty r
typePretty (TPtr t)     = "*" <> typePretty t
typePretty TInt         = "int"
typePretty TBool        = "bool"

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
typesMatch Equ l r = (r =! l) $> TBool
typesMatch Ord l r = do
  unless (l == TInt || l == TBool) $ raise $ NotComparable l
  l =! r
  pure TBool
typesMatch Assign l r = (r =! l) $> r

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
doField f e t@(TStruct fs) = case HM.lookup f fs of
  Nothing           -> raise $ DoesntHaveField t f
  Just ft           -> pure (e, ft)
doField f e (TPtr t) = doField f (EUnop Deref e) t
doField f _ t        = raise $ DoesntHaveField t f

doFunction :: QExpr -> QType -> [QType] -> M (QExpr, QType)
doFunction e (TFn as r) xs
  | length as == length xs = raise $ WrongArity xs as
  | otherwise              = zipWithM_ (=!) xs as $> (e, r)
doFunction e (TPtr t)   xs = doFunction (EUnop Deref e) t xs
doFunction _ t          _  = raise $ NotFunction t

assertLvalue :: QExpr -> M ()
assertLvalue (EUnop Deref _) = pure ()
assertLvalue (EVar _)        = pure ()
assertLvalue (EField _ _)    = pure ()
assertLvalue _               = raise NotLvalue

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
