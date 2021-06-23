module Returns where
import Relude
import Parse
import qualified Data.HashMap.Strict as HM

data DoesItReturn a = No a | Yes a
  deriving Functor

instance Applicative DoesItReturn where
  pure = Yes
  Yes f <*> Yes x = Yes (f x)
  Yes f <*> No  x = No (f x)
  No  f <*> Yes x = No (f x)
  No  f <*> No  x = No (f x)

instance Semigroup a => Semigroup (DoesItReturn a) where
  Yes x <> _     = Yes x
  No  x <> Yes y = Yes (x <> y)
  No  x <> No  y = No  (x <> y)

instance Monoid a => Monoid (DoesItReturn a) where
  mempty = No mempty

extract :: DoesItReturn a -> a
extract (Yes a) = a
extract (No a)  = a

toplevel :: [QDecl] -> Either Ident [QDecl]
toplevel = traverse decl

decl :: QDecl -> Either Ident QDecl
decl (DFn n as r blk) = case block blk of
  Yes blk'           -> pure $ DFn n as r blk'
  No blk' | isVoid r -> pure $ DFn n as r blk'
  No _               -> Left n
decl d@DVar{} = pure d

isVoid :: QType -> Bool
isVoid (TStruct fs) = HM.null fs
isVoid _            = False

block :: [QStmt] -> DoesItReturn [QStmt]
block = foldMap stmt

data LoopType = While | DoWhile
  deriving Eq

stmt :: QStmt -> DoesItReturn [QStmt]
stmt s = case s of
  SExpr _                   -> No [s]
  SWhile c blk              -> loop While c blk
  SDoWhile blk c            -> loop DoWhile c blk
  SIf c t e | isLit True  c -> block t
            | isLit False c -> block e
            | otherwise     -> fmap pure $ liftA2 (SIf c) (block t) (block e)
  SVar _ _ _                -> No [s]
  SReturn _                 -> Yes [s]

loop :: LoopType -> QExpr -> [QStmt] -> DoesItReturn [QStmt]
loop lt cond blk
  | isLit True cond               = Yes [ctor lt cond $ extract blk']
  | DoWhile <- lt, Yes xs <- blk' = Yes xs
  | otherwise                     = No [ctor lt cond (extract blk')]
  where blk' = block blk

isLit :: Bool -> QExpr -> Bool
isLit b (ELit (LBool b')) = b == b'
isLit _ _                 = False

ctor :: LoopType -> QExpr -> [QStmt] -> QStmt
ctor While   = SWhile
ctor DoWhile = flip SDoWhile
