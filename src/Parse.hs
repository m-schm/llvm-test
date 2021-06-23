module Parse
  ( toplevel
  , Ident, QDecl(..), QType(..), QExpr(..), QLit(..), QStmt(..)
  , Binop(..), Unop(..)
  ) where
import Relude hiding (many)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (char, string)
import Data.Char
import Relude.Unsafe (read)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

newtype QParseError = DuplicateKeys Ident
  deriving (Show, Eq, Ord)

instance ShowErrorComponent QParseError where
  showErrorComponent (DuplicateKeys v) = "Duplicate key: " <> T.unpack v

type Parser = Parsec QParseError Text

type Ident = Text

toplevel :: Parser [QDecl]
toplevel = many decl

ident :: Parser Ident
ident = do
  i <- takeWhile1P (Just "identifier") isAlpha
  guard $ i `notElem`
    ["while", "do", "return", "int", "bool", "false", "true", "void"]
  space
  pure i

kw :: Ident -> Parser ()
kw s = chunk s *> notFollowedBy (satisfy isAlpha) *> space

data QType
  = TStruct (HashMap Ident QType)
  | TFn [QType] QType
  | TPtr QType
  | TInt
  | TBool
  deriving (Show, Eq)

type_ :: Parser QType
type_ =
      TInt <$ string "int"
  <|> TBool <$ string "bool"
  <|> TStruct mempty <$ string "void"
  <|> TPtr <$ char '*' <*> type_
  <|> TFn <$> parens (type_ `sepEndBy` char ',') <* string "->" <*> type_
  <|> TStruct <$> braces (hmFromList =<< field `sepEndBy` char ',')
  where field = (,) <$> ident <* char ':' <*> type_

data QLit
  = LInt Integer
  | LBool Bool
  deriving Show

lit :: Parser QLit
lit = LInt . read . T.unpack <$> takeWhile1P (Just "integer") (`elem` digits) <* space
  <|> LBool True <$ string "true"
  <|> LBool False <$ string "false"
  where digits = "0123456789" :: [Char]

data QExpr
  = EBinop QExpr Binop QExpr
  | EUnop Unop QExpr
  | QExpr :$ [QExpr]
  | EVar Text
  | EField QExpr Text
  | ELit QLit
  | EStruct (HashMap Ident QExpr)
  deriving Show

data Binop
  = Add | Sub
  | Mul | Div | Mod
  | Pow
  | Eq | Neq | Lt | Leq | Gt | Geq
  | Set
  deriving Show
data Unop = Ref | Deref | Negate | Not
  deriving Show

prec :: Binop -> Int
prec Add = 2
prec Sub = 2
prec Mul = 3
prec Div = 3
prec Mod = 3
prec Pow = 4
prec Eq  = 1
prec Neq = 1
prec Lt  = 1
prec Leq = 1
prec Gt  = 1
prec Geq = 1
prec Set = 0

binop :: Parser Binop
binop = choice
  [ Add <$ char '+'
  , Sub <$ char '-'
  , Mul <$ char '*'
  , Div <$ char '/'
  , Mod <$ char '%'
  , Pow <$ char '^'
  , Eq  <$ string "=="
  , Neq <$ string "!="
  , Leq <$ string "<="
  , Lt  <$ char '<'
  , Geq <$ string ">="
  , Gt  <$ char '>'
  , Set <$ char '='
  ]

unop :: Parser Unop
unop = choice
  [ Ref    <$ char '&'
  , Deref  <$ char '*'
  , Negate <$ char '-'
  , Not    <$ char '!'
  ]

data OpStk a b = OpHead a | OpSnoc (OpStk a b) b a

mapHead :: (a -> a) -> OpStk a b -> OpStk a b
mapHead f (OpHead      a) = OpHead (f a)
mapHead f (OpSnoc xs b a) = OpSnoc xs b (f a)
{-# INLINE mapHead #-}

expr :: Parser QExpr
expr = tightExpr >>= operators . OpHead

operators :: OpStk QExpr Binop -> Parser QExpr
operators stk@(OpHead e) =
      (operators =<< OpSnoc stk <$> binop <*> tightExpr)
  <|> pure e
operators stk@(OpSnoc es b e) =
      do b' <- binopTighter b
         operators . OpSnoc stk b' =<< tightExpr
  <|> operators (mapHead (\e' -> EBinop e' b e) es)

binopTighter :: Binop -> Parser Binop
binopTighter b = try $ do
  b' <- binop
  guard $ prec b' >= prec b
  pure b'

rassoc :: QExpr -> Parser QExpr
rassoc e =
      (rassoc . (e :$) =<< parens (expr `sepEndBy` char ','))
  <|> (rassoc . EField e =<< char '.' *> ident)
  <|> pure e

tightExpr :: Parser QExpr
tightExpr =
      EUnop <$> unop <*> tightExpr
  <|> (exprHead >>= rassoc)

struct :: Parser QExpr
struct = fmap EStruct . braces $
  hmFromList =<< field `sepEndBy` char ','
  where field = (,) <$> ident <* char ':' <*> expr

exprHead :: Parser QExpr
exprHead = choice
  [ ELit <$> lit
  , EVar <$> ident
  , parens exprHead
  , struct
  ]

data QStmt
  = SExpr QExpr
  | SWhile QExpr [QStmt]
  | SDoWhile [QStmt] QExpr
  | SVar Ident QType QExpr
  | SReturn QExpr
  deriving Show

stmt :: Parser QStmt
stmt = choice
  [ SWhile   <$  kw "while" <*> expr <*> block
  , SDoWhile <$  kw "do" <*> block <* kw "while" <*> expr <* char ';'
  , SReturn  <$  kw "return" <*> expr <* char ';'
  , varDecl SVar expr
  , SExpr    <$> expr <* char ';'
  ]

block :: Parser [QStmt]
block = braces (many stmt) <* optional (char ';')

data QDecl
  = DFn Ident [(Ident, QType)] QType [QStmt]
  | DVar Ident QType QLit
  deriving Show

decl :: Parser QDecl
decl = varDecl DVar lit
   <|> DFn  <$> ident
            <*> parens (arg `sepEndBy` char ',')
            <*> option (TStruct mempty) (char ':' *> type_)
            <*> body
  where
    arg = (,) <$> ident <* char ':' <*> type_
    body = block
       <|> char '=' *> fmap (pure . SReturn) expr <* char ';'

varDecl :: (Ident -> QType -> a -> b) -> Parser a -> Parser b
varDecl ctor rhs =
  ctor <$> try (ident <* char ':') <*> type_ <* char '=' <*> rhs <* char ';'

char :: Char -> Parser ()
char c = single c *> space

string :: Text -> Parser ()
string s = chunk s *> space

parens, braces :: Parser a -> Parser a
parens = between (char '(') (char ')')
braces = between (char '{') (char '}')

hmFromList :: Foldable t => t (Ident, a) -> Parser (HashMap Ident a)
hmFromList = foldlM insert HM.empty where
  insert :: HashMap Ident a -> (Ident, a) -> Parser (HashMap Ident a)
  insert hm (k, v)
    | HM.member k hm = customFailure $ DuplicateKeys k
    | otherwise      = pure $ HM.insert k v hm
