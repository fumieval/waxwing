{-# LANGUAGE UndecidableInstances #-}
module AST where
import Data.Kind
import Data.List.NonEmpty

data Phase = Parsed

type family Ident (p :: Phase) :: Type

type instance Ident Parsed = String

type Stmts p = NonEmpty (Stmt p)

data Stmt p = ArgS (NonEmpty (Pat p))
  | BindS (Pat p) (Expr p)
  | ReturnS (Expr p)

deriving instance Show (Ident p) => Show (Stmt p)

data Pat p = AnyP
  | VarP (Ident p)
  | ForP (Pat p)
  | LitP Lit

deriving instance Show (Ident p) => Show (Pat p)

data Expr p = VarE (Ident p)
  | AppE (Expr p) (NonEmpty (Expr p))
  | LitE Lit
  | DoE (Stmts p)
  | IfE (NonEmpty (Stmts p))

deriving instance Show (Ident p) => Show (Expr p)

data Lit = IntL Integer
  | StrL String
  deriving Show