{-
  Frontend for typechecking functionality.

  For definitions, see Semantic.Types.
  For type inference rules, see Semantic.Types.Constraints.
-}
module Semantic.Typechecking (Typecheck (..)) where

import Data.Bifunctor qualified (second)
import Error (HissError)
import Semantic.Types (Substitutable (apply), Type (..), TypeEnv, emptyEnv, relabel)
import Semantic.Types.Constraints (inferDecl, runInfer, solve)
import Semantic.Types.Constraints qualified as Constraints (infer, inferProgram)
import Syntax.AST (Annotated, Binding, Decl (..), Expr, Program (..))
import Syntax.Lexer (Range)

-- | Typeclass for AST nodes that can be type checked.
class Annotated t => Typecheck t where
  typecheckEnv :: TypeEnv -> t Range -> Either HissError (t (Range, Type))

  typecheck :: t Range -> Either HissError (t (Range, Type))
  typecheck = typecheckEnv emptyEnv

instance Typecheck Expr where
  typecheckEnv env expr = do
    -- infer type and collect constraints
    (expr', cs) <- runInfer env (Constraints.infer expr)
    -- solve for type substitutions
    subst <- solve cs
    -- apply substitutions to all types
    let solved = apply subst expr'
    -- relabel all types
    let relabeled = fmap (Data.Bifunctor.second relabel) solved
    return relabeled

instance Typecheck Binding where
  -- binding has type TUnit
  typecheckEnv _ b = return $ fmap (,TUnit) b

instance Typecheck Decl where
  typecheckEnv env decl = do
    -- infer type and collect constraints
    (decl', cs) <- runInfer env (inferDecl decl)
    -- solve for type substitutions
    subst <- solve cs
    -- apply substitutions to type
    let solved = apply subst decl'
    -- relabel all types
    let relabeled = fmap (Data.Bifunctor.second relabel) solved
    return relabeled

instance Typecheck Program where
  typecheckEnv env prog = do
    -- infer type and collect constraints
    (program', cs) <- runInfer env (Constraints.inferProgram prog)
    -- solve for type substitutions
    subst <- solve cs
    -- apply substitutions to all types
    let solved = apply subst program'
    -- relabel all types
    let relabeled = fmap (Data.Bifunctor.second relabel) solved
    return relabeled