module Semantic.Names (collectNames, checkNames, reorderDecls) where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (MonadState (get, put), State, evalState, gets, modify)
import Data.Foldable (find)
import Data.Graph (Tree (Node), graphFromEdges, scc)
import Data.List (partition)
import Data.Map (Map)
import Data.Map qualified as Map (fromList, lookup)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert, map, member, singleton, toList, union)
import Data.Tree (Tree (rootLabel, subForest))
import Error (HissError (SemanticError))
import Syntax (getLineCol, start)
import Syntax.AST (Annotated (getAnn), Binding (..), Decl (..), Expr (..), Name (..), Program (..), declGetName, getIdent, progDecls)
import Syntax.Lexer (Range)

-- | Collects Names referenced in an expression and all of its descendants.
collectNames :: Expr a -> Set (Name a)
collectNames (EBool _ _) = Set.empty
collectNames (EInt _ _) = Set.empty
collectNames (EVar _ n) = Set.singleton n
collectNames (EFunApp _ f args) = collectNames f `Set.union` foldl Set.union Set.empty (map collectNames args)
collectNames (EUnaryOp _ _ e1) = collectNames e1
collectNames (EBinOp _ e1 _ e2) = collectNames e1 `Set.union` collectNames e2
collectNames (ELetIn _ _ e1 e2) = collectNames e1 `Set.union` collectNames e2
collectNames (EIf _ e1 e2 e3) = collectNames e1 `Set.union` collectNames e2 `Set.union` collectNames e3
collectNames (EParen _ e1) = collectNames e1

-- | Name-checks a program.
-- Returns an error if a global is declared more than once, a binding shadows a global, or an undeclared name is used.
-- On success, returns the program unchanged.
checkNames :: Program Range -> Either HissError (Program Range)
checkNames prog = do
  -- initialize name check ctx
  gs <- progGlobals prog
  let ctx =
        NameCheckCtx
          { globals = gs,
            declared = gs
          }
  -- actually run name check
  evalState (runExceptT $ progCheckNames prog) ctx
  -- return program unchanged
  return prog

-- | Collects the names of a program's globals, ensuring they are unique.
progGlobals :: Program Range -> Either HissError (Set (Name Range))
progGlobals = go . progDecls
  where
    go = foldl f (Right Set.empty)
    f (Left err) _ = Left err -- propagate error
    f (Right decld) decl
      -- check if this name is already declared
      -- if so, error
      | name `Set.member` decld = Left $ SemanticError $ "Name error: Global '" <> getIdent name <> "' redeclared at line " <> show line <> ", column " <> show column
      -- if not, insert it
      | otherwise = Right (name `Set.insert` decld)
      where
        name = declGetName decl
        (line, column) = getLineCol $ start $ getAnn name

data NameCheckCtx = NameCheckCtx
  { globals :: Set (Name Range),
    declared :: Set (Name Range)
  }

type NameCheck = ExceptT HissError (State NameCheckCtx)

-- | Throws if 'name' shadows a global in current context.
throwIfShadowsGlobal :: Name Range -> NameCheck ()
throwIfShadowsGlobal name = do
  gs <- gets globals

  when (name `Set.member` gs) $
    throwError $
      SemanticError $
        "Name error: Global '" <> getIdent name <> "' shadowed by declaration at line " <> show line <> ", column " <> show col
  where
    (line, col) = getLineCol $ start $ getAnn name

-- | Throws if 'name' is undeclared in current context.
throwIfUndeclared :: Name Range -> NameCheck ()
throwIfUndeclared name = do
  decld <- gets declared

  unless (name `Set.member` decld) $
    throwError $
      SemanticError $
        "Name error: Use of undeclared name '" <> getIdent name <> "' at line " <> show line <> ", column " <> show col
  where
    (line, col) = getLineCol $ start $ getAnn name

-- | Marks 'name' as declared in current context.
declare :: Name Range -> NameCheck ()
declare name = do
  decld <- gets declared
  let decld' = name `Set.insert` decld
  modify $ \ctx -> ctx {declared = decld'}

-- | Name-checks a program.
progCheckNames :: Program Range -> NameCheck ()
progCheckNames prog = do
  mapM_ declCheckNames $ progDecls prog

-- | Name-checks a declaration.
declCheckNames :: Decl Range -> NameCheck ()
declCheckNames (Decl _ (ValBinding _ _) body) = exprCheckNames body
declCheckNames (Decl _ (FuncBinding _ _ args) body) = do
  ctx <- get -- backup ctx

  -- mark args as declared in body
  mapM_ throwIfShadowsGlobal args
  mapM_ declare args
  exprCheckNames body

  put ctx -- restore ctx

exprCheckNames :: Expr Range -> NameCheck ()
exprCheckNames (EInt _ _) = return ()
exprCheckNames (EBool _ _) = return ()
exprCheckNames (EVar _ name) = throwIfUndeclared name
exprCheckNames (EFunApp _ fun args) = do
  exprCheckNames fun
  mapM_ exprCheckNames args
exprCheckNames (EUnaryOp _ _ operand) = exprCheckNames operand
exprCheckNames (EBinOp _ op1 _ op2) = do
  exprCheckNames op1
  exprCheckNames op2
exprCheckNames (ELetIn _ binding valExp inExp) = case binding of
  ValBinding _ name -> do
    ctx <- get -- back up ctx
    exprCheckNames valExp
    throwIfShadowsGlobal name
    declare name
    exprCheckNames inExp
    put ctx -- restore ctx
  FuncBinding _ name args -> do
    ctx <- get -- back up ctx

    -- function name is declared in inExp
    throwIfShadowsGlobal name
    declare name
    exprCheckNames inExp

    -- function name and args are declared in valExp
    mapM_ throwIfShadowsGlobal args
    mapM_ declare args
    exprCheckNames valExp

    put ctx -- restore ctx
exprCheckNames (EIf _ condExp thenExp elseExp) = do
  exprCheckNames condExp
  exprCheckNames thenExp
  exprCheckNames elseExp
exprCheckNames (EParen _ subexpr) = exprCheckNames subexpr

-- | Re-orders a program's declarations to support program interpretation.
--   Moves function declarations to the top, followed by value declarations in
--   dependency order.
--   Returns an error if a value cycle is detected.
reorderDecls :: Program Range -> Either HissError (Program Range)
reorderDecls prog = do
  let (funcDecls, valDecls) = partition isFuncDecl $ progDecls prog

  -- build map from name -> vertex id
  let nameToVertex = Map.fromList $ zip (map declGetName valDecls) [0, 1 ..]
  let edges = map (declAdjList nameToVertex) valDecls

  -- dependency graph of program's value bindings
  let (graph, nodeFromVertex, _) = graphFromEdges edges

  -- find SCCs
  let sccs = scc graph
  -- check if there are any value cycles
  -- graph is acyclic <=> each SCC contains exactly one vertex
  let cyclic = find isCyclic sccs

  case cyclic of
    -- if cycle, report an error
    -- todo: preview the dependency cycle (e.g. a->b->c->a)
    Just Node {rootLabel = x} -> Left $ SemanticError $ "Definition of '" <> getIdent name <> "' at line " <> show line <> ", column " <> show col <> " is cyclic"
      where
        name = (declGetName . declFromNode . nodeFromVertex) x
        (line, col) = (getLineCol . start . getAnn) name
    {- otherwise, the dependency graph is acyclic
       reorder decls:
       1. functions (in an arbitrary order)
       2. values (in reverse topological order)
       this ensures values can be initialized in the order they are declared -}
    Nothing -> return $ Program (getAnn prog) $ funcDecls ++ map (declFromNode . nodeFromVertex . rootLabel) sccs
  where
    isFuncDecl (Decl _ (FuncBinding {}) _) = True
    isFuncDecl _ = False
    isCyclic Node {subForest = []} = False
    isCyclic _ = True
    declFromNode (decl, _, _) = decl

-- | Given (name -> vertex) map and a declaration, creates an adjacency list entry
--   The node is the declaration itself, and the adjacency list tracks dependencies on other declarations.
declAdjList :: Map (Name Range) Int -> Decl Range -> (Decl Range, Int, [Int])
declAdjList nameToVertex decl@(Decl _ (ValBinding _ name) body) = (decl, vertex, depVertices)
  where
    -- vertex id of this decl
    vertex = case name `Map.lookup` nameToVertex of
      Just x -> x
      Nothing -> error "Compiler bug: decl name missing from 'nameToVertex' in declAdjList"
    -- names referenced in body
    deps = collectNames body -- TODO: also collect values used in function calls, etc.
    {- lookup vertex ids of deps

       we don't care about function calls (which are missing from nameToVertex) so we can just discard 'Nothing's
       ...and checkNames has already guaranteed there are no undeclared names -}
    depVertices = catMaybes $ Set.toList $ Set.map (`Map.lookup` nameToVertex) deps
declAdjList _ _ = error "Compiler bug: declAdjList called on a function binding"