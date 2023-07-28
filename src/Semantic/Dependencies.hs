{-
    1. Generation of program declaration dependency graph.
    2. Semantic pass to detect value cycles.
-}

module Semantic.Dependencies (dependencyGraph, reorderDecls) where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (MonadState (get, put), State, evalState, gets, modify)
import Data.Foldable (find)
import Data.Foldable qualified as Foldable (toList)
import Data.Function ((&))
import Data.Graph (graphFromEdges, scc, Vertex, Graph)
import Data.Map (Map)
import Data.Map qualified as Map (fromList, lookup)
import Data.Maybe (catMaybes, fromJust)
import Data.Set (Set, (\\))
import Data.Set qualified as Set (empty, fromList, insert, map, member, singleton, toList, union)
import Error (HissError (SemanticError))
import Syntax (getLineCol, start)
import Syntax.AST (Annotated (getAnn), Binding (..), Decl (..), Expr (..), Name (..), Program (..), declGetName, getIdent, progDecls)
import Syntax.Lexer (Range)
import Semantic.Names (collectNames)

-- | Given (name -> vertex) map and a declaration, creates an adjacency list entry
--   The node is the declaration itself, and the adjacency list tracks dependencies on other declarations.
declAdjList :: Map (Name a) Int -> Decl a -> (Decl a, Int, [Int])
declAdjList nameToVertex decl@(Decl _ binding body) = (decl, vertex, depVertices)
  where
    name = declGetName decl
    -- vertex id of this decl
    vertex = case name `Map.lookup` nameToVertex of
      Just x -> x
      Nothing -> error "Compiler bug: decl name missing from 'nameToVertex' in declAdjList"
    -- find free vars referenced in body
    deps = case binding of
      ValBinding _ _ -> collectNames body
      FuncBinding _ _ args -> collectNames body \\ (Set.fromList args `Set.union` Set.singleton name)
    -- lookup vertex ids of deps
    depVertices = catMaybes $ Set.toList $ Set.map (`Map.lookup` nameToVertex) deps

-- | Given a Program, constructs a dependency graph of its declarations.
--   Returns (graph, declFromVertex)
dependencyGraph :: Program a -> (Graph, Vertex -> Decl a)
dependencyGraph prog =
  let decls = progDecls prog
      -- map from decl name -> vertex id
      nameToVertex = Map.fromList $ zip (map declGetName decls) [0, 1 ..]
      -- compute dependency list for each decl
      edges = map (declAdjList nameToVertex) decls
      -- create graph
      (graph, nodeFromVertex, _) = graphFromEdges edges
   in (graph, declFromNode . nodeFromVertex)
  where
    -- Extracts declaration from adjacency list node.
    declFromNode (decl, _, _) = decl

-- | Re-orders a program's declarations to support program interpretation.
--   Declarations are reorganized into dependency order so that values are
--   always declared lexically before they are used.
--   If this is impossible (i.e. there is a cyclic definition), returns an error.
reorderDecls :: Program Range -> Either HissError (Program Range)
reorderDecls prog = do
  let (graph, declFromVertex) = dependencyGraph prog

  let sccs =
        scc graph -- find strongly connected components (SCCs)
          & map (fmap declFromVertex) -- convert from Forest Vertex -> Forest (Decl Range)

  {- in general, a graph is acyclic <=> each SCC contains exactly one vertex
     In our case, we only care about dependency cycles containing value declarations,
     since we allow mutually recursive functions
   -}
  let cyclic = find isCyclic sccs
  -- any dependency cycles containing values?
  case cyclic of
    Just tree ->
      -- if yes, find a value to report (since tree root might be a function declaration)
      let decl = fromJust $ find isValDecl tree
          name = declGetName decl
          (line, col) = (getLineCol . start . getAnn) name
       in Left $ SemanticError $ "Definition of '" <> getIdent name <> "' at line " <> show line <> ", column " <> show col <> " is cyclic"
    -- if no, return the program in dependency order
    Nothing -> return $ Program (getAnn prog) $ concatMap Foldable.toList sccs
  where
    --  Returns True if its argument is a function declaration.
    isFuncDecl (Decl _ (FuncBinding {}) _) = True
    isFuncDecl _ = False
    -- Returns True if its argument is a value declaration.
    isValDecl = not . isFuncDecl
    -- Returns True if its argument has more than one node and at least one is a value declaration.
    isCyclic tree = (length tree > 1) && any isValDecl tree
    -- Extracts declaration from adjacency list node.
    declFromNode (decl, _, _) = decl