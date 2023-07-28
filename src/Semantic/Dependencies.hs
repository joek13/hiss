{-
    1. Generation of program declaration dependency graph.
    2. Semantic pass to detect value cycles.
-}

module Semantic.Dependencies (reorderDecls) where

import Data.Foldable (find)
import Data.Foldable qualified as Foldable (toList)
import Data.Function ((&))
import Data.Graph (Graph, Vertex, graphFromEdges, scc)
import Data.Map (Map)
import Data.Map qualified as Map (fromList, lookup)
import Data.Maybe (catMaybes, fromJust)
import Data.Set ((\\))
import Data.Set qualified as Set (fromList, map, singleton, toList, union)
import Error (HissError (SemanticError))
import Semantic.Names (collectNames)
import Syntax (getLineCol, start)
import Syntax.AST (Annotated (getAnn), Binding (..), Decl (..), Name (..), Program (..), declGetName, getIdent, progDecls)
import Syntax.Lexer (Range)

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

-- | Groups a program's declarations into sets of mutually dependent declarations.
--   Equivalently, finds strongly connected components of the program dependency graph.
--   Returns list of strongly connected components in reverse topological order.
groupDecls :: Program a -> [[Decl a]]
groupDecls prog =
  let (graph, declFromVertex) = dependencyGraph prog
      sccs =
        scc graph -- find strongly connected components (SCCs)
          & map (fmap declFromVertex) -- convert from Forest Vertex -> Forest (Decl Range)
   in map Foldable.toList sccs

-- | Reorganizes a program's declarations into dependency order.
--   Reorders declarations so that values are always defined lexically before they are used.
--   If this is impossible (i.e., value has cyclic definition), returns an error.
reorderDecls :: Program Range -> Either HissError (Program Range)
reorderDecls prog =
  case find isCyclic groupedDecls of
    Just group ->
      -- if yes, find a value to report (first value might be a function)
      let decl = fromJust $ find isValDecl group
          name = declGetName decl
          (line, col) = (getLineCol . start . getAnn) name
       in Left $ SemanticError $ "Definition of '" <> getIdent name <> "' at line " <> show line <> ", column " <> show col <> " is cyclic"
    Nothing -> return $ Program (getAnn prog) $ concat groupedDecls
  where
    groupedDecls = groupDecls prog
    --  Returns True if its argument is a function declaration.
    isFuncDecl (Decl _ (FuncBinding {}) _) = True
    isFuncDecl _ = False
    -- Returns True if its argument is a value declaration.
    isValDecl = not . isFuncDecl
    {-
      in general, a graph is acyclic <=> each SCC contains exactly one node.
      we only care about detecting cyclic definitions for values
      (cyclic function definitions are just mutual recursion, which is allowed)

      isCyclic returns True if its argument has more than one node and at least one is a value declaration.
    -}
    isCyclic group = (length group > 1) && any isValDecl group