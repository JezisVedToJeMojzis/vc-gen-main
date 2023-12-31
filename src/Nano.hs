module Nano
  ( Nano
  , Function (..)
  , Statement (..)
  , skip
  , seq
  , VCGen (..)
  , check
  ) where

import qualified Prelude
import Prelude hiding (and, or, seq)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (foldrM)

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import qualified SMT
import Expr
import Logic

-- | A full Nano program.
type Nano a = [Function a]

-- | A function in Nano
data Function a = Function
  { fname :: a
  -- ^ Function name
  , fargs :: [a]
  -- ^ Function arguments
  , fbody :: Statement a
  -- ^ Function body
  , fpre :: Logic a
  -- ^ Pre condition of the function
  , fpost :: Logic a
  -- ^ Post condition of the function
  , fmods :: [a]
  -- ^ The variables this function modifies
  }
  deriving (Eq, Ord, Show)

-- | Nano statement
data Statement a
  = Seq [Statement a]
  -- ^ body0; body1; ... bodyN;
  | If (Logic a) (Statement a) (Statement a)
  -- ^ If conditional body0 body1
  | While (Logic a) (Logic a) (Statement a)
  -- ^ While invariant conditional body
  | Return (Expr a)
  -- ^ Return expr
  | Assume (Logic a)
  -- ^ Assume pred
  | Assert (Logic a)
  -- ^ Assert pred
  | Assign a (Expr a)
  -- ^ x := e
  | ArrAsn a (Expr a) (Expr a)
  -- ^ x[i] := e
  | AppAsn a a [Expr a]
  -- ^ x := f(e0, .., eN)
  | Havoc a
  -- ^ havoc
  | LoadPtr a  a 
  -- ^ y := *x (load) 
  | StorePtr a (Expr a) (Expr a) 
  -- ^ *x := e (store)
  deriving (Eq, Ord, Show)

instance Semigroup (Statement a) where
  lhs <> rhs = seq [lhs, rhs]

instance Monoid (Statement a) where
  mempty = skip

-- | Skip; essentially a No-Op
skip :: Statement a
skip = Seq []

-- | Seq (removes nested sequences)
seq :: [Statement a] -> Statement a
seq = unflatten . mconcat . (flatten <$>)
  where
    unflatten [s] = s
    unflatten s = Seq s

    flatten (Seq s) = s
    flatten s = [s]

--type MonadVCGen a m = (Ord a, MonadWriter (Logic a) m)
type MonadVCGen a m = (Ord a, MonadWriter (Logic a) m, MonadReader (Info a) m, MonadState Integer m)

-- | Reader info to lookup function information.
data Info a = Info
  { ifunc :: a
  -- ^ The current function we are checking
  , iprog :: Map a (Function a)
  -- ^ The whole program, indexed by function names
  }

-- | Lookup a function. Usefull for getting function contracts
lookupFunc :: MonadVCGen a m => a -> m (Function a)
lookupFunc name = do
  prog <- reader iprog
  return $ prog Map.! name

-- | Lookup the current function
currentFunc :: MonadVCGen a m => m (Function a)
currentFunc = reader ifunc >>= lookupFunc

-- | Get a fresh variable
fresh :: MonadVCGen String m => m String
fresh = state (\s -> ("$fresh" <> show s, s + 1))

-- | The return variable
result :: String
result = "$result"

-- | Global arr
memory :: String
memory = "$memory"

x :: String
x = "x"

stringToExpr :: String -> Expr String
stringToExpr str = Var str

exprToString :: Expr String -> String
exprToString (Var str) = str


-- | Generate verification conditions from this structure
class VCGen f where
  vcgen :: MonadVCGen String m => f String -> Logic String -> m (Logic String)

-- | Function `vcgen` implements weakest-precondition based program
-- verification, as discussed in class. It takes as input the current statement
-- and postcondtion, and returns the weakest precondition. See also slide 40
-- lecture 6.
--
-- Remember that our weakest-precondition rule for loops required us
-- to check some additional conditions on the invariants (function `vcs` in the
-- slides). In your code, you can save these additional verification conditions
-- in the background reader monad via `tell`.
--
-- These additional conditions will be checked together with the weakest
-- precondition produced by your implementation.
--
-- We recommend first implementing the basic algebra nano commands, such as Seq,
-- If Assume, Assert, Assign and ArrAsn, after which you can implement While.
--
-- If you have done all these steps, then you can start with the function
-- contract statements: Havoc, Return and AppAsn.
--
-- Of this, Havoc is probably the easiest, followed by Return. Note that Havoc
-- is never generated from the parser directly. It can however be usefull for
-- AppAsn.
--
-- For Return and AppAsn, we use the 'result' function to name the output of
-- a function.
--
-- You can use 'fresh' to generate a fresh variable name.

-- the function takes as argument a Nano statement and a post condition. 
-- From here, it should compute the weakest precondition for this statement. 
-- I would recommend starting with a structure like so:

-- vcgen (Assert l) post = ...
-- vcgen (Assume l) post = ...
-- vcgen _ _ = return false
instance VCGen Statement where

  -- Seq stmt
  vcgen (Seq []) post = return post
  vcgen (Seq stmts) post = foldrM (\stmt accPost -> vcgen stmt accPost) post stmts

  -- if else
  vcgen (If cond thenStmt elseStmt) post = do
    preThen <- vcgen thenStmt post
    preElse <- vcgen elseStmt post
    let preThen' = implies cond preThen
    let preElse' = implies (neg cond) preElse
    let pre = and [preThen', preElse']
    return pre

  -- Assume (P implies Q)
  vcgen (Assume l) post = do
    let pre = implies l post
    return pre

  -- Assert (P and Q)
  vcgen (Assert l) post = do
    let pre = and [post, l]
    return pre

 -- Assignment (subst)
  vcgen (Assign var expr) post = do
    let pre = subst var expr post
    return pre

  -- Array Assignment
  vcgen (ArrAsn array index expr) post = do
    let pre = subst array (Store (Array array) index expr) post
    return pre
  
  -- Havoc: This is similar to a forall. We need to universally quantify the variable and update the postcondition accordingly.
  vcgen (Havoc var) post = do
    let pre = Forall var post
    return pre

    
  -- vcgen (Havoc var) post = do
  --   let havocVars = var -- Assuming var is a single variable to be universally quantified
  --   let preHavoc = foldr (\v acc -> Forall [v] acc) (post :: Logic String) havocVars -- Ensure the type is consistent
  --   return preHavoc

  -- Return
  vcgen (Return expr) post = do
    func <- currentFunc
    let post' = subst result expr (fpost func)
    return post'
    
  -- 1.
  -- Load (⊢{Q[μ[y]/x]} x:= ∗y{Q})
  -- vcgen (LoadPtr array index expr) post = do -- LoadPtr "x" (Var "memory") (Var "ptr")
  --   let pre = subst x (Store (Array x) expr expr) post -- x[ptr] = ptr
  --   return pre
  
  -- -- Store (⊢{Q[μ⟨x◁e⟩/μ]} ∗x := e {Q})
  -- vcgen (StorePtr lhs rhs) post = do -- StorePtr "ptr" (Const 5)
  --   let pre = subst x (Store (Array x) (Var lhs) rhs) post -- x[ptr] = 5
  --   return pre 

  -- 2.
  -- Load (⊢{Q[μ[y]/x]} x:= ∗y{Q})
  -- vcgen (LoadPtr lhs rhs) post = do -- LoadPtr "x" "ptr"
  --   let pre = subst memory (Var rhs) post -- memory = Var "ptr"
  --   return pre
  
  -- -- Store (⊢{Q[μ⟨x◁e⟩/μ]} ∗x := e {Q})
  -- vcgen (StorePtr lhs rhs) post = do -- StorePtr "ptr" (Const 5)
  --   let pre = if (Var lhs) == (Var memory) -- Both memory and lhs = "ptr"
  --          then subst x rhs post -- x = 5
  --          else post
  --   return pre 

  -- 3.
  -- Load (⊢{Q[μ[y]/x]} x:= ∗y{Q})
  -- vcgen (LoadPtr lhs rhs) post = do -- LoadPtr "x" "ptr"
  --   let pre = subst memory (Store (Array memory) (Var rhs) (Var lhs)) post -- memory = Var "ptr"
  --   return pre
  
  -- -- Store (⊢{Q[μ⟨x◁e⟩/μ]} ∗x := e {Q})
  -- vcgen (StorePtr lhs rhs) post = do -- StorePtr "ptr" (Const 5)
  --   let searchMemory = Select (Array memory) (Var lhs)
  --   let pre = if searchMemory == Select (Store (Array memory) (Var lhs) (Var x)) (Var lhs)
  --           then subst x rhs post
  --           else subst x searchMemory post
  --   return pre 

 -- 4.
 -- Load (⊢{Q[μ[y]/x]} x:= ∗y{Q})
  -- vcgen (LoadPtr lhs rhs) post = do -- LoadPtr "x" "ptr"
  --   let pre = subst memory (Store (Array memory) (Var rhs) (Var lhs)) post -- memory[ptr] = Var "x"
  --   return pre
  
  -- -- Store (⊢{Q[μ⟨x◁e⟩/μ]} ∗x := e {Q})
  -- vcgen (StorePtr array index expr) post = do -- StorePtr "ptr" Var "x" (Const 5)
  --   let pre = if (Var memory) == Store (Array memory) (Var array) (index) -- if (memory[ptr] = x) == (memory[ptr] = x)
  --           then subst x expr post -- x = 5
  --           else post
  --   return pre 

-- 5.
-- Load (⊢{Q[μ[y]/x]} x:= ∗y{Q})
--   vcgen (LoadPtr lhs rhs) post = do -- LoadPtr "x" "ptr"
--     let pre = subst memory (Store (Array memory) (Var rhs) (Var lhs)) post -- memory[ptr] = x
--     return pre
  
-- -- Store (⊢{Q[μ⟨x◁e⟩/μ]} ∗x := e {Q})
--   vcgen (StorePtr array index expr) post = do -- StorePtr "ptr" Var "x" (Const 5)
--     let searchMemory = Select (Array memory) (Var array) -- Select memory[ptr]
--     let pre = if searchMemory == index -- if Select memory[ptr] == x
--             then subst x expr post -- x = 5
--             else subst x (Var memory) post
--     return pre 

-- 6.
-- Load (⊢{Q[μ[y]/x]} x:= ∗y{Q})
--   vcgen (LoadPtr lhs rhs) post = do -- LoadPtr "x" "ptr"
--     let pre = subst memory (Var lhs) post -- memory = x
--     return pre
  
-- -- Store (⊢{Q[μ⟨x◁e⟩/μ]} ∗x := e {Q})
--   vcgen (StorePtr array index expr) post = do -- StorePtr "ptr" Var "x" (Const 5)
--     let pre = if (Var memory) == index -- if memory == x
--             then subst memory expr post -- x == 5
--             else post
--     return pre 

-- 7.
-- Load (⊢{Q[μ[y]/x]} x:= ∗y{Q})
  vcgen (LoadPtr lhs rhs) post = do -- LoadPtr "x" "ptr"
    let pre = subst memory (Store (Array memory) (Var rhs) (Var lhs)) post -- memory[ptr] = x
    return pre -- Store (Array "$memory") (Var "ptr") (Var "x")
  
-- Store (⊢{Q[μ⟨x◁e⟩/μ]} ∗x := e {Q})
  vcgen (StorePtr array index expr) post = do -- StorePtr "ptr" Var "x" (Const 5)
    let tmp = Store (Array memory) (Var array) expr -- memory[ptr] = 5 *or* Store (Array "memory") (Var "ptr") (Const 5)
    let pre = if index == (Var x) -- if index of ptr == x
            then subst x tmp post 
            else post
    return pre -- Store (Store (Array "$memory") (Var "ptr") (Var "x")) (Var "ptr") (Const 5)

-- 8.
-- Load (⊢{Q[μ[y]/x]} x:= ∗y{Q})
--   vcgen (LoadPtr lhs rhs) post = do -- LoadPtr "x" "ptr"
--     let pre = subst memory (Var lhs) post -- memory = x
--     return pre -- memory = x
  
-- -- Store (⊢{Q[μ⟨x◁e⟩/μ]} ∗x := e {Q})
--   vcgen (StorePtr array index expr) post = do -- StorePtr "ptr" Var "x" (Const 5)
--     let tmp = Store (Array memory) (Var array) expr -- x[ptr] = 5 *or* Store (Array "x") (Var "ptr") (Const 5)
--     let pre = if index == (Var x) -- if index of ptr == x
--             then subst x tmp post 
--             else post
--     return pre -- Store (Var "x") (Var "ptr") (Const 5)


 -- Catch statement
 -- vcgen _ _ = return false

instance VCGen Function where
  vcgen func post' = do
    let pre = Assume $ fpre func
    let body = fbody func
    let post = Assert $ fpost func
    let stmt = seq [pre, body, post]
    vcgen stmt post'

-- | Check validity
check :: Nano String -> IO Bool
check nano = do
  let progmap = Map.fromList $ (\f -> (fname f, f)) <$> nano
  let runner fun = runWriter . flip runReaderT (Info (fname fun) progmap) . flip evalStateT 0
  let check' fun = let (pre, vcs) = runner fun $ vcgen fun true in pre <> vcs
  let vcs = check' <$> nano
  mapM_ (putStrLn . show) vcs -- for debugging
  valid <- mapM SMT.valid vcs
  return $ Prelude.and valid