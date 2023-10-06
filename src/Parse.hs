module Parse
  ( Constraints
  , nano
  , function
  , statement
  , logic
  , predicate
  , expr
  ) where

import qualified Language.ECMAScript3.Syntax as JS
import qualified Language.ECMAScript3.Parser as JS
import Data.Composition

import Control.Monad.State
import Control.Applicative (empty, Alternative)
import Prelude hiding (seq, and, or)

import Expr
import Logic
import Nano

data Constraints a = Constraints
  { invariant :: Logic a
  , require :: Logic a
  , ensure :: Logic a
  , modifies :: [a]
  }
  deriving (Show, Eq, Ord)

instance Semigroup (Constraints a) where
  Constraints i r e m <> Constraints i' r' e' m' 
    = Constraints (i <> i') (r <> r') (e <> e') (m <> m')

instance Monoid (Constraints a) where
  mempty = Constraints mempty mempty mempty mempty

type MonadNano a m = (MonadState (Constraints a) m, Alternative m)

-- | Parses Javascript and converts it into Nano.
nano :: String -> IO (Maybe (Nano String))
nano path = do
  JS.Script _ stmts <- JS.parseFromFile path
  return $ mapM function stmts

-- | Converts a JS function statement into a function.
function :: JS.Statement a -> Maybe (Function String)
function (JS.FunctionStmt _ (JS.Id _ name) args body) = do
  (body', constraints) <- flip runStateT mempty $ mapM statement body
  let args' = (\(JS.Id _ v) -> v) <$> args
  return $ Function 
    { fname = name
    , fargs = args'
    , fbody = seq body'
    , fpre = require constraints
    , fpost = ensure constraints
    , fmods = modifies constraints
    }
function _ = empty

-- | Converts JS into Nano.
--
-- This should convert the following subset of JS into Nano statements:
-- - Empty statement
-- - Return
-- - Assignment (both 'x := expr' and 'arr[i] := expr')
-- - Variable declaration (only when it assigns a value)
-- - Block statement
-- - If statement (with and without else)
-- - While statement (check `scopeInv`)
-- - Specialise function with the names "assume", "assert", "invariant", 
--   "requires" and "ensures". To perform appropriate actions.

helpWDecl :: MonadNano String m => JS.VarDecl a -> m (Statement String)
helpWDecl (JS.VarDecl _ (JS.Id _ var) (Just rhs)) = do
  rhs' <- expr rhs -- if rhs is valid it parses it to nano
  return $ Assign var rhs' -- rhs is assigned to var name
helpWDecl _ = return $ skip -- else Seq [] (empty sequence of stmts)

-- convert id to string
convertIdToString :: JS.Id a -> String
convertIdToString (JS.Id _ s) = s

statement :: MonadNano String m => JS.Statement a -> m (Statement String)
-- EmptyStmt a
statement (JS.EmptyStmt _) = return (skip)  -- Empty statement (skip = Seq [] = empty sequence of statements)

-- Return
statement (JS.ReturnStmt _ (Just expression)) = do -- ReturnStmt a (Maybe (Expression a)) // return expr;, spec 12.9
  expr' <- expr expression  -- parsing js expr into nano expr
  return $ Return expr'  -- nano return

-- Assignment
statement (AssignStmt var rhs) = do
  case rhs of
    JS.CallExpr _ (JS.VarRef _ fName) args -> do
      args' <- mapM expr args
      return $ AppAsn var (convertIdToString fName) args'  -- Extract the string value and assign the result of the function call to the variable
    _ -> do
      rhs' <- expr rhs  -- parse rhs into nano
      return $ Assign var rhs'  -- rhs is assigned to var name

-- Variable declaration
statement (JS.VarDeclStmt _ declarations) = do -- VarDeclStmt a [VarDecl a]	// var x, y=42;, spec 12.2
  statements <- mapM helpWDecl declarations -- apply helpWDecl fun to each decl
  return $ Seq statements 

-- Block statement
statement (JS.BlockStmt _ stmts) = do -- block of statements {} // BlockStmt a [Statement a] // {stmts}, spec 12.1
  stmts' <- mapM statement stmts -- mapping statement to each statement in stmts (getting nano statements)
  return $ Seq stmts' -- combining and returning the statements as nano block 

-- If statement
statement (IfStmt conditional body0 body1) = do
  cond' <- logic conditional -- Convert the condition expression to Nano logic
  body0' <- statement body0 -- Convert the bodies of the if and else branches to Nano statements
  body1' <- statement body1 
  return $ If cond' body0' body1' -- Return a Nano If statement

-- If single statement (no else branch)
statement (IfSingleStmt conditional body) = do
  cond' <- logic conditional -- Convert the condition expression to Nano logic
  body' <- statement body -- Convert the body of the if branch to Nano statements
  return $ If cond' body' skip -- Return a Nano If statement with no else branch
  
-- While statement
statement (JS.WhileStmt _ expr stmt) = do  -- WhileStmt a (Expression a) (Statement a) // while (e) do stmt, spec 12.6
  expr' <- logic expr
  (stmt', inv) <- scopeInv (statement stmt)  -- handle inv
  return $ While inv expr' stmt'

  -- Assume
statement (JS.ExprStmt _ (JS.CallExpr _ (JS.VarRef _ (JS.Id _ "assume")) [stmt])) = do -- ExprStmt a (Expression a) // expr;, spec 12.4
  stmt' <- logic stmt -- parsed stmt
  return $ Assume stmt'  -- nano assume

-- Assert
statement (JS.ExprStmt _ (JS.CallExpr _ (JS.VarRef _ (JS.Id _ "assert")) [stmt])) = do 
  stmt' <- logic stmt  -- parsed stmt
  return $ Assert stmt' -- nano assert 

-- Invariant
statement (JS.ExprStmt _ (JS.CallExpr _ (JS.VarRef _ (JS.Id _ "invariant")) [stmt])) = do
  stmt' <- logic stmt  -- parse stmt
  addInvariant stmt'  
  return $ skip

-- Requires
statement (JS.ExprStmt _ (JS.CallExpr _ (JS.VarRef _ (JS.Id _ "requires")) [stmt])) = do
  stmt' <- logic stmt
  addRequire stmt'  -- Add the requirement to the upper function's contract
  return $ skip

-- Ensures
statement (JS.ExprStmt _ (JS.CallExpr _ (JS.VarRef _ (JS.Id _ "ensures")) [stmt])) = do
  stmt' <- logic stmt
  addEnsure stmt'  -- Add the postcondition to the upper function's contract
  return $ skip

-- TODO change to "empty" later, skip is for debugging
statement _ = empty

-- | Helper function to scope invariant fetching to a block.
scopeInv :: MonadNano String m => m a -> m (a, Logic String)
scopeInv m = do
  outer <- state $ \cons -> (invariant cons, cons { invariant = true })
  result <- m
  inv <- state $ \cons -> (invariant cons, cons { invariant = outer })
  return (result, inv)

-- | Helper function to add an invariant to the upper while.
addInvariant :: MonadNano a m => Logic a -> m ()
addInvariant l = modify (mempty { invariant = l } <>)

-- | Helper function to add an require to the upper function.
addRequire :: MonadNano a m => Logic a -> m ()
addRequire l = modify (mempty { require = l } <>)

-- | Helper function to add an ensure to the upper function.
addEnsure :: MonadNano a m => Logic a -> m ()
addEnsure l = modify (mempty { ensure = l } <>)


addModifies :: MonadNano a m => a -> m ()
addModifies x = modify (mempty { modifies = [x] } <>)


-- | Converts JS into Nano logic.
--
-- This should convert the following subset of JS into Nano expressions:
-- - Boolean literals
-- - Conjuncts and Disjuncts
-- - Negation
-- - Functions called "forall" or "exists" with two arguments (of which the
--   first a variable) into its respective quantifier. 
--   Hint: check 'CallExpr'
-- - Remaining expressions should become predicates (if possible)
multipleLAnds :: Logic String -> Bool
multipleLAnds (And _) = True
multipleLAnds _ = False

logic :: MonadNano String m => JS.Expression a -> m (Logic String)
logic (JS.PrefixExpr _ JS.PrefixLNot (JS.BoolLit _ b)) = return $ if b then false else true -- negated bool
logic (JS.BoolLit _ b) = return $ if b then true else false -- not negated bool

logic (JS.InfixExpr _ op lhs rhs) = case op of
  JS.OpLAnd -> do -- &&
    lhs' <- logic lhs
    rhs' <- logic rhs
    return (and [lhs', rhs'])
  JS.OpLOr -> do -- ||
    lhs' <- logic lhs
    rhs' <- logic rhs
    return $ or [lhs', rhs']
  JS.OpGEq -> do -- >=
    lhs' <- expr lhs
    rhs' <- expr rhs
    return $ Pred (lhs' :>=: rhs')
  JS.OpEq -> do -- ==
    lhs' <- expr lhs
    rhs' <- expr rhs
    return $ Pred (lhs' :==: rhs')
  JS.OpNEq -> do -- !=
    lhs' <- expr lhs
    rhs' <- expr rhs
    return $ Neg (Pred (lhs' :==: rhs'))
  JS.OpLEq -> do -- <=
    lhs' <- expr lhs
    rhs' <- expr rhs
    return $ Pred (lhs' :<=: rhs')
  _ -> empty

-- Handle negation
logic (Negate e) = do
  e' <- logic e
  return $ Neg e'

-- Handle quantifiers: forall
logic (Call "forall" [Variable var, expr]) = do
  expr' <- logic expr
  return $ Forall var expr'

-- Handle quantifiers: exists
logic (Call "exists" [Variable var, expr]) = do 
  expr' <- logic expr
  return (exists var expr')
  
logic _ = empty  


-- | Converts JS into Nano expressions of type Bool
--
-- This should convert the following subset of JS into Nano expressions:
-- - All (strict) (in)equalities
--
-- Notice how we return Logic here, this is because we express some of the
-- operations via a negation of a predicate.
predicate :: MonadNano String m => JS.Expression a -> m (Logic String)
predicate (JS.InfixExpr _ op lhs rhs) = case op of
  JS.OpEq -> do -- ==
    lhs' <- expr lhs -- convert js into nano
    rhs' <- expr rhs
    return $ Pred (lhs' :==: rhs')  -- return nano in needed "format"
  JS.OpNEq -> do -- !=
    lhs' <- expr lhs
    rhs' <- expr rhs
    return $ Neg (Pred (lhs' :==: rhs'))  
  JS.OpLT -> do -- <
    lhs' <- expr lhs
    rhs' <- expr rhs
    return $ Pred (lhs' :<=: rhs')
  JS.OpLEq -> do -- <=
    lhs' <- expr lhs
    rhs' <- expr rhs
    return $ Pred (lhs' :<=: rhs')
  JS.OpGT -> do -- >
    lhs' <- expr lhs
    rhs' <- expr rhs
    return $ Neg (Pred (lhs' :<=: rhs'))
  JS.OpGEq -> do -- >=
    lhs' <- expr lhs
    rhs' <- expr rhs
    return $ Pred (lhs' :>=: rhs')
  _ -> empty

predicate _ = empty

-- | Converts JS into Nano expressions of type Int
--
-- This should convert the following subset of JS into Nano expressions:
-- - Integer literals
-- - Variables
-- - Array indexing (the array itself may be just a variable)
-- - Binary arithmetic
-- - Unary minus
--
-- You can look up the types of a JS.Expression in their docs.
-- For an example of a variable pattern match, check out the 'Variable'
-- pattern below. You are free to add more patterns like this, or just straight
-- up pattern match against the code like you would normally.
--
-- Below, you will find a bunch of patterns which you can use to implement
-- the parser. You can essentially implement the functions above with just
-- these patterns, with the exception of having to match on the JS operators.
--
-- If you're curious about what these patterns are exactly, you can look up the
-- PatternSynonyms Haskell pragma.
--
-- You can use these patterns as follows:
-- expr (Variable var) = ...
--
-- Here, 'var' will be of type String, as dictated by the pattern.
--
-- Note that all these expressions contain location information, you may just
-- discard this with a '_' in your pattern match. 
-- Note that you will have to us every pattern at least once (and some multiple
-- times), unless stated otherwise.
--
-- If you miss a case that you should parse, check out what the JavaScript 
-- parser will produce by running it separately. This way, you could find
-- the culprit expression.

expr :: MonadNano String m => JS.Expression a -> m (Expr String)
expr (Variable x) = return (Var x) -- js var into nano
expr (Int i) = return (Const (fromIntegral i)) -- js integer into nano

expr (InfixExpr lhs op rhs) = do -- for operations
  lhs' <- expr lhs
  rhs' <- expr rhs
  case op of
    JS.OpAdd -> return (BinOp Add lhs' rhs')  
    JS.OpMul -> return (BinOp Mul lhs' rhs')  
    JS.OpDiv -> return (BinOp Div lhs' rhs')  
    JS.OpMod -> return (BinOp Mod lhs' rhs')  
    JS.OpSub -> return (BinOp Sub lhs' rhs')
    _ -> empty

-- unary minus
expr (Minus e) = do -- for e.g. -3
   e' <- expr e -- convert into nano
   return  (BinOp Sub (Const 0) e') -- 0 - e' = - e'

expr (JS.BracketRef _ (JS.VarRef _ (JS.Id _ array)) index) = do -- for arrays e.g. x[i]
  array' <- return (Array array) -- nano array
  index' <- expr index -- parse 
  return (Select array' index') -- select element from array 

expr _ = empty

-- | You can use this to pattern match on a variable.
-- For more info on this, search for the PatternSynonyms language pragma.
--
-- Feel free to add more patterns if you wish!
pattern Variable :: String -> JS.Expression a
pattern Variable x <- JS.VarRef _ (JS.Id _ x)

pattern Int :: Int -> JS.Expression a
pattern Int i <- JS.IntLit _ i

pattern Bool :: Bool -> JS.Expression a
pattern Bool b <- JS.BoolLit _ b

pattern Minus :: JS.Expression a -> JS.Expression a
pattern Minus e <- JS.PrefixExpr _ JS.PrefixMinus e

pattern Negate :: JS.Expression a -> JS.Expression a
pattern Negate e <- JS.PrefixExpr _ JS.PrefixLNot e

pattern ArrayIndex :: String -> JS.Expression a -> JS.Expression a
pattern ArrayIndex array index <- JS.BracketRef _ (Variable array) index

pattern InfixExpr :: JS.Expression a -> JS.InfixOp -> JS.Expression a -> JS.Expression a
pattern InfixExpr lhs op rhs <- JS.InfixExpr _ op lhs rhs

pattern Call :: String -> [JS.Expression a] -> JS.Expression a
pattern Call name arguments <- JS.CallExpr _ (Variable name) arguments

pattern CallStmt :: String -> [JS.Expression a] -> JS.Statement a
pattern CallStmt name arguments <- JS.ExprStmt _ (Call name arguments)

pattern WhileStmt :: JS.Expression a -> JS.Statement a -> JS.Statement a
pattern WhileStmt conditional body <- JS.WhileStmt _ conditional body

pattern IfStmt :: JS.Expression a -> JS.Statement a -> JS.Statement a -> JS.Statement a
pattern IfStmt conditional body0 body1 <- JS.IfStmt _ conditional body0 body1

pattern IfSingleStmt :: JS.Expression a -> JS.Statement a -> JS.Statement a
pattern IfSingleStmt conditional body <- JS.IfSingleStmt _ conditional body

pattern BlockStmt :: [JS.Statement a] -> JS.Statement a
pattern BlockStmt body <- JS.BlockStmt _ body

pattern EmptyStmt :: JS.Statement a
pattern EmptyStmt <- JS.EmptyStmt _

pattern ReturnStmt :: JS.Expression a -> JS.Statement a
pattern ReturnStmt expr <- JS.ReturnStmt _ (Just expr)

-- | This is a helper for the other assign statements, you do not have to use
-- this directly.
pattern AssignStmt' :: JS.LValue a -> JS.Expression a -> JS.Statement a
pattern AssignStmt' lhs rhs <- JS.ExprStmt _ (JS.AssignExpr _ JS.OpAssign lhs rhs)

-- | You still have to distinguish between an expression or function call on
-- the rhs when using this pattern.
pattern AssignStmt :: String -> JS.Expression a -> JS.Statement a
pattern AssignStmt var rhs <- AssignStmt' (JS.LVar _ var) rhs

pattern ArrAsnStmt :: String -> JS.Expression a -> JS.Expression a -> JS.Statement a
pattern ArrAsnStmt array index rhs <- AssignStmt' (JS.LBracket _ (Variable array) index) rhs

pattern DeclStmt :: [JS.VarDecl a] -> JS.Statement a
pattern DeclStmt statements <- JS.VarDeclStmt _ statements

pattern Decl :: String -> JS.Expression a -> JS.VarDecl a
pattern Decl var expr <- JS.VarDecl _ (JS.Id _ var) (Just expr)