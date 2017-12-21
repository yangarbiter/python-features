{-# LANGUAGE LambdaCase #-}
module NanoML.Classify where

import Control.Monad
import Data.Monoid
import qualified Data.Set as Set
import Data.Set (Set)

import Language.Python.Common hiding ((<>))


data ES = Ex ExprSpan | St StatementSpan deriving (Show)

data Literal
  = LI Integer
  | LD Double
  | LB Bool
  | LC Char
  | LS String
  deriving (Show, Eq)

foldExpr :: Monoid a => (ExprSpan -> a -> a) -> a -> ExprSpan -> a
foldExpr f z = go
  where
  go e = f e $ case e of
    Var {} -> z
    Int {} -> z
    LongInt {} -> z
    Float {} -> z
    Imaginary {} -> z
    Bool {} -> z
    None {} -> z
    -- Ellipsis
    ByteStrings {} -> z
    Strings {} -> z
    UnicodeStrings {} -> z
    Call e args _ -> mconcat $ map go (e:(argExpr <$> args))
    Subscript x y _ -> mappend (go x) (go y)
    -- SlicedExpr
    CondExpr t b f _ -> go b <> go t <> go f
    BinaryOp _ x y _ -> mappend (go x) (go y)
    UnaryOp _ x _ -> go x
    -- Dot
    Lambda _ e _ -> go e
    Tuple es _ -> mconcat (map go es)
    -- Yield
    -- Generator
    -- ListComp
    List es _ -> mconcat (map go es)
    -- Dictionary
    -- DictComp
    Set es _ -> mconcat (map go es)
    -- SetComp
    -- Starred
    Paren e _ -> go e
    StringConversion e _ -> go e
    e -> error $ "unhandled case of foldExpr: " ++ (show e)

diff :: ExprSpan -> ExprSpan -> Set SrcSpan
diff e1 e2 = case (e1, e2) of
  (Var x _, Var y _)
    | x == y
      -> mempty
  (Int i1 s1 _, Int i2 s2 _) --TODO could the string versions matter?
    | i1 == i2
      -> mempty
  (LongInt i1 s1 _, LongInt i2 s2 _)
    | i1 == i2
      -> mempty
  (Float i1 s1 _, Float i2 s2 _)
    | i1 == i2
      -> mempty
  (Imaginary i1 s1 _, Imaginary i2 s2 _)
    | i1 == i2
      -> mempty
  (Bool i1 _, Bool i2 _)
    | i1 == i2
      -> mempty
  (None _, None _)
      -> mempty
  -- Ellipsis
  (ByteStrings i1 _, ByteStrings i2 _)
    | i1 == i2
      -> mempty
  (Strings i1 _, Strings i2 _)
    | i1 == i2
      -> mempty
  (UnicodeStrings i1 _, UnicodeStrings i2 _)
    | i1 == i2
      -> mempty
  (Call f1 a1 _, Call f2 a2 _)
    | length a1 == length a2
      -> merge $ (diff f1 f2) : (zipWith diff (argExpr <$> a1) (argExpr <$> a2))
  -- Subscript x y _ ->
  -- -- SlicedExpr
  -- CondExpr t b f _ ->
  (BinaryOp bx x1 x2 _, BinaryOp by y1 y2 _)
    | bx == by
      -> merge [diff x1 y1, diff x2 y2]
  (UnaryOp ux x _, UnaryOp uy y _)
    | ux == uy
      -> diff x y
  -- Dot
  -- Lambda _ e _
  -- Tuple es _
  -- Yield
  -- Generator
  -- ListComp
  -- List es _
  -- Dictionary
  -- DictComp
  -- Set es _
  -- SetComp
  -- Starred
  -- Paren e _
  -- StringConversion e _
  -- _ -> error $ "unhandled case of diff: " ++ (show e1) ++ (show e2)
  _ -> Set.singleton $ annot e1 --TODO as new exprs are supported, remember to add them here!!
  where
    merge = mconcat

type TExpr = ExprSpan

ctfold :: Monoid a => (ES {- parent -} -> ES -> a -> a) -> a -> ES -> a
ctfold f z r = go r r
  where
  go p x' = f p x' $ case x' of
    St s' -> case s' of
      Import {} -> z
      FromImport {} -> z
      While cond body els _ -> mconcat (map (go x') ((Ex cond) : (St <$> (body ++ els))))
      For vs gen body els _ -> mconcat (map (go x') ((Ex <$> vs ++ [gen]) ++ (St <$> (body ++ els))))
      Fun _ _ Nothing body _ -> mconcat (map (go x') (St <$> body)) --TODO we do not yet support annotations or default values on function Parameters (or result annotations)
      -- Class _ args body _ -> (Ex <$> (argExpr <$> args)) ++ (St <$> body)
      -- Conditional gs els _ -> concatMap (\(e, ss) -> [Ex e] ++ (St <$> ss)) gs ++ (St <$> els)
      Assign xs e _ -> mconcat (map (go x') (Ex <$> (xs ++ [e])))
      AugmentedAssign x _ e _ -> mconcat (map (go x') (Ex <$> [x,e]))
      --Decorated
      Return Nothing _ -> z
      Return (Just e) _ -> go x' (Ex e)
      --Try
      --Raise
      --With
      Pass _ -> z
      Break _ -> z
      Continue _ -> z
      Delete es _ -> mconcat (map (go x') (Ex <$> es))
      StmtExpr e _ -> go x' (Ex e)
      Global _ _ -> z
      NonLocal _ _ -> z
      Assert es _ -> mconcat (map (go x') (Ex <$> es))
      Print _ es _ _ -> mconcat (map (go x') (Ex <$> es))
      --Exec
      e -> error $ "unhandled case of ctfold: " ++ (show e)
    Ex e' -> case e' of
      Var {} -> z
      Int {} -> z
      LongInt {} -> z
      Float {} -> z
      Imaginary {} -> z
      Bool {} -> z
      None {} -> z
      -- Ellipsis
      ByteStrings {} -> z
      Strings {} -> z
      UnicodeStrings {} -> z
      Call e args _ -> mconcat $ map (go x') (Ex <$> (e:(argExpr <$> args)))
      Subscript x y _ -> mappend (go x' (Ex x)) (go x' (Ex y))
      -- SlicedExpr
      CondExpr t b f _ -> go x' (Ex b) <> go x' (Ex t) <> go x' (Ex f)
      BinaryOp _ x y _ -> mappend (go x' (Ex x)) (go x' (Ex y))
      UnaryOp _ x _ -> go x' (Ex x)
      -- Dot
      Lambda _ e _ -> go x' (Ex e)
      Tuple es _ -> mconcat (map (go x') (Ex <$> es))
      -- Yield
      -- Generator
      -- ListComp
      List es _ -> mconcat (map (go x') (Ex <$> es))
      -- Dictionary
      -- DictComp
      Set es _ -> mconcat (map (go x') (Ex <$> es))
      -- SetComp
      -- Starred
      Paren e _ -> go x' (Ex e)
      StringConversion e _ -> go x' (Ex e)
      e -> error $ "unhandled case of ctfoldExpr: " ++ (show e)

data Diff
  = Ins ES Diff
  | Del ES Diff
  | Cpy ES Diff
  | End
  deriving Show

ins :: ES -> Diff -> Diff
ins = Ins
del :: ES -> Diff -> Diff
del = Del
cpy :: ES -> Diff -> Diff
cpy = Cpy
end :: Diff
end = End

meet :: Diff -> Diff -> Diff
meet dx dy = if cost dx <= cost dy then dx else dy

data N = Z | S N deriving (Eq, Ord)

cost :: Diff -> N
cost = \case
  Ins e d -> S $ cost d
  Del e d -> S $ cost d
  Cpy e d -> S $ cost d
  End     -> Z

showDiff :: Diff -> String
showDiff = \case
  Ins e d -> "Ins (" ++ show (exprKind e) ++ ") (" ++ showDiff d ++ ")"
  Del e d -> "Del (" ++ show (exprKind e) ++ ") (" ++ showDiff d ++ ")"
  Cpy e d -> "Cpy (" ++ show (exprKind e) ++ ") (" ++ showDiff d ++ ")"
  End     -> "End"

diffSpans :: Diff -> [ES] -> Set SrcSpan
diffSpans d' es = Set.fromList $ go d' (concatMap allSubESes es)
  where
  go _ [] = []
  go d' (x:xs) = case d' of
    Ins e d -> getSrcSpan x : go d (x:xs)
    -- TODO: should we collapse `del e (ins* es)`, e.g.
    -- if a leaf is replaced by an entire tree?
    Del e (Ins _ d) -> getSrcSpan e : go d xs
    Del e d -> getSrcSpan e : go d xs
    Cpy e d -> go d xs
    End -> [] -- WEIRD

getSrcSpan :: ES -> SrcSpan
getSrcSpan (Ex e) = annot e
getSrcSpan (St s) = annot s

allSubESes :: ES -> [ES]
allSubESes x = x : (concatMap allSubESes $ subESes x)
-- allSubESes :: ES -> [ES]
-- allSubESes (Ex e) = Ex <$> allSubExprs e
-- allSubESes (St s) = (St s) : case s of --NOTE: returns in different order?
--   Import {} -> []
--   FromImport {} -> []
--   While cond body els _ -> concatMap allSubESes $ (Ex cond) : (St <$> (body ++ els))
--   For vs gen body els _ -> concatMap allSubESes $ (Ex <$> vs ++ [gen]) ++ (St <$> (body ++ els))
--   Fun _ _ Nothing body _ -> concatMap allSubESes $ St <$> body --TODO we do not yet support annotations or default values on function Parameters (or result annotations)
--   Class _ args body _ -> concatMap allSubESes $ (Ex <$> (argExpr <$> args)) ++ (St <$> body)
--   Conditional gs els _ -> concatMap allSubESes $ concatMap (\(e, ss) -> [Ex e] ++ (St <$> ss)) gs ++ (St <$> els)
--   Assign xs e _ -> concatMap allSubESes $ (Ex <$> xs ++ [e])
--   AugmentedAssign x _ e _ -> concatMap allSubESes $ (Ex <$> [x,e])
--   -- --Decorated
--   -- Return Nothing _ -> []
--   -- Return (Just e) _ -> [Ex e]
--   -- --Try
--   -- --Raise
--   -- --With
--   -- Pass _ -> []
--   -- Break _ -> []
--   -- Continue _ -> []
--   -- Delete es _ -> Ex <$> es
--   -- StmtExpr e _ -> [Ex e]
--   -- Global _ _ -> []
--   -- NonLocal _ _ -> []
--   -- Assert es _ -> Ex <$> es
--   -- Print _ es _ _ -> Ex <$> es
--   --Exec
--
-- allSubExprs e = e : case e of
--   Var {} -> []
--   Int {} -> []
--   LongInt {} -> []
--   Float {} -> []
--   Imaginary {} -> []
--   Bool {} -> []
--   None {} -> []
--   -- Ellipsis
--   ByteStrings {} -> []
--   Strings {} -> []
--   UnicodeStrings {} -> []
--   Call e args _ -> allSubExprs e ++ concatMap allSubExprs (argExpr <$> args)
--   Subscript x y _ -> allSubExprs x ++ allSubExprs y
--   -- SlicedExpr
--   CondExpr t b f _ -> allSubExprs t ++ allSubExprs b ++ allSubExprs f
--   BinaryOp _ x y _ -> allSubExprs x ++ allSubExprs y
--   UnaryOp _ x _ -> allSubExprs x
--   -- Dot
--   Lambda _ e _ -> allSubExprs e
--   Tuple es _ -> concatMap allSubExprs es
--   -- Yield
--   -- Generator
--   -- ListComp
--   List es _ -> concatMap allSubExprs es
--   -- Dictionary
--   -- DictComp
--   Set es _ -> concatMap allSubExprs es
--   -- SetComp
--   -- Starred
--   Paren e _ -> allSubExprs e
--   StringConversion e _ -> allSubExprs e
--   e -> error $ "unhandled case of allSubExprs: " ++ (show e)

data DiffT
  = CC ES ES Diff DiffT DiffT DiffT
  | CN ES Diff DiffT
  | NC ES Diff DiffT
  | NN Diff

getDiff :: DiffT -> Diff
getDiff = \case
  CC _ _ d _ _ _ -> d
  CN _ d _ -> d
  NC _ d _ -> d
  NN d -> d

diffExprsT :: [ES] -> [ES] -> DiffT
diffExprsT [] []
  = NN end
diffExprsT (x:xss) []
  = let d = diffExprsT (subESes x ++ xss) []
    in CN x (del x (getDiff d)) d
diffExprsT [] (y:yss)
  = let d = diffExprsT [] (subESes y ++ yss)
    in NC y (ins y (getDiff d)) d
diffExprsT (x:xss) (y:yss)
  = CC x y (bestT x y i d c) i d c
  where
  xs = subESes x
  ys = subESes y

  c = diffExprsT (xs ++ xss) (ys ++ yss)
  i = extendi x c
  d = extendd y c

extendi :: ES -> DiffT -> DiffT
extendi x dt = case dt of
  NN d     -> CN x (del x d) dt
  CN _ d _ -> CN x (del x d) dt
  _        -> extracti dt $ \y dt' ->
    let i = extendi x dt'
        d = dt
        c = dt'
    in CC x y (bestT x y i d c) i d c

extracti :: DiffT -> (ES -> DiffT -> r) -> r
extracti dt k = case dt of
  CC _ y _ i _ _ -> k y i
  NC y _ i       -> k y i
  _              -> error "extracti"

extendd :: ES -> DiffT -> DiffT
extendd y dt = case dt of
  NN d     -> NC y (ins y d) dt
  NC _ d _ -> NC y (ins y d) dt
  _        -> extractd dt $ \x dt' ->
    let i = dt
        d = extendd y dt'
        c = dt'
    in CC x y (bestT x y i d c) i d c

extractd :: DiffT -> (ES -> DiffT -> r) -> r
extractd dt k = case dt of
  CC x _ _ _ d _ -> k x d
  CN x _ d       -> k x d
  _              -> error "extractd"

bestT :: ES -> ES -> DiffT -> DiffT -> DiffT -> Diff
bestT x y i d c
  | exprKind x == exprKind y
  , length (subESes x) == length (subESes y)
  = cpy x (getDiff c) -- del x (getDiff d) `meet` ins y (getDiff i)
  | otherwise
  = del x (getDiff d) `meet` ins y (getDiff i)
    -- `meet`

data ESKind
  = VarK IdentSpan
  | AppK
  | BopK OpSpan
  | UopK OpSpan
  | LitK Literal
  | IteK
  | TupleK
  | ListK
  | TerminalStatementK (Statement ())
  | AssignK
  | AugmentedAssignK
  | ReturnK
  | DeleteK
  | StmtExprK
  | AssertK
  | PrintK
  | ConditionalK Int
  | ClassK Int
  deriving (Eq, Show)

--TODO a Suite should be alowed to change size without effectively changing Kind
exprKind :: ES -> ESKind
exprKind (Ex e) = exprKind' e
exprKind (St s) = case s of
  Import {} -> TerminalStatementK $ void s
  FromImport {} -> TerminalStatementK $ void s
  -- While cond body els _ -> (Ex cond) : (St <$> (body ++ els))
  -- For vs gen body els _ -> (Ex <$> vs ++ [gen]) ++ (St <$> (body ++ els))
  -- Fun _ _ Nothing body _ -> St <$> body --TODO we do not yet support annotations or default values on function Parameters (or result annotations)
  Class _ args body _ -> ClassK (length args)
  Conditional gs _ _ -> ConditionalK (length gs)
  Assign {} -> AssignK
  AugmentedAssign {} -> AugmentedAssignK
  -- --Decorated
  Return Nothing _ -> TerminalStatementK $ void s
  Return (Just e) _ -> ReturnK
  -- --Try
  -- --Raise
  -- --With
  Pass _ -> TerminalStatementK $ void s
  Break _ -> TerminalStatementK $ void s
  Continue _ -> TerminalStatementK $ void s
  Delete {} -> DeleteK
  StmtExpr {} -> StmtExprK
  Global _ _ -> TerminalStatementK $ void s
  NonLocal _ _ -> TerminalStatementK $ void s
  Assert {} -> AssertK
  Print {} -> PrintK
  --Exec


exprKind' :: ExprSpan -> ESKind
exprKind' = \case
  Var v _ -> VarK v
  Int i _ _ -> LitK (LI i)
  -- LongInt i _ _ -> LitK (LI i)
  Float f _ _ -> LitK (LD f)
  -- Imaginary {} -> []
  Bool b _ -> LitK (LB b)
  -- None {} -> []
  -- Ellipsis
  -- ByteStrings {} -> []
  -- Strings {} -> []
  -- UnicodeStrings {} -> []
  Call {} -> AppK
  -- Subscript x y _ -> [x,y]
  -- SlicedExpr
  CondExpr {} -> IteK
  BinaryOp o _ _ _ -> BopK o
  UnaryOp o _ _ -> UopK o
  -- Dot
  -- Lambda _ e _ -> [e]
  Tuple {} -> TupleK
  -- Yield
  -- Generator
  -- ListComp
  List {} -> ListK
  -- Dictionary
  -- DictComp
  -- Set es _ -> es
  -- SetComp
  -- Starred
  -- Paren e _ -> [e]
  -- StringConversion e _ -> [e]
  e -> error $ "unhandled case of exprKind: " ++ (show e)

subESes :: ES -> [ES]
subESes = \case
  Ex e -> Ex <$> subExprs e
  St s -> case s of
    Import {} -> []
    FromImport {} -> []
    While cond body els _ -> (Ex cond) : (St <$> (body ++ els))
    For vs gen body els _ -> (Ex <$> vs ++ [gen]) ++ (St <$> (body ++ els))
    Fun _ _ Nothing body _ -> St <$> body --TODO we do not yet support annotations or default values on function Parameters (or result annotations)
    Class _ args body _ -> (Ex <$> (argExpr <$> args)) ++ (St <$> body)
    Conditional gs els _ -> concatMap (\(e, ss) -> [Ex e] ++ (St <$> ss)) gs ++ (St <$> els)
    Assign xs e _ -> (Ex <$> xs ++ [e])
    AugmentedAssign x _ e _ -> (Ex <$> [x,e])
    --Decorated
    Return Nothing _ -> []
    Return (Just e) _ -> [Ex e]
    --Try
    --Raise
    --With
    Pass _ -> []
    Break _ -> []
    Continue _ -> []
    Delete es _ -> Ex <$> es
    StmtExpr e _ -> [Ex e]
    Global _ _ -> []
    NonLocal _ _ -> []
    Assert es _ -> Ex <$> es
    Print _ es _ _ -> Ex <$> es
    --Exec

subExprs :: ExprSpan -> [ExprSpan]
subExprs = \case
  Var {} -> []
  Int {} -> []
  LongInt {} -> []
  Float {} -> []
  Imaginary {} -> []
  Bool {} -> []
  None {} -> []
  -- Ellipsis
  ByteStrings {} -> []
  Strings {} -> []
  UnicodeStrings {} -> []
  Call e args _ -> e:(argExpr <$> args)
  Subscript x y _ -> [x,y]
  -- SlicedExpr
  CondExpr t b f _ -> [t,b,f]
  BinaryOp _ x y _ -> [x,y]
  UnaryOp _ x _ -> [x]
  -- Dot
  Lambda _ e _ -> [e]
  Tuple es _ -> es
  -- Yield
  -- Generator
  -- ListComp
  List es _ -> es
  -- Dictionary
  -- DictComp
  Set es _ -> es
  -- SetComp
  -- Starred
  Paren e _ -> [e]
  StringConversion e _ -> [e]
  e -> error $ "unhandled case of subExprs" ++ (show e)

argExpr :: Argument a -> Expr a
argExpr (ArgExpr e _) = e
argExpr (ArgVarArgsPos e _) = e
argExpr (ArgVarArgsKeyword e _) = e
argExpr (ArgKeyword _ e _) = e
