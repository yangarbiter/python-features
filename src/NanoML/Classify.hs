{-# LANGUAGE LambdaCase #-}
module NanoML.Classify where

import Control.Monad
import Data.Monoid
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (catMaybes, isJust)

import Language.Python.Common hiding ((<>))


data ES = Ex ExprSpan | St StatementSpan deriving (Show)

instance Span ES where
  getSpan (Ex e) = annot e
  getSpan (St s) = annot s

data Literal
  = LI Integer
  | LLI Integer
  | LIm Double
  | LD Double
  | LB Bool
  | LC Char
  | LS [String]
  | LBS [String]
  | LUS [String]
  deriving (Show, Eq)

diff :: ExprSpan -> ExprSpan -> Set SrcSpan
diff e1 e2 = case (e1, e2) of
  (Lambda {}, _)
    -> error "Lambda unhandled in diff"
  (_, Lambda {})
    -> error "Lambda unhandled in diff"
  (Yield {}, _)
    -> error "Yield unhandled in diff"
  (_, Yield {})
    -> error "Yield unhandled in diff"
  (Var x _, Var y _)
    | void x == void y
      -> mempty
  (Int i1 _ _, Int i2 _ _)
    | i1 == i2
      -> mempty
  (LongInt i1 _ _, LongInt i2 _ _)
    | i1 == i2
      -> mempty
  (Float i1 _ _, Float i2 _ _)
    | i1 == i2
      -> mempty
  (Imaginary i1 _ _, Imaginary i2 _ _)
    | i1 == i2
      -> mempty
  (Bool i1 _, Bool i2 _)
    | i1 == i2
      -> mempty
  (None _, None _)
      -> mempty
  (Ellipsis _, Ellipsis _)
      -> mempty
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
    | (length a1 == length a2) && sameArgTypes a1 a2
      -> merge $ (diff f1 f2) : (zipWith diff (argExpr <$> a1) (argExpr <$> a2))
  (Subscript x1 x2 _, Subscript y1 y2 _)
      -> merge [diff x1 y1, diff x2 y2]
  (SlicedExpr e1 ss1 _, SlicedExpr e2 ss2 _)
    | length ss1 == length ss2,
      let sliceDiffs = zipWith diffSlice ss1 ss2,
      all isJust sliceDiffs
      -> merge $ (diff e1 e2) : catMaybes sliceDiffs
  (CondExpr t1 b1 f1 _, CondExpr t2 b2 f2 _)
      -> merge [diff t1 t2, diff b1 b2, diff f1 f2]
  (BinaryOp bx x1 x2 _, BinaryOp by y1 y2 _)
    | void bx == void by
      -> merge [diff x1 y1, diff x2 y2]
  (UnaryOp ux x _, UnaryOp uy y _)
    | void ux == void uy
      -> diff x y
  (Dot x1 a1 _, Dot x2 a2 _)
    | void a1 == void a2
      -> diff x1 x2
  -- Lambda _ e _
  (Tuple es1 _, Tuple es2 _)
    | length es1 == length es2
      -> merge $ zipWith diff es1 es2
  -- Yield
  (Generator c1 _, Generator c2 _)
    | Just ss <- diffComprehension c1 c2
      -> ss
  (ListComp c1 _, ListComp c2 _)
    | Just ss <- diffComprehension c1 c2
      -> ss
  (List es1 _, List es2 _)
    | length es1 == length es2
      -> merge $ zipWith diff es1 es2
  (Dictionary es1 _, Dictionary es2 _)
    | length es1 == length es2
      -> merge $ zipWith diffDictPair es1 es2
  (DictComp c1 _, DictComp c2 _)
    | Just ss <- diffComprehension c1 c2
      -> ss
  (Set es1 _, Set es2 _)
    | length es1 == length es2
      -> merge $ zipWith diff es1 es2
  (SetComp c1 _, SetComp c2 _)
    | Just ss <- diffComprehension c1 c2
      -> ss
  (Starred x1 _, Starred x2 _)
      -> diff x1 x2
  (Paren x1 _, Paren x2 _)
      -> diff x1 x2
  -- StringConversion e _   --(Version 2 only)
  _ -> Set.singleton $ annot e1
  where
    merge = mconcat

diffComprehension :: ComprehensionSpan -> ComprehensionSpan -> Maybe (Set SrcSpan)
diffComprehension e1@(Comprehension x1 f1 _) e2@(Comprehension x2 f2 _) = do
  ssX <- diffComprehensionExpr x1 x2
  ssF <- diffCompFor f1 f2
  return $ mconcat [ssX, ssF]

diffComprehensionExpr :: ComprehensionExprSpan -> ComprehensionExprSpan -> Maybe (Set SrcSpan)
diffComprehensionExpr (ComprehensionExpr e1) (ComprehensionExpr e2) = Just $ diff e1 e2
diffComprehensionExpr (ComprehensionDict (DictMappingPair k1 v1)) (ComprehensionDict (DictMappingPair k2 v2)) = Just $ mconcat [diff k1 k2, diff v1 v2]
diffComprehensionExpr _ _ = Nothing

diffCompFor :: CompForSpan -> CompForSpan -> Maybe (Set SrcSpan)
diffCompFor (CompFor fs1 i1 iter1 _) (CompFor fs2 i2 iter2 _)
  | isJust iter1 || isJust iter2
    = error "diffCompFor unhandled case"
  | length fs1 /= length fs2
    = Nothing
  | otherwise
    = Just $ mconcat $ (diff i1 i2) : zipWith diff fs1 fs2

diffDictPair :: DictMappingPairSpan -> DictMappingPairSpan -> Set SrcSpan
diffDictPair (DictMappingPair x1 y1) (DictMappingPair x2 y2) = mconcat [diff x1 x2, diff y1 y2]

sameArgTypes :: [Argument a] -> [Argument a] -> Bool
sameArgTypes as1 as2 = all sameArgType (zip as1 as2)
  where
    sameArgType (ArgExpr _ _, ArgExpr _ _) = True
    sameArgType (ArgVarArgsPos _ _, ArgVarArgsPos _ _) = True
    sameArgType (ArgVarArgsKeyword _ _, ArgVarArgsKeyword _ _) = True
    sameArgType (ArgKeyword k1 _ _, ArgKeyword k2 _ _) = void k1 == void k2

diffSlice :: SliceSpan -> SliceSpan -> Maybe (Set SrcSpan)
diffSlice e1 e2 = case (e1, e2) of
  (SliceProper l1 u1 s1 _, SliceProper l2 u2 s2 _)
    | comparable e1 e2
      -> Just $ mconcat [diffMaybe l1 l2, diffMaybe u1 u2, diffMaybe2 s1 s2]
  (SliceExpr e1 _, SliceExpr e2 _)
      -> Just $ diff e1 e2
  (SliceEllipsis _, SliceEllipsis _)
      -> Just $ mempty
  _ -> Nothing

diffMaybe :: Maybe ExprSpan -> Maybe ExprSpan -> Set SrcSpan
diffMaybe Nothing Nothing = mempty
diffMaybe (Just e1) (Just e2) = diff e1 e2
diffMaybe (Just e1) Nothing = Set.singleton $ annot e1
diffMaybe _ _ = error "diffMaybe: this shouldn't happen"

diffMaybe2 :: Maybe (Maybe ExprSpan) -> Maybe (Maybe ExprSpan) -> Set SrcSpan
diffMaybe2 Nothing Nothing = mempty
diffMaybe2 (Just Nothing) (Just Nothing) = mempty
diffMaybe2 (Just (Just e1)) (Just (Just e2)) = diff e1 e2
diffMaybe2 (Just (Just e1)) _ = Set.singleton $ annot e1
diffMaybe2 _ _ = error "diffMaybe2: this shouldn't happen"

comparable :: SliceSpan -> SliceSpan -> Bool
comparable (SliceProper l1 u1 s1 _) (SliceProper l2 u2 s2 _) =
     (l1 /= Nothing || l1 == l2)
  && (u1 /= Nothing || u1 == u2)
  && ((s1 /= Nothing && s1 /= Just Nothing) || s1 == s2)

ctfold :: Monoid a => (ES {- parent -} -> ES -> a -> a) -> a -> ES -> a
--NOTE: hack using Print as top level just because Print isn't used in Python3
ctfold f z r = go (St (Print False [] False SpanEmpty)) r
  where
    go p x = f p x (if null subs then z else mconcat (map (go x) subs))
      where
        subs = subESes' x -- subESes' skips Expr that is child of StmtExpr

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
  Ins e d -> "Ins (" ++ show (esKind e) ++ ") (" ++ showDiff d ++ ")"
  Del e d -> "Del (" ++ show (esKind e) ++ ") (" ++ showDiff d ++ ")"
  Cpy e d -> "Cpy (" ++ show (esKind e) ++ ") (" ++ showDiff d ++ ")"
  End     -> "End"

diffSpans :: Diff -> [ES] -> Set SrcSpan
diffSpans d' es = Set.fromList $ go d' (concatMap allSubESes es)
  where
  go _ [] = []
  go d' (x:xs) = case d' of
    Ins e d -> getSpan x : go d (x:xs)
    -- TODO: should we collapse `del e (ins* es)`, e.g.
    -- if a leaf is replaced by an entire tree?
    Del e (Ins _ d) -> getSpan e : go d xs
    Del e d -> getSpan e : go d xs
    Cpy e d -> go d xs
    End -> [] -- WEIRD

allSubESes :: ES -> [ES]
allSubESes x = x : (concatMap allSubESes $ subESes x)

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
  | esKind x == esKind y
  , length (subESes x) == length (subESes y)
  = cpy x (getDiff c) -- del x (getDiff d) `meet` ins y (getDiff i)
  | otherwise
  = del x (getDiff d) `meet` ins y (getDiff i)
    -- `meet`

data ESKind
  = VarK (Ident ())
  | AppK
  | BopK (Op ())
  | UopK (Op ())
  | LitK Literal
  | IteK
  | TupleK
  | ListK
  | TerminalStatementK (Statement ())
  | AssignK
  | AugmentedAssignK (AssignOp ())
  | DecoratedK [Decorator ()]
  | RaiseK
  | WithK [Bool]
  | ReturnK
  | TryK Int [(Int, Int)] Int
  | DeleteK
  | StmtExprK
  | AssertK
  | ConditionalK [Int]
  | ClassK (Ident ()) Int
  | WhileK Int
  | ForK Int Int
  | ParenK
  | FunK (Ident ()) Int
  | DotK (Ident ())
  | SliceK
  | SubscriptK
  | DictionaryK Int
  | NoneK
  | EllipsisK
  | SetK
  | StarredK
  | StringConversionK
  | ListCompK (Comprehension ())
  | DictCompK (Comprehension ())
  | SetCompK (Comprehension ())
  | YieldK (Maybe (YieldArg ()))
  deriving (Eq, Show)

--TODO a Suite should be alowed to change size without effectively changing Kind?
esKind :: ES -> ESKind
esKind (Ex e) = exprKind (void e)
esKind (St s) = stmtKind (void s)

stmtKind :: Statement () -> ESKind
stmtKind s = case s of
  Import {} -> TerminalStatementK s
  FromImport {} -> TerminalStatementK s
  While _ body _ _ -> WhileK (length body)
  For vs _ body _ _ -> ForK (length vs) (length body)
  Fun name args Nothing _ _ -> FunK name (length args) --TODO args have idents
  Class name args body _ -> ClassK name (length args) --TODO args have idents
  Conditional gs _ _ -> ConditionalK (length . snd <$> gs)
  Assign {} -> AssignK
  AugmentedAssign _ op _ _ -> AugmentedAssignK op
  Decorated ds _ _ -> DecoratedK ds
  Return {} -> ReturnK
  Try body excepts els _ _ -> TryK (length body) (handlerClassifier <$> excepts) (length els)
  Raise {} -> RaiseK
  With ctx _ _ -> WithK (isJust . snd <$> ctx)
  Pass _ -> TerminalStatementK s
  Break _ -> TerminalStatementK s
  Continue _ -> TerminalStatementK s
  Delete {} -> DeleteK
  StmtExpr {} -> StmtExprK
  Global _ _ -> TerminalStatementK s
  NonLocal _ _ -> TerminalStatementK s
  Assert {} -> AssertK
  -- Print b1 _ b2 _ -> PrintK b1 b2
  --Exec
  e -> error $ "unhandled case of stmtKind: " ++ (show e)

handlerClassifier :: Handler a -> (Int, Int)
handlerClassifier (Handler (ExceptClause c _) ss _) = (countJusts c, length ss)
  where
    countJusts Nothing = 0
    countJusts (Just (_, Nothing)) = 1
    countJusts _ = 2

-- NOTE: ignores "expr_literal"s
exprKind :: Expr () -> ESKind
exprKind = \case
  Var v _ -> VarK v
  Int i _ _ -> LitK (LI i)
  LongInt i _ _ -> LitK (LLI i)
  Float f _ _ -> LitK (LD f)
  Imaginary v _ _ -> LitK (LIm v)
  Bool b _ -> LitK (LB b)
  None _ -> NoneK
  Ellipsis _ -> EllipsisK
  ByteStrings ss _ -> LitK (LBS ss)
  Strings ss _ -> LitK (LS ss)
  UnicodeStrings ss _ -> LitK (LUS ss)
  Call {} -> AppK --TODO different kinds of arguments
  Subscript x y _ -> SubscriptK
  SlicedExpr {} -> SliceK --TODO different kinds of Slices
  CondExpr {} -> IteK
  BinaryOp o _ _ _ -> BopK o
  UnaryOp o _ _ -> UopK o
  Dot _ a _ -> DotK a
  -- Lambda _ e _ -> [e]
  Tuple {} -> TupleK
  Yield a _ -> YieldK (defaultYieldArg <$> a)
  -- Generator
  ListComp c _ -> ListCompK $ defaultComprehension c
  List {} -> ListK
  Dictionary pairs _ -> DictionaryK (length pairs)
  DictComp c _ -> DictCompK $ defaultComprehension c
  Set {} -> SetK
  SetComp c _ -> SetCompK $ defaultComprehension c
  Starred {} -> StarredK
  Paren {} -> ParenK
  StringConversion {} -> StringConversionK
  e -> error $ "unhandled case of exprKind: " ++ (show e)

defaultExpr = None ()

defaultComprehension :: Comprehension () -> Comprehension ()
defaultComprehension (Comprehension e f _) = Comprehension e' f' ()
  where
    e' = case e of
      ComprehensionExpr _ -> ComprehensionExpr (defaultExpr)
      ComprehensionDict _ -> ComprehensionDict (DictMappingPair (defaultExpr) (defaultExpr))
    f' = defaultCompFor f

defaultCompFor :: CompFor () -> CompFor ()
defaultCompFor (CompFor es _ it _) = CompFor es' defaultExpr it' ()
  where
    es' = map (const defaultExpr) es
    it' = defaultCompIter <$> it

defaultCompIter :: CompIter () -> CompIter ()
defaultCompIter (IterFor cf _) = IterFor (defaultCompFor cf) ()
defaultCompIter (IterIf ci _)  = IterIf (defaultCompIf ci) ()

defaultCompIf :: CompIf () -> CompIf ()
defaultCompIf (CompIf _ ci _) = CompIf defaultExpr (defaultCompIter <$> ci) ()

defaultYieldArg :: YieldArg () -> YieldArg ()
defaultYieldArg (YieldFrom e _) = YieldFrom defaultExpr ()
defaultYieldArg (YieldExpr e)   = YieldExpr defaultExpr

subESes' :: ES -> [ES]
subESes' = \case
  Ex e -> subESes (Ex e)
  St s -> case s of
    StmtExpr e _ -> subESes (Ex e)
    _ -> subESes (St s)

subESes :: ES -> [ES]
subESes = \case
  Ex e -> Ex <$> subExprs e
  St s -> case s of
    Import {} -> []
    FromImport {} -> []
    While cond body els _ -> (Ex cond) : (St <$> (body ++ els))
    For vs gen body els _ -> (Ex <$> vs ++ [gen]) ++ (St <$> (body ++ els))
    Fun _ _ Nothing body _ -> St <$> body --TODO support annotations or default values on function/lambda Parameters (or result annotations)
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

subExprs :: (Show a) => Expr a -> [Expr a]
subExprs = \case
  Var {} -> []
  Int {} -> []
  LongInt {} -> []
  Float {} -> []
  Imaginary {} -> []
  Bool {} -> []
  None {} -> []
  Ellipsis {} -> []
  ByteStrings {} -> []
  Strings {} -> []
  UnicodeStrings {} -> []
  Call e args _ -> e:(argExpr <$> args)
  Subscript x y _ -> [x,y]
  SlicedExpr e ss _ -> e:(concatMap sliceExpr ss)
  CondExpr t b f _ -> [t,b,f]
  BinaryOp _ x y _ -> [x,y]
  UnaryOp _ x _ -> [x]
  Dot e _ _ -> [e]
  Lambda _ e _ -> [e]
  Tuple es _ -> es
  -- Yield
  Generator c _ -> comprehensionSubExprs c
  ListComp c _ -> comprehensionSubExprs c
  List es _ -> es
  Dictionary pairs _ -> concatMap dictMappingPairSubExprs pairs
  DictComp c _ -> comprehensionSubExprs c
  Set es _ -> es
  SetComp c _ -> comprehensionSubExprs c
  Starred e _ -> [e]
  Paren e _ -> [e]
  StringConversion e _ -> [e]
  e -> error $ "unhandled case of subExprs: " ++ (show e)

comprehensionSubExprs (Comprehension ce cf _) = ceSubExprs ce ++ cfSubExprs cf
  where
    ceSubExprs (ComprehensionExpr e) = [e]
    ceSubExprs (ComprehensionDict pair) = dictMappingPairSubExprs pair
    cfSubExprs (CompFor es e Nothing _) = es ++ [e]
    cfSubExprs _ = error "unhandled case of comprehensionSubExprs"

dictMappingPairSubExprs (DictMappingPair e1 e2) = [e1, e2]

argExpr :: Argument a -> Expr a
argExpr (ArgExpr e _) = e
argExpr (ArgVarArgsPos e _) = e
argExpr (ArgVarArgsKeyword e _) = e
argExpr (ArgKeyword _ e _) = e

sliceExpr :: Slice a -> [Expr a]
sliceExpr (SliceProper x y Nothing _) = catMaybes [x, y]
sliceExpr (SliceProper x y (Just z) _) = catMaybes [x, y, z]
sliceExpr (SliceExpr e _) = [e]
sliceExpr (SliceEllipsis _) = []
