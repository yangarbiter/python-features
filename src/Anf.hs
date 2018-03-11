{-# LANGUAGE RecordWildCards #-}
module Anf ( makeAnfSource
           , makeAnfModule
           , makeAnfStat
           , makeAnfExpr
           ) where

import Control.Monad.State.Class
import Control.Monad.State.Strict
import Language.Python.Common
import Language.Python.Common.Pretty
import Language.Python.Version3.Parser

makeAnfSource :: String -> Either ParseError String
makeAnfSource s = fmap (go . fst) $ parseModule s "interaction"
  where go = prettyText . MkVerboseModule . fst . flip runState 0 . makeAnfModule

newtype VerboseModule = MkVerboseModule ModuleSpan
instance Pretty VerboseModule where
  pretty (MkVerboseModule (Module stmts)) = vcat $ map prettyWithSpanComment stmts

prettyWithSpanComment :: StatementSpan -> Doc
prettyWithSpanComment stmt = pretty stmt <+> (text " # ") <+> pretty (annot stmt)

type Fresh = State Int

fresh :: Fresh Int
fresh = modify' (+1) >> get

makeAnfModule :: ModuleSpan -> Fresh ModuleSpan
makeAnfModule (Module stats) = fmap Module $ makeAnfStats stats

makeAnfStat :: StatementSpan -> Fresh [StatementSpan]
makeAnfStat s@Import{} = pure [s]
makeAnfStat s@FromImport{} = pure [s]
makeAnfStat While{..} = do
  (condStmts, condExpr) <- makeAnfExpr while_cond
  newBody <- fmap (++ condStmts) $ makeAnfStats while_body
  newElse <- makeAnfStats while_else
  pure $ condStmts ++ [While condExpr newBody newElse stmt_annot]
makeAnfStat For{..} = do
  (genStmts, genExpr) <- makeAnfExpr for_generator
  newBody <- makeAnfStats for_body
  newElse <- makeAnfStats for_else
  pure $ genStmts ++ [For for_targets genExpr newBody newElse stmt_annot]
makeAnfStat s@Fun{..} = do
  bodyStats <- makeAnfStats fun_body
  pure $ [s { fun_body = bodyStats }]
makeAnfStat s@Class{..} = do
  (argStmts, argExprs) <- makeAnfArgs class_args
  bodyStats <- makeAnfStats class_body
  pure $ argStmts ++ [s { class_args = argExprs, class_body = bodyStats }]
makeAnfStat Conditional{..} = do
  (guardStmts, guards) <- makeAnfGuards cond_guards
  elseStmts <- makeAnfStats cond_else
  pure $ guardStmts ++ [Conditional guards elseStmts stmt_annot]
  where makeAnfGuard :: (ExprSpan, Suite SrcSpan)
                     -> Fresh ([StatementSpan], (ExprSpan, Suite SrcSpan))
        makeAnfGuard (condition, body) = do
          -- TODO: Make this less strict
          (condStmts, condExpr) <- makeAnfExpr condition
          bodyStmts <- makeAnfStats body
          pure $ (condStmts, (condExpr, bodyStmts))
        makeAnfGuards :: [(ExprSpan, Suite SrcSpan)]
                      -> Fresh ([StatementSpan], [(ExprSpan, Suite SrcSpan)])
        makeAnfGuards = fmap sequence . traverse makeAnfGuard
makeAnfStat Assign{..} = do
  -- TODO?: Transform target too (here and AugmentedAssign)
  (srcStmts, srcExpr) <- makeAnfExpr assign_expr
  pure $ srcStmts ++ [Assign assign_to srcExpr stmt_annot]
makeAnfStat AugmentedAssign{..} = do
  (srcStmts, srcExpr) <- makeAnfExpr aug_assign_expr
  pure $ srcStmts ++ [AugmentedAssign aug_assign_to aug_assign_op srcExpr stmt_annot]
makeAnfStat Decorated{} = error "Unsupported: Decorated"
makeAnfStat Return{..} = case return_expr of
  Nothing -> pure [Return Nothing stmt_annot]
  Just e  -> do
    (valStmts, valExpr) <- makeAnfExpr e
    pure $ valStmts ++ [Return (Just valExpr) stmt_annot]
makeAnfStat Try{} = error "Unsupported: Try"
makeAnfStat Raise{} = error "Unsupported: Raise"
makeAnfStat With{} = error "Unsupported: With"
makeAnfStat s@Pass{} = pure [s]
makeAnfStat s@Break{} = pure [s]
makeAnfStat s@Continue{} = pure [s]
makeAnfStat Delete{} = error "Unsupported: Delete"
makeAnfStat StmtExpr{..} = do
  (stats, expr) <- makeAnfExpr stmt_expr
  pure $ stats ++ [StmtExpr expr stmt_annot]
makeAnfStat s@Global{} = pure [s]
makeAnfStat s@NonLocal{} = pure [s]
makeAnfStat Assert{..} = do
  (valStmts, valExprs) <- makeAnfExprs assert_exprs
  pure $ valStmts ++ [Assert valExprs stmt_annot]
makeAnfStat Print{} = error "Unsupported: Print" -- not needed in python3
makeAnfStat Exec{} = error "Unsupported: Exec" -- not needed in python3

makeAnfStats :: [StatementSpan] -> Fresh [StatementSpan]
makeAnfStats stats = fmap concat $ traverse makeAnfStat stats

idFromNumbers :: Int -> Int -> Int -> Int -> String
idFromNumbers sr sc er ec =
  concat [show sr, "_", show sc, "_", show er, "_", show ec]

spanId :: SrcSpan -> String
spanId SpanCoLinear{..} =
  idFromNumbers span_row span_start_column span_row span_end_column
spanId SpanMultiLine{..} =
  idFromNumbers span_start_row span_start_column span_end_row span_end_column
spanId SpanPoint{..} =
  idFromNumbers span_row span_column span_row span_column

locIdent :: SrcSpan -> Fresh IdentSpan
locIdent ss = pure $ Ident ("span_" ++ spanId ss) ss

makeAnfThings :: (a -> Fresh ([StatementSpan], a)) -> [a]
              -> Fresh ([StatementSpan], [a])
makeAnfThings f things = do
  pairs <- traverse f things
  pure (concat . map fst $ pairs, map snd pairs)

makeAnfArgWith :: (ExprSpan -> SrcSpan -> ArgumentSpan) -> ExprSpan -> SrcSpan
               -> Fresh ([StatementSpan], ArgumentSpan)
makeAnfArgWith f e ss = do
  (eStmts, eExpr) <- makeAnfExpr e
  pure $ (eStmts, f eExpr ss)

makeAnfArg :: ArgumentSpan -> Fresh ([StatementSpan], ArgumentSpan)
makeAnfArg (ArgExpr e ss) = makeAnfArgWith ArgExpr e ss
makeAnfArg (ArgVarArgsPos e ss) = makeAnfArgWith ArgVarArgsPos e ss
makeAnfArg (ArgVarArgsKeyword e ss) = makeAnfArgWith ArgVarArgsKeyword e ss
makeAnfArg (ArgKeyword k e ss) = makeAnfArgWith (ArgKeyword k) e ss

makeAnfArgs :: [ArgumentSpan] -> Fresh ([StatementSpan], [ArgumentSpan])
makeAnfArgs = makeAnfThings makeAnfArg

namedExpr :: ExprSpan -> Fresh ([StatementSpan], ExprSpan)
namedExpr e = do
  name <- locIdent ann
  let assigner = Assign [Var name ann] e ann
  pure ([assigner], Var name ann)
  where ann = annot e

makeAnfExpr :: ExprSpan -> Fresh ([StatementSpan], ExprSpan)
makeAnfExpr e@Var{} = namedExpr e
makeAnfExpr e@Int{} = namedExpr e
makeAnfExpr e@LongInt{} = namedExpr e
makeAnfExpr e@Float{} = namedExpr e
makeAnfExpr e@Imaginary{} = namedExpr e
makeAnfExpr e@Bool{} = namedExpr e
makeAnfExpr e@None{} = namedExpr e
makeAnfExpr e@Ellipsis{} = namedExpr e
makeAnfExpr e@ByteStrings{} = namedExpr e
makeAnfExpr e@Strings{} = namedExpr e
makeAnfExpr e@UnicodeStrings{} = namedExpr e
makeAnfExpr Call{..} = do
  (funStmts, funExpr) <- makeAnfExpr call_fun
  (argStmts, argExprs) <- makeAnfArgs call_args
  (callStmts, callExpr) <- namedExpr $ Call funExpr argExprs expr_annot
  pure $ (funStmts ++ argStmts ++ callStmts, callExpr)
makeAnfExpr Subscript{..} = do
  (subscripteeStmts, subscripteeExpr) <- makeAnfExpr subscriptee
  (subscriptStmts, subscriptExpr) <- makeAnfExpr subscript_expr
  (ssStmts, ssExpr) <- namedExpr $ Subscript subscripteeExpr subscriptExpr expr_annot
  pure $
    (subscripteeStmts ++ subscriptStmts ++ ssStmts, ssExpr)
makeAnfExpr SlicedExpr{} = error "Unsupported: SlicedExpr"
makeAnfExpr CondExpr{} = error "Unsupported: CondExpr"
makeAnfExpr BinaryOp{..} = do
  (leftStmts, leftExpr) <- makeAnfExpr left_op_arg
  (rightStmts, rightExpr) <- makeAnfExpr right_op_arg
  (opStmts, opExpr) <- namedExpr $ BinaryOp operator leftExpr rightExpr expr_annot
  pure $ (leftStmts ++ rightStmts ++ opStmts, opExpr)
makeAnfExpr UnaryOp{..} = do
  (opStmts, opExpr) <- makeAnfExpr op_arg
  (resStmts, resExpr) <- namedExpr $ UnaryOp operator opExpr expr_annot
  pure $ (opStmts ++ resStmts, resExpr)
makeAnfExpr Dot{..} = do
  (dottedStmts, dottedExpr) <- makeAnfExpr dot_expr
  (dotStmts, dotExpr) <- namedExpr $ Dot dottedExpr dot_attribute expr_annot
  pure $ (dottedStmts ++ dotStmts, dotExpr)
makeAnfExpr Lambda{} = error "Unsupported: Lambda"
makeAnfExpr Tuple{..} = do
  (eStmts, eExprs) <- makeAnfExprs tuple_exprs
  (tupleStmts, tupleExpr) <- namedExpr $ Tuple eExprs expr_annot
  pure $ (eStmts ++ tupleStmts, tupleExpr)
makeAnfExpr Yield{} = error "Unsupported: Yield"
makeAnfExpr Generator{} = error "Unsupported: Generator"
makeAnfExpr ListComp{} = error "Unsupported: ListComp"
makeAnfExpr List{..} = do
  (eStmts, eExprs) <- makeAnfExprs list_exprs
  (listStmts, listExpr) <- namedExpr $ List eExprs expr_annot
  pure $ (eStmts ++ listStmts, listExpr)
makeAnfExpr Dictionary{..} = do
  (pairStmts, pairs) <- makeAnfPairs dict_mappings
  (dictStmts, dictExpr) <- namedExpr $ Dictionary pairs expr_annot
  pure $ (pairStmts ++ dictStmts, dictExpr)
  where makeAnfPair (DictMappingPair key value) = do
          (pairStmts, [keyExpr, valueExpr]) <- makeAnfExprs [key, value]
          pure $ (pairStmts, DictMappingPair keyExpr valueExpr)
        makeAnfPairs = makeAnfThings makeAnfPair
makeAnfExpr DictComp{} = error "Unsupported: DictComp"
makeAnfExpr Set{..} = do
  (eStmts, eExprs) <- makeAnfExprs set_exprs
  (setStmts, setExpr) <- namedExpr $ Set eExprs expr_annot
  pure $ (eStmts ++ setStmts, setExpr)
makeAnfExpr SetComp{} = error "Unsupported: SetComp"
makeAnfExpr Starred{..} = do
  (eStmts, eExpr) <- makeAnfExpr starred_expr
  (starStmts, starExpr) <- namedExpr $ Starred eExpr expr_annot
  pure $ (eStmts ++ starStmts, starExpr)
makeAnfExpr Paren{..} = do
  (eStmts, eExpr) <- makeAnfExpr paren_expr
  (parenStmts, parenExpr) <- namedExpr $ Paren eExpr expr_annot
  pure $ (eStmts ++ parenStmts, parenExpr)
makeAnfExpr StringConversion{} = error "Unsupported: StringConversion" -- not needed in python3

makeAnfExprs :: [ExprSpan] -> Fresh ([StatementSpan], [ExprSpan])
makeAnfExprs = makeAnfThings makeAnfExpr
