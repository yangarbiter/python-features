{-# LANGUAGE RecordWildCards #-}
module Anf ( makeAnfModule
           , makeAnfStat
           , makeAnfExpr
           ) where

import Control.Monad.State.Class
import Control.Monad.State.Strict
import Language.Python.Common

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
makeAnfStat Fun{} = error "Unsupported: Fun"
makeAnfStat Class{} = error "Unsupported: Class"
makeAnfStat Conditional{}   = error "Unsupported: Conditional"
makeAnfStat Assign{} = error "Unsupported: Assign"
makeAnfStat AugmentedAssign{} = error "Unsupported: Aug Assign"
makeAnfStat Decorated{} = error "Unsupported: Decorated"
makeAnfStat Return{} = error "Unsupported: Return"
makeAnfStat Try{} = error "Unsupported: Try"
makeAnfStat Raise{} = error "Unsupported: Raise"
makeAnfStat With{} = error "Unsupported: With"
makeAnfStat s@Pass{} = pure [s]
makeAnfStat s@Break{} = pure [s]
makeAnfStat s@Continue{} = pure [s]
makeAnfStat Delete{} = error "Unsupported: Delete"
makeAnfStat StmtExpr{} = error "Unsupported: Expression"
makeAnfStat s@Global{} = pure [s]
makeAnfStat s@NonLocal{} = pure [s]
makeAnfStat Assert{} = error "Unsupported: Assert"
makeAnfStat Print{} = error "Unsupported: Print"
makeAnfStat Exec{} = error "Unsupported: Exec"

makeAnfStats :: [StatementSpan] -> Fresh [StatementSpan]
makeAnfStats stats = fmap concat $ traverse makeAnfStat stats

locIdent :: SrcSpan -> Fresh IdentSpan
locIdent ss = do
  i <- fresh
  let SpanPoint _ row column = spanStartPoint ss
      name = "ident_" ++ show row ++ "_" ++ show column ++ "_" ++ show i
  pure $ Ident name ss

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

makeAnfExpr :: ExprSpan -> Fresh ([StatementSpan], ExprSpan)
makeAnfExpr e@Var{} = pure ([], e)
makeAnfExpr e@Int{} = pure ([], e)
makeAnfExpr e@LongInt{} = pure ([], e)
makeAnfExpr e@Float{} = pure ([], e)
makeAnfExpr e@Imaginary{} = pure ([], e)
makeAnfExpr e@Bool{} = pure ([], e)
makeAnfExpr e@None{} = pure ([], e)
makeAnfExpr e@Ellipsis{} = pure ([], e)
makeAnfExpr e@ByteStrings{} = pure ([], e)
makeAnfExpr e@Strings{} = pure ([], e)
makeAnfExpr e@UnicodeStrings{} = pure ([], e)
makeAnfExpr Call{..} = do
  (funStmts, funExpr) <- makeAnfExpr call_fun
  (argStmts, argExprs) <- makeAnfArgs call_args
  pure $ (funStmts ++ argStmts, Call funExpr argExprs expr_annot)
makeAnfExpr Subscript{} = error "Unsupported: Subscript"
makeAnfExpr SlicedExpr{} = error "Unsupported: SlicedExpr"
makeAnfExpr CondExpr{} = error "Unsupported: CondExpr"
makeAnfExpr BinaryOp{} = error "Unsupported: BinaryOp"
makeAnfExpr UnaryOp{} = error "Unsupported: UnaryOp"
makeAnfExpr Dot{} = error "Unsupported: Dot"
makeAnfExpr Lambda{} = error "Unsupported: Lambda"
makeAnfExpr Tuple{} = error "Unsupported: Tuple"
makeAnfExpr Yield{} = error "Unsupported: Yield"
makeAnfExpr Generator{} = error "Unsupported: Generator"
makeAnfExpr ListComp{} = error "Unsupported: ListComp"
makeAnfExpr List{} = error "Unsupported: List"
makeAnfExpr Dictionary{} = error "Unsupported: Dictionary"
makeAnfExpr DictComp{} = error "Unsupported: DictComp"
makeAnfExpr Set{} = error "Unsupported: Set"
makeAnfExpr SetComp{} = error "Unsupported: SetComp"
makeAnfExpr Starred{} = error "Unsupported: Starred"
makeAnfExpr Paren{} = error "Unsupported: Paren"
makeAnfExpr StringConversion{} = error "Unsupported: StringConversion"

makeAnfExprs :: [ExprSpan] -> Fresh ([StatementSpan], [ExprSpan])
makeAnfExprs = makeAnfThings makeAnfExpr
