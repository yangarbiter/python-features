{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module MyPretty (mypretty) where

-- based on https://hackage.haskell.org/package/language-python-0.5.4/docs/src/Language-Python-Common-PrettyAST.html

import Language.Python.Common.Pretty
import Language.Python.Common.PrettyAST
import Language.Python.Common.AST

spanComment :: StatementSpan -> Doc
spanComment stmt = (text " # ") <+> pretty (annot stmt)

prettyWithSpanComment :: StatementSpan -> Doc
prettyWithSpanComment stmt = pretty stmt <+> spanComment stmt

indent :: Doc -> Doc
indent doc = nest 4 doc

-- XXX is there a better way to do this?
blankLine :: Doc
blankLine = text []

prettyString :: String -> Doc
   -- XXX should handle the escaping properly
-- prettyString str = text (show str)
prettyString str = text str

class MyPretty a where
  mypretty :: a -> Doc

instance MyPretty ModuleSpan where
   mypretty (Module stmts) = vcat $ map mypretty stmts

prettySuite :: [StatementSpan] -> Doc
prettySuite stmts = vcat $ map mypretty stmts

optionalKeywordSuite :: String -> [StatementSpan] -> Doc
optionalKeywordSuite _ [] = empty
optionalKeywordSuite keyword stmts = text keyword <> colon $+$ indent (prettySuite stmts)

prettyOptionalList :: Pretty a => [a] -> Doc
prettyOptionalList [] = empty
prettyOptionalList list = parens $ commaList list

prettyGuards :: [(ExprSpan, SuiteSpan)] -> Doc
prettyGuards [] = empty
prettyGuards ((cond,body):guards)
   = text "elif" <+> pretty cond <> colon $+$ indent (prettySuite body) $+$
     prettyGuards guards

instance MyPretty StatementSpan where
   -- pretty :: Statement -> Doc
   mypretty stmt@(Import {}) = prettyWithSpanComment stmt
   mypretty stmt@(FromImport {}) = prettyWithSpanComment stmt
   mypretty stmt@(While {})
      = text "while" <+> pretty (while_cond stmt) <> colon <+> spanComment stmt $+$
        indent (prettySuite (while_body stmt)) $+$ optionalKeywordSuite "else" (while_else stmt)
   mypretty stmt@(For {})
      = text "for" <+> commaList (for_targets stmt) <+> text "in" <+> pretty (for_generator stmt) <> colon <+> spanComment stmt $+$
        indent (prettySuite (for_body stmt)) $+$ optionalKeywordSuite "else" (for_else stmt)
   mypretty stmt@(Fun {})
      = text "def" <+> pretty (fun_name stmt) <> parens (commaList (fun_args stmt)) <+>
        perhaps (fun_result_annotation stmt) (text "->") <+>
        pretty (fun_result_annotation stmt) <> colon <+> spanComment stmt $+$ indent (prettySuite (fun_body stmt))
   mypretty stmt@(Class {})
      = text "class" <+> pretty (class_name stmt) <> prettyOptionalList (class_args stmt) <>
        colon <+> spanComment stmt $+$ indent (prettySuite (class_body stmt))
   mypretty stmt@(Conditional { cond_guards = guards, cond_else = optionalElse })
      = case guards of
           (cond,body):xs ->
              text "if" <+> pretty cond <> colon <+> spanComment stmt $+$ indent (prettySuite body) $+$
              prettyGuards xs $+$
              optionalKeywordSuite "else" optionalElse
           [] -> error "Attempt to pretty print conditional statement with empty guards"
   -- XXX is the assign_to always a singleton?
   mypretty stmt@(Assign {}) = prettyWithSpanComment stmt
   mypretty stmt@(AugmentedAssign {}) = prettyWithSpanComment stmt
   mypretty (Decorated { decorated_decorators = decs, decorated_def = stmt})
      = vcat (map pretty decs) $+$ mypretty stmt --TODO annotate the decorators?
   mypretty stmt@(Return {}) = prettyWithSpanComment stmt
   --TODO?:
   mypretty (Try { try_body = body, try_excepts = handlers, try_else = optionalElse, try_finally = finally})
      = text "try" <> colon $+$ indent (prettySuite body) $+$
        prettyHandlers handlers $+$ optionalKeywordSuite "else" optionalElse $+$
        optionalKeywordSuite "finally" finally
   mypretty stmt@(Raise {}) = prettyWithSpanComment stmt
   mypretty stmt@(With { with_context = context, with_body = body })
      = text "with" <+> hcat (punctuate comma (map prettyWithContext context)) <+> colon <+> spanComment stmt $+$
        indent (prettySuite body)
   mypretty stmt@(Pass {}) = prettyWithSpanComment stmt
   mypretty stmt@(Break {}) = prettyWithSpanComment stmt
   mypretty stmt@(Continue {}) = prettyWithSpanComment stmt
   mypretty stmt@(Delete {}) = prettyWithSpanComment stmt
   mypretty stmt@(StmtExpr {}) = prettyWithSpanComment stmt
   mypretty stmt@(Global {}) = prettyWithSpanComment stmt
   mypretty stmt@(NonLocal {}) = prettyWithSpanComment stmt
   mypretty stmt@(Assert { assert_exprs = es }) = prettyWithSpanComment stmt
   mypretty stmt@(Print {}) = error "unhandled in mypretty: Print (py2 only)"
   mypretty stmt@(Exec {}) = error "unhandled in mypretty: Exec (py2 only)"

prettyWithContext :: (Expr a, Maybe (Expr a)) -> Doc
prettyWithContext (e, Nothing) = pretty e
prettyWithContext (e, Just as) = pretty e <+> text "as" <+> pretty as

prettyHandlers :: [Handler a] -> Doc
prettyHandlers = foldr (\next rec -> pretty next $+$ rec) empty
