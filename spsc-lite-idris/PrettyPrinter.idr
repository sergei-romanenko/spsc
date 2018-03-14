module PrettyPrinter

import Text.PrettyPrint.WL

import SLanguage
import ProcessTree

%default covering

docProgram : Program -> Doc
docProgram (MkProgram rules) =
  vsep (map (text . show) rules)

docTask : Task -> Doc
docTask (MkTask exp prog) =
  (text . show) exp |$| text "where" |+| line |$| docProgram prog

export
ppTask : Task -> String
ppTask task =
  toString 0.4 80 $ docTask task

mutual

  docTree : Tree -> NodeId -> Doc
  docTree tree nId =
    let MkNode _ exp contr parent children back = getNode tree nId in
    docContr contr |+|
    text (cast nId) |+| text " : " |+| text (show exp) |+|
    maybe empty (\backId => line |+| text ("--> " ++ cast backId)) back |+|
    (nest 4 $ docChildren tree children)

  docContr : Maybe Contraction -> Doc
  docContr Nothing = empty
  docContr (Just (MkContraction vname cname cparams)) =
    char '{' |+|
    text vname |+| text " = " |+| text (showPat cname cparams) |+|
    char '}' |+| line
  
  docChildren : Tree -> List NodeId -> Doc
  docChildren tree [] = empty
  docChildren tree (nId :: nIds) =
    line |+| line |+| docTree tree nId |+| docChildren tree nIds

export
ppTree : Tree -> String
ppTree tree =
  toString 0.4 80 $ docTree tree 0
