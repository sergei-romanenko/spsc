module PrettyPrinter

import Text.PrettyPrint.WL

import SLanguage
import ProcessTree

%default covering

mutual

  docTree : Tree -> NodeId -> Doc
  docTree tree nId =
    let MkNode _ exp contr parent children = getNode tree nId in
    docContr contr |+|
    text (cast nId) |+| text " : " |+| text (show exp) |+|
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
