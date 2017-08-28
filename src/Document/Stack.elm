module Document.Stack exposing(..)

import List.Extra
import Utility

import Types exposing(Document, DocumentStack, defaultDocument)

{-|
  Push a new document onto the stack after deleting
  the previous version if it is there.
-}
push : Document -> DocumentStack -> DocumentStack
push document stack =
  let
    _  = Debug.log "Pushing document" document.id
    stack2 = Utility.removeWhen (\doc -> doc.id == document.id) stack
    stack3 = [document] ++ (List.take 9 stack2)
  in
    List.sortWith titleCompare stack3


titleCompare doc1 doc2 =
  case (doc1.title == doc2.title, doc1.title < doc2.title) of
    (True, _) -> EQ
    (_, True) -> LT
    (_, False) -> GT




top : DocumentStack -> Document
top docstack =
  List.head docstack |> Maybe.withDefault defaultDocument
