module Document.Stack exposing(..)

import List.Extra

import Types exposing(Document, DocumentStack, defaultDocument)

push : Document -> DocumentStack -> DocumentStack
push document stack =
  if (List.Extra.notMember document stack) then
     [document] ++ (List.take 9 stack)
  else
    stack   

top : DocumentStack -> Document
top docstack =
  List.head docstack |> Maybe.withDefault defaultDocument
