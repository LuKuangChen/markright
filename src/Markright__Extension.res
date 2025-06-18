open Markright__Concept

type t = (string, ~fullDocument: list<block>) => list<block>

let newBlock = (f): t => {
  (content, ~fullDocument) => {
    list{f(content, ~fullDocument)}
  }
}

let newSpan = (f): t => {
  (content, ~fullDocument) => {
    list{Paragraph(list{f(content, ~fullDocument)})}
  }
}

let newRaw = (f): t => {
  (content, ~fullDocument) => {
    list{Raw(f(content, ~fullDocument))}
  }
}
