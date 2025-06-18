module Concept: {
  module Tag: {
    type t = Oblique | Boldface | Monospaced | Highlighted | Underscored | Strikethrough
    let toHTMLString: t => string
  }
  type tag = Tag.t
  type rec block =
    | Heading1(list<block>)
    | Heading2(list<block>)
    | Heading3(list<block>)
    | OrderedList(list<list<block>>)
    | UnorderedList(list<list<block>>)
    | CheckList(list<(bool, list<block>)>)
    | Table(list<list<list<block>>>)
    | Quotation(list<block>)
    | Paragraph(list<span>)
    | EmbededBlock(string, string)
    | RawBlock(string)
  and span = Tagged(tag, list<span>) | Plain(string) | EmbededSpan(string, string) | RawSpan(string)
}

module Extension: {
  type t = (string, ~fullDocument: list<Concept.block>) => list<Concept.block>
  let newBlock: ((string, ~fullDocument: list<Concept.block>) => Concept.block) => t
  let newSpan: ((string, ~fullDocument: list<Concept.block>) => Concept.span) => t
  let newRaw: ((string, ~fullDocument: list<Concept.block>) => string) => t
}

let compile: (string, ~extensions: RescriptCore.Dict.t<Extension.t>=?) => string
