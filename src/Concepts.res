module Tag = {
  type t =
    | Oblique
    | Boldface
    | Monospaced
    | Highlighted
    | Underscored
    | Strikethrough

  let toString = (t: t) => {
    switch t {
    | Oblique => "em"
    | Boldface => "strong"
    | Monospaced => "code"
    | Highlighted => "mark"
    | Underscored => "ins"
    | Strikethrough => "del"
    }
  }
}
type tag = Tag.t

type rec document = list<block>
and paragraph = list<span>
and block =
  | Heading1(document)
  | Heading2(document)
  | Heading3(document)
  | OrderedList(list<document>)
  | UnorderedList(list<document>)
  | CheckList(list<(bool, document)>)
  | Table(list<list<document>>)
  | Quotation(document)
  | Embeded(string, string)
  | Paragraph(paragraph)
and span =
  | Tagged(tag, list<span>)
  | Embeded(string, string)
  | Plain(string)
  | Raw(string)
