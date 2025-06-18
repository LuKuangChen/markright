module Tag = {
  type t =
    | Oblique
    | Boldface
    | Monospaced
    | Highlighted
    | Underscored
    | Strikethrough

  let toHTMLString = (t: t) => {
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

type rec block =
  | Heading1(list<block>)
  | Heading2(list<block>)
  | Heading3(list<block>)
  | OrderedList(list<list<block>>)
  | UnorderedList(list<list<block>>)
  | CheckList(list<(bool, list<block>)>)
  | Table(list<list<list<block>>>)
  | Quotation(list<block>)
  | Embeded(string, string)
  | Paragraph(list<span>)
  | Raw(string)
and span =
  | Tagged(tag, list<span>)
  | Embeded(string, string)
  | Plain(string)
  | Raw(string)
