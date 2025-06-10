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
  | Paragraph(paragraph)
  | List({ordered: bool, content: list<document>})
  | Blockquote(document)
  | Table(list<list<document>>)
  | BEval(string)
and span =
  | Tagged(tag, list<span>)
  | SEval(string)
  | Plain(string)
