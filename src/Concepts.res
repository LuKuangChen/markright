module Tag = {
  type t =
    | Italic
    | Strong
    | Delete
    | Highlight
    | Underline
    | Monospace

  let toString = (t: t) => {
    switch t {
    | Italic => "emph"
    | Strong => "strong"
    | Delete => "del"
    | Highlight => "mark"
    | Underline => "ins"
    | Monospace => "code"
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
