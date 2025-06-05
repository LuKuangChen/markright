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

type rec document = list<paragraph>
and paragraph = list<span>
and span =
  | Tagged(tag, list<span>)
  | Eval(string)
  | Plain(string)
