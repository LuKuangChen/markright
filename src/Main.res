open Concepts

type token =
  | Tag(tag)
  | Final(span)

let parseParagraph = (it: string) => {
  let makeRecParser = (k, delim: string, tag: tag) => (it: string): array<token> => {
    it
    ->String.split(String.concat(delim, delim))
    ->Array.flatMapWithIndex((x, i) => {
      let x =
        x
        ->String.replaceAll(String.concat(" ", delim), delim)
        ->k
      if i == 0 {
        x
      } else {
        [Tag(tag), ...x]
      }
    })
  }
  let parseRec: string => array<token> =
    (x => [Final(Plain(x))])
    ->makeRecParser("/", Italic)
    ->makeRecParser("*", Strong)
    ->makeRecParser("~", Delete)
    ->makeRecParser("!", Highlight)
    ->makeRecParser("_", Underline)
    ->makeRecParser("`", Monospace)

  let groupTokens = (tokens: array<token>): paragraph => {
    let rec splitAtFirst = (ts, tag) => {
      switch ts {
      | list{} => failwith("not found")
      | list{t, ...ts} =>
        if t == Tag(tag) {
          (list{}, ts)
        } else {
          let (head, tail) = splitAtFirst(ts, tag)
          (list{t, ...head}, tail)
        }
      }
    }
    let rec f = (ts: list<token>): list<span> => {
      switch ts {
      | list{} => list{}
      | list{Final(x), ...ts} => list{x, ...f(ts)}
      | list{Tag(t), ...ts} => {
          let (head, ts) = splitAtFirst(ts, t)
          list{Tagged(t, f(head)), ...f(ts)}
        }
      }
    }
    f(tokens->List.fromArray)
  }

  it
  ->String.split(`""`)
  ->Array.flatMapWithIndex((token, i) => {
    if Int.mod(i, 2) == 0 {
      parseRec(token)
    } else {
      [Final(Eval(token))]
    }
  })
  ->groupTokens
}

let escape = (x: string): string => {
    x
    ->String.replaceAll(`&`, "&amp;")
    ->String.replaceAll(`<`, "&lt;")
    ->String.replaceAll(`>`, "&gt;")
    ->String.replaceAll(`"`, "&quot;")
    ->String.replaceAll(`'`, "&#39;")
}

let rec spansToString = (spans: list<span>) => {
  spans
  ->List.map(spanToString)
  ->List.toArray
  ->Array.join("")
}
and spanToString = span => {
  switch span {
  | Tagged(tag, spans) => {
      let tag = Tag.toString(tag)
      `<${tag}>${spansToString(spans)}</${tag}>`
    }
  | Plain(x) => escape(x)
  | Eval(x) => {
      let tag = "code"
      `<${tag}>${escape(x)}</${tag}>`
    }
  }
}
