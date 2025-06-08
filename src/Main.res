open Concepts

type document_token = 
    | ParagraphLine(string)
    | ListItem({ordered: bool, content: document})
    | TableElement({col: int, content: document})
    | TableBreak
    | Empty
    | Final(block)

let rec parseDocument = (it: string): document => {
  let rec takeAllIndented = (indent: int, lines: list<string>) => {
    switch lines {
    | list{} => (list{}, list{})
    | list{line, ...lines} =>
      if line == "" || line->String.startsWith(String.repeat(" ", indent)) {
        let (head, lines) = takeAllIndented(indent, lines)
        (list{line->String.substring(~start=indent, ~end=line->String.length), ...head}, lines)
      } else {
        (list{}, list{line, ...lines})
      }
    }
  }
  let rec parse = (lines: list<string>): list<document_token> => {
    switch lines {
    | list{} => list{}
    | list{line, ...lines} =>
      if line->String.startsWith("- ") {
        let line = line->String.substring(~start=2, ~end=line->String.length)
        let (head, lines) = takeAllIndented(2, lines)
        let token: document_token = ListItem({ordered: false, content: parseDocument(list{line, ...head})})
        list{token, ...parse(lines)}
      } else if line->String.startsWith("# ") {
        let line = line->String.substring(~start=2, ~end=line->String.length)
        let (head, lines) = takeAllIndented(2, lines)
        let token: document_token = ListItem({ordered: true, content: parseDocument(list{line, ...head})})
        list{token, ...parse(lines)}
      } else if line->String.startsWith("> ") {
        let line = line->String.substring(~start=2, ~end=line->String.length)
        let (head, lines) = takeAllIndented(2, lines)
        let token: document_token = Final(Blockquote(parseDocument(list{line, ...head})))
        list{token, ...parse(lines)}
      } else if line->String.startsWith("| ") {
        let line = line->String.substring(~start=2, ~end=line->String.length)
        let (head, lines) = takeAllIndented(2, lines)
        let token: document_token = TableElement({col: 0, content: parseDocument(list{line, ...head})})
        list{token, ...parse(lines)}
      } else if line->String.startsWith("|| ") {
        let line = line->String.substring(~start=3, ~end=line->String.length)
        let (head, lines) = takeAllIndented(3, lines)
        let token: document_token = TableElement({col: 1, content: parseDocument(list{line, ...head})})
        list{token, ...parse(lines)}
      } else if line->String.startsWith("||| ") {
        let line = line->String.substring(~start=4, ~end=line->String.length)
        let (head, lines) = takeAllIndented(4, lines)
        let token: document_token = TableElement({col: 2, content: parseDocument(list{line, ...head})})
        list{token, ...parse(lines)}
      } else if line == "|-" {
        let token: document_token = TableBreak
        list{token, ...parse(lines)}
      } else if line == "" {
        list{Empty, ...parse(lines)}
      } else {
        list{ParagraphLine(line), ...parse(lines)}
      } 
    }
  }
  and parseDocument = (lines: list<string>): document => {
    let lines = parse(lines)
    failwith("todo")
  }
  it
  ->String.split("\n")
  ->List.fromArray
  ->parseDocument
}

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
      [Final(SEval(token))]
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
  | SEval(x) => {
      let tag = "code"
      `<${tag}>${escape(x)}</${tag}>`
    }
  }
}
