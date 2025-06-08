open Concepts

type list_element = {ordered: bool, content: document}

type table_element =
  | Cell(document)
  | Break

type document_token =
  | ParagraphLine(string)
  | ListElement(list_element)
  | TableElement(table_element)
  | Empty
  | Final(block)

let rec takeWhile = (xs, f) => {
  switch xs {
  | list{} => (list{}, list{})
  | list{x, ...xs} =>
    switch f(x) {
    | None => (list{}, list{x, ...xs})
    | Some(y) => {
        let (ys, xs) = takeWhile(xs, f)
        (list{y, ...ys}, xs)
      }
    }
  }
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

let parseDocument = (it: string): document => {
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
        let token: document_token = ListElement({
          ordered: false,
          content: parseDocument(list{line, ...head}),
        })
        list{token, ...parse(lines)}
      } else if line->String.startsWith("# ") {
        let line = line->String.substring(~start=2, ~end=line->String.length)
        let (head, lines) = takeAllIndented(2, lines)
        let token: document_token = ListElement({
          ordered: true,
          content: parseDocument(list{line, ...head}),
        })
        list{token, ...parse(lines)}
      } else if line->String.startsWith("> ") {
        let line = line->String.substring(~start=2, ~end=line->String.length)
        let (head, lines) = takeAllIndented(2, lines)
        let token: document_token = Final(Blockquote(parseDocument(list{line, ...head})))
        list{token, ...parse(lines)}
      } else if line->String.startsWith("| ") {
        let line = line->String.substring(~start=2, ~end=line->String.length)
        let (head, lines) = takeAllIndented(2, lines)
        let token: document_token = TableElement(Cell(parseDocument(list{line, ...head})))
        list{token, ...parse(lines)}
      } else if line == "|-" {
        let token: document_token = TableElement(Break)
        list{token, ...parse(lines)}
      } else if line == "" {
        list{Empty, ...parse(lines)}
      } else {
        list{ParagraphLine(line), ...parse(lines)}
      }
    }
  }
  and makeTable = (es: list<table_element>): list<list<document>> => {
    let rec collectRows = (row, es) => {
      switch es {
      | list{} => {
          let row = row->List.reverse
          list{row}
        }
      | list{Cell(cell), ...es} => collectRows(list{cell, ...row}, es)
      | list{Break, ...es} => {
          let row = row->List.reverse
          list{row, ...collectRows(list{}, es)}
        }
      }
    }
    collectRows(list{}, es)
  }
  and groupLines = (ts: list<document_token>): document => {
    switch ts {
    | list{} => list{}
    | list{TableElement(e), ...ts} => {
        let (es, ts) = takeWhile(ts, t => {
          switch t {
          | TableElement(e) => Some(e)
          | _ => None
          }
        })
        let es = list{e, ...es}
        list{Table(makeTable(es)), ...groupLines(ts)}
      }
    | list{ListElement({ordered: true, content}), ...ts} => {
        let (es, ts) = takeWhile(ts, t => {
          switch t {
          | ListElement({ordered: true, content}) => Some(content)
          | _ => None
          }
        })
        let es = list{content, ...es}
        list{List({ordered: true, content: es}), ...groupLines(ts)}
      }
    | list{ListElement({ordered: false, content}), ...ts} => {
        let (es, ts) = takeWhile(ts, t => {
          switch t {
          | ListElement({ordered: false, content}) => Some(content)
          | _ => None
          }
        })
        let es = list{content, ...es}
        list{List({ordered: false, content: es}), ...groupLines(ts)}
      }
    | list{ParagraphLine(line), ...ts} => {
        let (es, ts) = takeWhile(ts, t => {
          switch t {
          | ParagraphLine(content) => Some(content)
          | _ => None
          }
        })
        let es = list{line, ...es}
        list{Paragraph(parseParagraph(es->List.toArray->Array.join(" "))), ...groupLines(ts)}
      }
    | list{Final(block), ...ts} => list{block, ...groupLines(ts)}
    | list{Empty, ...ts} => groupLines(ts)
    }
  }
  and parseDocument = (lines: list<string>): document => {
    let lines = parse(lines)
    groupLines(lines)
  }
  it
  ->String.split("\n")
  ->List.fromArray
  ->parseDocument
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
