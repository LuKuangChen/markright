open Concepts

type document_modifier =
  | OrderedList
  | UnorderedList
  | CheckList(bool)
  | TableElement

type document_token =
  | ParagraphLine(string)
  | SubDocument(document_modifier, document)
  | TableBreak
  | Empty
  | FinalB(block)

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

let makeSpanParser = (k, delim: string, tag: tag) => (it: string): array<token> => {
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

let parseSpan: string => array<token> =
  (x => [Final(Plain(x))])
  ->makeSpanParser("/", Oblique)
  ->makeSpanParser("*", Boldface)
  ->makeSpanParser("`", Monospaced)
  ->makeSpanParser("^", Highlighted)
  ->makeSpanParser("_", Underscored)
  ->makeSpanParser("~", Strikethrough)

let parseParagraph = (it: string) => {
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
  ->String.split(`==`)
  ->Array.flatMapWithIndex((token, i) => {
    if Int.mod(i, 2) == 0 {
      parseSpan(token)
    } else {
      [Final(EmbededS(token))]
    }
  })
  ->groupTokens
}

let rec takeAllIndented = (indent: int, lines: list<string>) => {
  switch lines {
  | list{} => (list{}, list{})
  | list{line, ...lines} =>
    if line == "" || line->String.startsWith(String.repeat(" ", indent)) {
      let line = line->String.substring(~start=indent, ~end=line->String.length)
      let (head, lines) = takeAllIndented(indent, lines)

      // If the last line is empty, do not consume it.
      // This is to ensure that empty lines can be used to break lists, tables, etc.
      if head == list{} && line == "" {
        (list{}, list{line, ...lines})
      } else {
        (list{line, ...head}, lines)
      }
    } else {
      (list{}, list{line, ...lines})
    }
  }
}

let makeTable = (es: list<option<document>>): list<list<document>> => {
  let rec collectRows = (row, es) => {
    switch es {
    | list{} => {
        let row = row->List.reverse
        list{row}
      }
    | list{Some(cell), ...es} => collectRows(list{cell, ...row}, es)
    | list{None, ...es} => {
        let row = row->List.reverse
        list{row, ...collectRows(list{}, es)}
      }
    }
  }
  collectRows(list{}, es)
}

let rec groupLines = (ts: list<document_token>): document => {
  switch ts {
  | list{} => list{}
  | list{TableBreak, ...ts} => {
      let (es, ts) = takeWhile(ts, t => {
        switch t {
        | TableBreak => Some(None)
        | SubDocument(TableElement, e) => Some(Some(e))
        | _ => None
        }
      })
      list{Table(makeTable(es)), ...groupLines(ts)}
    }
  | list{SubDocument(TableElement, e), ...ts} => {
      let (es, ts) = takeWhile(ts, t => {
        switch t {
        | TableBreak => Some(None)
        | SubDocument(TableElement, e) => Some(Some(e))
        | _ => None
        }
      })
      let es = list{Some(e), ...es}
      list{Table(makeTable(es)), ...groupLines(ts)}
    }
  | list{SubDocument(OrderedList, content), ...ts} => {
      let (es, ts) = takeWhile(ts, t => {
        switch t {
        | SubDocument(OrderedList, content) => Some(content)
        | _ => None
        }
      })
      let es = list{content, ...es}
      list{OrderedList(es), ...groupLines(ts)}
    }
  | list{SubDocument(UnorderedList, content), ...ts} => {
      let (es, ts) = takeWhile(ts, t => {
        switch t {
        | SubDocument(UnorderedList, content) => Some(content)
        | _ => None
        }
      })
      let es = list{content, ...es}
      list{UnorderedList(es), ...groupLines(ts)}
    }
  | list{SubDocument(CheckList(checked), content), ...ts} => {
      let (es, ts) = takeWhile(ts, t => {
        switch t {
        | SubDocument(CheckList(checked), content) => Some((checked, content))
        | _ => None
        }
      })
      let es = list{(checked, content), ...es}
      list{CheckList(es), ...groupLines(ts)}
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
  | list{FinalB(block), ...ts} => list{block, ...groupLines(ts)}
  | list{Empty, ...ts} => groupLines(ts)
  }
}

let parseDocument = (it: string): document => {
  let rec parse = (lines: list<string>): list<document_token> => {
    switch lines {
    | list{} => list{}
    | list{line, ...lines} =>
      let tryParseSubBlock = (mark, subblock: list<string> => document_token) => {
        if line->String.startsWith(mark->String.concat(" ")) {
          let line = line->String.substring(~start=2, ~end=line->String.length)
          let (head, lines) = takeAllIndented(2, lines)
          let token: document_token = subblock(list{line, ...head})
          Some(list{token, ...parse(lines)})
        } else {
          None
        }
      }
      let tryParseSubDocument = (mark, subdoc: document => document_token) => {
        tryParseSubBlock(mark, b => subdoc(parseDocument(b)))
      }
      None
      ->Option.orElse(tryParseSubDocument("#", d => FinalB(Heading1(d))))
      ->Option.orElse(tryParseSubDocument("##", d => FinalB(Heading2(d))))
      ->Option.orElse(tryParseSubDocument("###", d => FinalB(Heading3(d))))
      ->Option.orElse(tryParseSubDocument(">", d => FinalB(Heading2(d))))
      ->Option.orElse(tryParseSubDocument(".", d => SubDocument(OrderedList, d)))
      ->Option.orElse(tryParseSubDocument("-", d => SubDocument(UnorderedList, d)))
      ->Option.orElse(tryParseSubDocument("o", d => SubDocument(CheckList(false), d)))
      ->Option.orElse(tryParseSubDocument("x", d => SubDocument(CheckList(true), d)))
      ->Option.orElse(tryParseSubDocument("|", d => SubDocument(TableElement, d)))
      ->Option.orElse(tryParseSubDocument(".", d => SubDocument(OrderedList, d)))
      ->Option.orElse(
        tryParseSubBlock("=", d => FinalB(EmbededB(d->List.toArray->Array.join("\n")))),
      )
      ->Option.getOr(list{
        if RegExp.test(%re("/^\w*$/g"), line) {
          Empty
        } else if "|-" == line {
          TableBreak
        } else {
          ParagraphLine(line)
        },
        ...parse(lines),
      })
    }
  }
  and parseDocument = (lines: list<string>): document => {
    let lines: list<document_token> = parse(lines)
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
  | EmbededS(x) => {
      let tag = "code"
      `<${tag}>${escape(x)}</${tag}>`
    }
  }
}

let checkListToString = (content: list<(bool, string)>): string => {
  `<ul>${content
    ->List.map(((checked, d)) => `<li><input type="checkbox" ${checked ? "checked" : ""}>${d}</li>`)
    ->List.toArray
    ->Array.join("")}</ul>`
}

let rec blockToString = (block: block) => {
  switch block {
  | Heading1(p) => `<h1>${documentToString(p)}</h1>`
  | Heading2(p) => `<h2>${documentToString(p)}</h2>`
  | Heading3(p) => `<h3>${documentToString(p)}</h3>`
  | Paragraph(p) => `<p>${spansToString(p)}</p>`
  | OrderedList(content) => listToString(true, content)
  | UnorderedList(content) => listToString(false, content)
  | CheckList(content) =>
    checkListToString(content->List.map(((checked, d)) => (checked, documentToString(d))))
  | Quotation(content) => {
      let tag = "Quotation"
      let content = content->documentToString
      `<${tag}>${content}</${tag}>`
    }
  | Table(content) => {
      let content = content->List.map(tableRowToString)->List.toArray->Array.join("")
      `<table>${content}</table>`
    }
  | EmbededB(_content) => failwith("todo")
  }
}
and listToString = (ordered: bool, content: list<document>) => {
  let tag = ordered ? "ol" : "ul"
  let content =
    content
    ->List.map(documentToString)
    ->List.map(x => `<li>${x}</li>`)
    ->List.toArray
    ->Array.join("")
  `<${tag}>${content}</${tag}>`
}
and tableRowToString = (content: list<document>) => {
  let content = content->List.map(tableCellToString)->List.toArray->Array.join("")
  let tag = "tr"
  `<${tag}>${content}</${tag}>`
}
and tableCellToString = (content: document) => {
  `<td>${content->documentToString}</td>`
}
and documentToString = (document: document) => {
  switch document {
  | list{Paragraph(p)} => spansToString(p)
  | document =>
    document
    ->List.map(blockToString)
    ->List.toArray
    ->Array.join("")
  }
}
