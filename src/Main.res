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

type span_token =
  | Tag(tag)
  | Final(span)

let makeSpanParser = (k, delim: string, tag: tag) => (it: string): array<span_token> => {
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

let parseSpan: string => array<span_token> =
  (x => [Final(Plain(x))])
  ->makeSpanParser("/", Oblique)
  ->makeSpanParser("*", Boldface)
  ->makeSpanParser("`", Monospaced)
  ->makeSpanParser("^", Highlighted)
  ->makeSpanParser("_", Underscored)
  ->makeSpanParser("~", Strikethrough)

let parseParagraph = (it: string) => {
  let groupTokens = (tokens: array<span_token>): paragraph => {
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
    let rec f = (ts: list<span_token>): list<span> => {
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
      let elements = token->String.splitAtMost("|", ~limit=2)
      switch (elements->Array.get(0), elements->Array.get(1)) {
      | (Some(f), Some(x)) => [Final(Embeded(f, x))]
      | (Some(f), None) => [Final(Embeded(f, ""))]
      | _ => failwith(`invalid embeding: ${token}`)
      }
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
  | list{Final(block), ...ts} => list{block, ...groupLines(ts)}
  | list{Empty, ...ts} => groupLines(ts)
  }
}

let orElse = (fst: option<'x>, snd: unit => option<'x>): option<'x> => {
  switch fst {
  | Some(x) => Some(x)
  | None => snd()
  }
}

let orFinal = (fst: option<'x>, snd: unit => 'x): 'x => {
  switch fst {
  | Some(x) => x
  | None => snd()
  }
}

let parseDocument = (it: string): document => {
  let rec parse = (lines: list<string>): list<document_token> => {
    switch lines {
    | list{} => list{}
    | list{line, ...lines} =>
      let tryParseSubBlock = (mark, subblock: list<string> => document_token) => {
        if line->String.startsWith(mark->String.concat(" ")) {
          let line =
            line->String.substring(~start=1 + mark->String.length, ~end=line->String.length)
          let (head, lines) = takeAllIndented(2, lines)
          let block = list{line, ...head}
          let token: document_token = subblock(block)
          Some(list{token, ...parse(lines)})
        } else {
          None
        }
      }
      let tryParseSubDocument = (mark, subdoc: document => document_token) => {
        tryParseSubBlock(mark, b => subdoc(parseDocument(b)))
      }
      tryParseSubDocument("#", d => Final(Heading1(d)))
      ->orElse(() => tryParseSubDocument("##", d => Final(Heading2(d))))
      ->orElse(() => tryParseSubDocument("###", d => Final(Heading3(d))))
      ->orElse(() => tryParseSubDocument(">", d => Final(Quotation(d))))
      ->orElse(() => tryParseSubDocument(".", d => SubDocument(OrderedList, d)))
      ->orElse(() => tryParseSubDocument("-", d => SubDocument(UnorderedList, d)))
      ->orElse(() => tryParseSubDocument("o", d => SubDocument(CheckList(false), d)))
      ->orElse(() => tryParseSubDocument("x", d => SubDocument(CheckList(true), d)))
      ->orElse(() => tryParseSubDocument("|", d => SubDocument(TableElement, d)))
      ->orElse(() =>
        tryParseSubBlock("=", d => Final({
          switch d {
          | list{f, ...x} => {
              let x = x->List.toArray->Array.join("\n")
              Embeded(f, x)
            }
          | _ => failwith("invalid embedding")
          }
        }))
      )
      ->orFinal(() => list{
        if RegExp.test(%re("/^\s*$/g"), line) {
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

let asSpans = (document: document): list<span> => {
  switch document {
  | list{} => list{}
  | list{Raw(x)} => list{Raw(x)}
  | list{Paragraph(spans)} => spans
  | _ => failwith(`Expecting spans`)
  }
}

let checkListToString = (content: list<(bool, string)>): string => {
  `<ul>${content
    ->List.map(((checked, d)) => `<li><input type="checkbox" ${checked ? "checked" : ""}>${d}</li>`)
    ->List.toArray
    ->Array.join("")}</ul>`
}

let evaluator: dict<(string, document) => list<block>> = Dict.fromArray([
  ("raw", (content, _) => list{Paragraph(list{Raw(content)})}),
  (
    "now",
    (_content, _document) => {
      Js.Date.make()
      ->Js.Date.toISOString
      ->(x => list{Paragraph(list{Plain(x)})})
    },
  ),
  (
    "toc",
    (_, document) => {
      list{
        OrderedList(
          document->List.mapWithIndex((b, i) => {
            switch b {
            | Heading2(d) => {
              let rec subsectionsOf = (bs) => {
                switch bs {
                  | list{} => list{}
                  | list{Heading2(_), ..._} => list{}
                  | list{Heading3(d), ...bs} => list{d, ...subsectionsOf(bs)}
                  | list{_, ...bs} => subsectionsOf(bs)
                }
              }
              Some(list{...d, OrderedList(subsectionsOf(document->List.drop(i+1)->Option.getOr(list{})))})
            }
            | _ => None
            }
          })->List.filterMap(v => v),
        ),
      }
    },
  ),
])

let documentToString = (document): string => {
  let evaluate = (f, content): document => {
    switch evaluator->Dict.get(f) {
    | None => failwith(`Unknown evaluator ${f}`)
    | Some(f) => f(content, document)
    }
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
    | Raw(x) => x
    | Plain(x) => escape(x)
    | Embeded(f, x) => evaluate(f, x)->asSpans->spansToString
    }
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
        let tag = "blockquote"
        let content = content->documentToString
        `<${tag}>${content}</${tag}>`
      }
    | Table(content) => {
        let content = content->List.map(tableRowToString)->List.toArray->Array.join("")
        `<table>${content}</table>`
      }
    | Embeded(f, content) => evaluate(f, content)->documentToString
    | Raw(s) => s
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
  and documentToString = document => {
    switch document {
    | list{Paragraph(p)} => spansToString(p)
    | document =>
      document
      ->List.map(blockToString)
      ->List.toArray
      ->Array.join("")
    }
  }
  documentToString(document)
}
