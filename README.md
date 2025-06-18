# KC's Markright

KC's Markright is a like Markdown but done right. This repository describes the design of Markright and provides a compiler from Markright to HTML.

## Design Goals

Overall, the goal is to stay close to Markdown, because may people including me are already familiar with Markdown's syntax, to improve consistency, and to improve extensibility to suit several needs.

### Similar semantics and syntaxes

Markdown often keep the syntax and the semantics close. For example, it uses `-` for dashsed unordered list, and `**` for boldfacing.

However, this is not always the case. For example, it is unclear why `_` also means oblique or boldfacing rather than underscored.

### Tables

A Markdown table cell must contain a one-liner, which means lists, multiple paragraphs, etc. cannot appear inside a table.

### Multiple Code Style

Markdown offers only one syntax for embedding code: ``` `` ```. This makes it difficult when typesetting different kinds of "code". For example, one might want to typeset computer programs and computer outputs differently.

### Embedded Code

It is desirable to embed programs in the documents, such as getting user inputs and producing outputs.

## Design

A **document** is a list of *blocks*.

A **block** is one of the following

|         Meaning | Notation       | Content  |
| --------------: | -------------- | -------- |
|         Heading | Starts w/ `#`s | Document |
|       Quotation | Starts w/ `>`  | Document |
|    Ordered List | Starts w/ `.`  | Document |
|  Unordered List | Starts w/ `-`  | Document |
|   Table Element | Starts w/ `\|` | Document |
| Embeded Content | Starts w/ `=`  | Any      |
|       Paragraph | Otherwise      | Spans    |

A few notes on the table:

- When a block starts with a symbol, the symbol must be followed by a whitespace and the subsequent lines of the same block must be indented by two whitespaces.
- A heading's level is determined by the number of `#`s: one `#` means level-1 heading, `##` means level-2, and so on.
- Table rows are seperated by `|-`.

A **span** is one of the following

| Meaning         | Notation           | Content |
| --------------- | ------------------ | ------- |
| Oblique         | Wrap w/ `//`       | Spans   |
| Boldface        | Wrap w/ `**`       | Spans   |
| Monospaced      | Wrap w/ ``` `` ``` | Spans   |
| Highlighted     | Wrap w/ `^^`       | Spans   |
| Underscored     | Wrap w/ `__`       | Spans   |
| Strikethrough   | Wrap w/ `~~`       | Spans   |
| Embeded Content | Wrap w/ ```==```   | Any     |

An embeded content must specify how to process its content.
The process is specified with an utterance of the regular expression `[a-zA-Z][a-zA-Z0-9]*`.
A block-level embeded content specifies its process on its first line, where as
a span-level embeded content specifies its process at the beginning, seperated from its actual content with `|`. For example

```
= py
  # This is a piece of code to be processed by `py`,
  # which is supposed to highlight the code with
  # Python-style syntax highlighting
  print("Hello world!")

Programmers can present code inline, e.g., ==py|print("Hello world!")==.
```

## Development

### Installation

```sh
npm install
```

### Build

- Build: `npm run res:build`
- Clean: `npm run res:clean`
- Build & watch: `npm run res:dev`
