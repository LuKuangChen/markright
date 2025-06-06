# Markright

Markright is a markdown-like language but done right. This repository describes the design of Markright and provides a compiler from Markright to HTML.

## Design

In-line format:

- (recursive) `**` means boldface
- (recursive) `//` means italic
- (recursive) `__` means underline
- (recursive) `!!` means highlight
- (atomic) ``` `` ``` means monospace
- (atomic) `""` quotes text and interprete them (by default, interprete as monospace verbatim), where the ending quote can be followed by an interpreter name.

Paragraph-level format:

```
- Unordered list
# Ordered list
> Block quote
| Table cell
|| Table cell at the second left column
||| etc., etc.
|- Table start / end / row break
""" block interp
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

### Run

```sh
node src/Demo.res.js
```
