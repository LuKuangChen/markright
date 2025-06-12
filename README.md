# KC's Markright

KC's Markright is a like Markdown but done right. This repository describes the design of Markright and provides a compiler from Markright to HTML.

## Problem of Markdown

## Design

In-line format:

- (recursive) `//` means oblique
- (recursive) `**` means boldface
- (recursive) `__` means underlined
- (recursive) `!!` means highlighted
- (atomic) ``` `` ``` means monospaced
- (atomic) `""` quotes text

Paragraph-level format:

```
- Unordered list
# Ordered list
> Block quote
| Table cell
|- Table start / end / row break
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
