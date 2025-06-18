type extension = (string, Concepts.document) => list<Concepts.block>
let parseDocument: string => Concepts.document
let toHTMLString: (Markright.Concepts.document, ~extensions: dict<extension>=?) => string
let compile: (string, ~extensions: dict<extension>=?) => string
