type extension = (string, list<Concepts.block>) => list<Concepts.block>
let parseDocument: string => list<Concepts.block>
let toHTMLString: (list<Concepts.block>, ~extensions: dict<extension>=?) => string
let compile: (string, ~extensions: dict<extension>=?) => string
