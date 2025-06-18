let parseDocument: string => list<Concept.block>
let toHTMLString: (list<Concept.block>, ~extensions: dict<Extension.t>=?) => string
let compile: (string, ~extensions: dict<Extension.t>=?) => string
