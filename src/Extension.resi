type t = (string, ~fullDocument: list<Concept.block>) => list<Concept.block>
let newBlock: ((string, ~fullDocument: list<Concept.block>) => Concept.block) => t
let newSpan: ((string, ~fullDocument: list<Concept.block>) => Concept.span) => t
let newRaw: ((string, ~fullDocument: list<Concept.block>) => string) => t
