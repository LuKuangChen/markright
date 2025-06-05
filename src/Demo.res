let compile = (source: string): string => {
    source
    ->Main.parseParagraph
    ->Main.spansToString
}

Console.log("Hello, world!")

[
    "Foo**bar**",
    "Fo**//ba//r**",
]->Array.forEach(x => x->compile->Console.log)
