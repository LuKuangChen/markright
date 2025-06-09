open Main

let pad = (content) => {
  String.concat("  ", content->String.replaceAll("\n", "\n  "))
}

{
  let testName = "unordered list"
  let source = ["- Foobar", "- Hello"]->Array.join("\n")
  let wanted = ["<ul><li>Foobar</li><li>Hello</li></ul>"]->Array.join("")
  let actual = source->parseDocument->documentToString
  if actual != wanted {
    Console.log(`Test "${testName}" failed!`)
    Console.log(`Wanted:`)
    Console.log(pad(wanted))
    Console.log(`Actual:`)
    Console.log(pad(actual))
  }
}
