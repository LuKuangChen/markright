open Main

let pad = content => {
  String.concat("  ", content->String.replaceAll("\n", "\n  "))
}

let testTranslate = (testName, source, wanted) => {
  let actual = source->parseDocument->documentToString
  if actual != wanted {
    Console.log(`Test "${testName}" failed!`)
    Console.log(`Wanted:`)
    Console.log(pad(wanted))
    Console.log(`Actual:`)
    Console.log(pad(actual))
  }
}

testTranslate(
  "Unordered list",
  ["- Foobar", "- Hello"]->Array.join("\n"),
  "<ul><li>Foobar</li><li>Hello</li></ul>",
)

testTranslate(
  "Ordered list",
  [". Foobar", ". Hello"]->Array.join("\n"),
  "<ol><li>Foobar</li><li>Hello</li></ol>",
)

testTranslate(
  "Blockquote",
  ["> Hello", "  world!"]->Array.join("\n"),
  "<blockquote>Hello world!</blockquote>",
)

testTranslate(
  "Table",
  [
    "| **Name**",
    "| **Age**",
    "|-",
    "| Alice",
    "| 19",
    "|-",
    "| Bob",
    "| 18",
    "|-",
    "| Calvin",
    "| 20",
  ]->Array.join("\n"),
  [
    "<table>",
    "<tr><td><strong>Name</strong></td><td><strong>Age</strong></td></tr>",
    "<tr><td>Alice</td><td>19</td></tr>",
    "<tr><td>Bob</td><td>18</td></tr>",
    "<tr><td>Calvin</td><td>20</td></tr>",
    "</table>",
  ]->Array.join(""),
)

testTranslate(
  "Paragraphs seperated by blank lines",
  ["Hello", "", "World"]->Array.join("\n"),
  "<p>Hello</p><p>World</p>",
)

testTranslate(
  "Lists seperated by blank lines",
  ["- Foobar", "", "- Hello"]->Array.join("\n"),
  "<ul><li>Foobar</li></ul><ul><li>Hello</li></ul>",
)

testTranslate(
  "Nested List",
  [
    "- Foobar",
    "  . A",
    "  . B",
    "  . C",
    "- Barzzz"
  ]->Array.join(""),
  "<ol><li>Foobar</li><li>Hello</li></ol>")
