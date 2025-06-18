let pad = content => {
  String.concat("  ", content->String.replaceAll("\n", "\n  "))
}

let makeTest = (testName, action, wanted) => {
  report => {
    let actual = action()
    if actual == wanted {
      report(None)
    } else {
      report(Some(testName))
      Console.log(`Test "${testName}" failed!`)
      Console.log(`Wanted:`)
      Console.log(pad(wanted))
      Console.log(`Actual:`)
      Console.log(pad(actual))
    }
  }
}

let runTests = tests => {
  let totalTest = ref(0)
  let failedTests = []
  let report = failure => {
    totalTest := totalTest.contents + 1
    failure->Option.forEach(testName => failedTests->Array.push(testName))
  }
  tests->Array.forEach(test => test(report))
  if failedTests->Array.length > 0 {
    Console.log(
      `Failed ${failedTests
        ->Array.length
        ->Int.toString} out of ${totalTest.contents->Int.toString} tests:`,
    )
    failedTests->Array.forEach(testName => Console.log(`- ${testName}`))
  } else {
    Console.log(`All ${totalTest.contents->Int.toString} test(s) passed!`)
  }
}

let testTranslate = (testName, source, wanted) => {
  makeTest(testName, () => source->Markright.compile, wanted)
}

runTests([
  testTranslate(
    "Unordered list",
    ["- Foobar", "- Hello"]->Array.join("\n"),
    "<ul><li>Foobar</li><li>Hello</li></ul>",
  ),
  testTranslate(
    "Ordered list",
    [". Foobar", ". Hello"]->Array.join("\n"),
    "<ol><li>Foobar</li><li>Hello</li></ol>",
  ),
  testTranslate(
    "Blockquote",
    ["> Hello", "  world!"]->Array.join("\n"),
    "<blockquote>Hello world!</blockquote>",
  ),
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
  ),
  testTranslate(
    "Paragraphs seperated by blank lines",
    ["Hello", "", "World"]->Array.join("\n"),
    "<p>Hello</p><p>World</p>",
  ),
  testTranslate(
    "Lists seperated by blank lines",
    ["- Foobar", "", "- Hello"]->Array.join("\n"),
    "<ul><li>Foobar</li></ul><ul><li>Hello</li></ul>",
  ),
  testTranslate(
    "Nested list",
    ["- Foobar", "  . A", "  . B", "  . C", "- Barzzz"]->Array.join("\n"),
    "<ul><li><p>Foobar</p><ol><li>A</li><li>B</li><li>C</li></ol></li><li>Barzzz</li></ul>",
  ),
  testTranslate(
    "TOC",
    ["= toc", "## Sec1", "foobar", "### Subsec 1.1", "## Sec 2"]->Array.join("\n"),
    "<ol><li><p>Sec1</p><ol><li>Subsec 1.1</li></ol></li><li><p>Sec 2</p><ol></ol></li></ol><h2>Sec1</h2><p>foobar</p><h3>Subsec 1.1</h3><h2>Sec 2</h2>",
  ),
])
