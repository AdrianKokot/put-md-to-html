module MdToHtml.Tests.Markdown.List

open NUnit.Framework
open MdToHtml.Modules.MarkdownParser

[<SetUp>]
let Setup () = ()

[<Test>]
let ShouldParseOrderedList () =
    let content = "1. First\n2. Second".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ OrderedList(
              [ ListItem([ Paragraph([ Text("First") ]) ])
                ListItem([ Paragraph([ Text("Second") ]) ]) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseTwoOrderedListNextToEachOther () =
    let content = "1. First\n2. Second\n\n1. Third".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ OrderedList(
              [ ListItem([ Paragraph([ Text("First") ]) ])
                ListItem([ Paragraph([ Text("Second") ]) ]) ]
          )
          OrderedList([ ListItem([ Paragraph([ Text("Third") ]) ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseOrderedListWithItemsOnMultipleLines () =
    let content = "1. First\n   Second\n2. Next item".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ OrderedList(
              [ ListItem([ Paragraph([ Text("First") ]); Paragraph([ Text("Second") ]) ])
                ListItem([ Paragraph([ Text("Next item") ]) ]) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseOrderedListWithNestedOrderedList () =
    let content = "1. First\n   1. Nested".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ OrderedList(
              [ ListItem(
                    [ Paragraph([ Text("First") ])
                      OrderedList([ ListItem([ Paragraph([ Text("Nested") ]) ]) ]) ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseOrderedListWithNestedOrderedListWithMultipleItems () =
    let content =
        "1. First\n   1. Nested1\n   2. Nested2".Split([| '\n' |]) |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ OrderedList(
              [ ListItem(
                    [ Paragraph([ Text("First") ])
                      OrderedList(
                          [ ListItem([ Paragraph([ Text("Nested1") ]) ])
                            ListItem([ Paragraph([ Text("Nested2") ]) ]) ]
                      ) ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseOrderedListWithMultipleLineListItemsAndWithNestedOrderedListWithMultipleLineItems () =
    let content =
        "1. First List Item\n  First List Item Second Line\n   First List Item Third Line\n   1. First List Item Nested List First Item\n   First List Item Nested List First Item Second Line\n   First List Item Nested List First Item Third Line\n   2. First List Item Nested List Second Item\n   First List Item Nested List Second Item Second Line\n   First List Item Nested List Second Item Third Line"
            .Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ OrderedList(
              [ ListItem(
                    [ Paragraph([ Text("First List Item") ])
                      Paragraph([ Text("First List Item Second Line") ])
                      Paragraph([ Text("First List Item Third Line") ])
                      OrderedList(
                          [ ListItem(
                                [ Paragraph([ Text("First List Item Nested List First Item") ])
                                  Paragraph([ Text("First List Item Nested List First Item Second Line") ])
                                  Paragraph([ Text("First List Item Nested List First Item Third Line") ]) ]
                            )
                            ListItem(
                                [ Paragraph([ Text("First List Item Nested List Second Item") ])
                                  Paragraph([ Text("First List Item Nested List Second Item Second Line") ])
                                  Paragraph([ Text("First List Item Nested List Second Item Third Line") ]) ]
                            ) ]
                      ) ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseUnorderedList () =
    let content = "* First\n* Second".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ UnorderedList(
              [ ListItem([ Paragraph([ Text("First") ]) ])
                ListItem([ Paragraph([ Text("Second") ]) ]) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseTwoUnorderedListNextToEachOther () =
    let content = "* First\n* Second\n\n* Third".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ UnorderedList(
              [ ListItem([ Paragraph([ Text("First") ]) ])
                ListItem([ Paragraph([ Text("Second") ]) ]) ]
          )
          UnorderedList([ ListItem([ Paragraph([ Text("Third") ]) ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseUnorderedListWithItemsOnMultipleLines () =
    let content = "* First\n  Second\n* Next item".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ UnorderedList(
              [ ListItem([ Paragraph([ Text("First") ]); Paragraph([ Text("Second") ]) ])
                ListItem([ Paragraph([ Text("Next item") ]) ]) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseUnorderedListWithNestedUnorderedList () =
    let content = "* First\n  * Nested".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ UnorderedList(
              [ ListItem(
                    [ Paragraph([ Text("First") ])
                      UnorderedList([ ListItem([ Paragraph([ Text("Nested") ]) ]) ]) ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseUnorderedListWithNestedUnorderedListWithMultipleItems () =
    let content = "* First\n  * Nested1\n  * Nested2".Split([| '\n' |]) |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ UnorderedList(
              [ ListItem(
                    [ Paragraph([ Text("First") ])
                      UnorderedList(
                          [ ListItem([ Paragraph([ Text("Nested1") ]) ])
                            ListItem([ Paragraph([ Text("Nested2") ]) ]) ]
                      ) ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseUnorderedListWithMultipleLineListItemsAndWithNestedUnorderedListWithMultipleLineItems () =
    let content =
        "* First List Item\n  First List Item Second Line\n  First List Item Third Line\n  * First List Item Nested List First Item\n  First List Item Nested List First Item Second Line\n  First List Item Nested List First Item Third Line\n  * First List Item Nested List Second Item\n  First List Item Nested List Second Item Second Line\n  First List Item Nested List Second Item Third Line"
            .Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ UnorderedList(
              [ ListItem(
                    [ Paragraph([ Text("First List Item") ])
                      Paragraph([ Text("First List Item Second Line") ])
                      Paragraph([ Text("First List Item Third Line") ])
                      UnorderedList(
                          [ ListItem(
                                [ Paragraph([ Text("First List Item Nested List First Item") ])
                                  Paragraph([ Text("First List Item Nested List First Item Second Line") ])
                                  Paragraph([ Text("First List Item Nested List First Item Third Line") ]) ]
                            )
                            ListItem(
                                [ Paragraph([ Text("First List Item Nested List Second Item") ])
                                  Paragraph([ Text("First List Item Nested List Second Item Second Line") ])
                                  Paragraph([ Text("First List Item Nested List Second Item Third Line") ]) ]
                            ) ]
                      ) ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseUnorderedListWithDifferentSymbols () =
    let content = "* First\n+ Second\n- Third".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ UnorderedList(
              [ ListItem([ Paragraph([ Text("First") ]) ])
                ListItem([ Paragraph([ Text("Second") ]) ])
                ListItem([ Paragraph([ Text("Third") ]) ]) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseUnorderedListWithDifferentSymbolsAndNestedUnorderedList () =
    let content = "* First\n  + Nested".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ UnorderedList(
              [ ListItem(
                    [ Paragraph([ Text("First") ])
                      UnorderedList([ ListItem([ Paragraph([ Text("Nested") ]) ]) ]) ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))
