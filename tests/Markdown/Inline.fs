module MdToHtml.Tests.Markdown.Inline

open NUnit.Framework
open MdToHtml.Modules.MarkdownParser

[<SetUp>]
let Setup () = ()

[<Test>]
let ShouldParseHeaderWithEmphasis () =
    let content =
        "# This is some header with *emphasis*".Split([| '\n' |]) |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Header(1, [ Text("This is some header with "); Emphasis([ Text("emphasis") ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseEmphasis () =
    let content = "*This is some text with emphasis*".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Emphasis([ Text("This is some text with emphasis") ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseStrong () =
    let content = "**This is some text with strong**".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [ Paragraph([ Strong([ Text("This is some text with strong") ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseStrongAndEmphasis () =
    let content =
        "**This is some text with _strong_ and _emphasis_**".Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph(
              [ Strong(
                    [ Text("This is some text with ")
                      Emphasis([ Text("strong") ])
                      Text(" and ")
                      Emphasis([ Text("emphasis") ]) ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseStrongAndEmphasisInHeader () =
    let content =
        "# **This is some text with _strong_ and _emphasis_**".Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Header(
              1,
              [ Strong(
                    [ Text("This is some text with ")
                      Emphasis([ Text("strong") ])
                      Text(" and ")
                      Emphasis([ Text("emphasis") ]) ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseTwoEmphasisInOneLine () =
    let content =
        "This is some text with *emphasis* and *emphasis2*".Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph(
              [ Text("This is some text with ")
                Emphasis([ Text("emphasis") ])
                Text(" and ")
                Emphasis([ Text("emphasis2") ]) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseInlineCode () =
    let content =
        "This is some text with `inline code`".Split([| '\n' |]) |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Text("This is some text with "); InlineCode("inline code") ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseInlineCodeWrappedInEmphasis () =
    let content =
        "This is some text with *`inline code`*".Split([| '\n' |]) |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Text("This is some text with "); Emphasis([ InlineCode("inline code") ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseInlineCodeWrappedInStrong () =
    let content =
        "This is some text with **`inline code`**".Split([| '\n' |]) |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Text("This is some text with "); Strong([ InlineCode("inline code") ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseInlineCodeWrappedInStrongAndEmphasis () =
    let content =
        "This is some text with **`inline code`** and *`inline code2`*"
            .Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph(
              [ Text("This is some text with ")
                Strong([ InlineCode("inline code") ])
                Text(" and ")
                Emphasis([ InlineCode("inline code2") ]) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseLink () =
    let content =
        "[This is a link](https://www.google.com)".Split([| '\n' |]) |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Link("https://www.google.com", "", [ Text("This is a link") ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseLinkWithStrongAndEmphasis () =
    let content =
        "[This is a link with **strong** and *emphasis*](https://www.google.com)"
            .Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph(
              [ Link(
                    "https://www.google.com",
                    "",
                    [ Text("This is a link with ")
                      Strong([ Text("strong") ])
                      Text(" and ")
                      Emphasis([ Text("emphasis") ]) ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseLinkWithInlineCode () =
    let content =
        "[This is a link with `inline code`](https://www.google.com)".Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Link("https://www.google.com", "", [ Text("This is a link with "); InlineCode("inline code") ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseLinkWithStrongAndEmphasisAndInlineCode () =
    let content =
        "[This is a link with **strong** and *emphasis* and `inline code`](https://www.google.com)"
            .Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph(
              [ Link(
                    "https://www.google.com",
                    "",
                    [ Text("This is a link with ")
                      Strong([ Text("strong") ])
                      Text(" and ")
                      Emphasis([ Text("emphasis") ])
                      Text(" and ")
                      InlineCode("inline code") ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseLinkWithStrongAndEmphasisAndInlineCodeInLinkText () =
    let content =
        "[This is a link with **strong** and *emphasis* and `inline code`](https://www.google.com)"
            .Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph(
              [ Link(
                    "https://www.google.com",
                    "",
                    [ Text("This is a link with ")
                      Strong([ Text("strong") ])
                      Text(" and ")
                      Emphasis([ Text("emphasis") ])
                      Text(" and ")
                      InlineCode("inline code") ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

let ShouldParseLinkWithTitle () =
    let content =
        "[This is a link with a title](https://www.google.com \"Google\")"
            .Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Link("https://www.google.com", "Google", [ Text("This is a link with a title") ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

let ShouldParseLinkWithTitleAndStrongAndEmphasis () =
    let content =
        "[This is a link with a title and **strong** and *emphasis*](https://www.google.com \"Google\")"
            .Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph(
              [ Link(
                    "https://www.google.com",
                    "Google",
                    [ Text("This is a link with a title and ")
                      Strong([ Text("strong") ])
                      Text(" and ")
                      Emphasis([ Text("emphasis") ]) ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseImage () =
    let content =
        "![This is an image](https://www.google.com)".Split([| '\n' |]) |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Image("https://www.google.com", "", "This is an image") ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseImageWithTitle () =
    let content =
        "![This is an image with a title](https://www.google.com \"Google\")"
            .Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Image("https://www.google.com", "Google", "This is an image with a title") ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseImageInLink () =
    let content =
        "[![This is an image](https://www.google.com)](https://www.google.com)"
            .Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Link("https://www.google.com", "", [ Image("https://www.google.com", "", "This is an image") ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseImageInLinkWithTitle () =
    let content =
        "[![This is an image with a title](https://www.google.com \"Google\")](https://www.google.com)"
            .Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph(
              [ Link(
                    "https://www.google.com",
                    "",
                    [ Image("https://www.google.com", "Google", "This is an image with a title") ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseEscapedCharacters () =
    let content =
        "This is some text with \\*escaped\\* \\\\ \\_ \\{ \\} \\[ \\] \\< \\> \\( \\) \\# \\+ \\- \\. \\! \\| characters"
            .Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Text("This is some text with *escaped* \\ _ { } [ ] &lt; &gt; ( ) # + - . ! | characters") ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldAllowHtml () =
    let content =
        "<p>This is some <strong>html</strong></p>".Split([| '\n' |]) |> List.ofArray

    let actual = parseMarkdown content

    let expected = [ Paragraph([ Text("<p>This is some <strong>html</strong></p>") ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldEncodeHtmlSpecialCharactersIfEscaped () =
    let content =
        "This is some text with \\<escaped\\> \\& \\\" \\' characters".Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Text("This is some text with &lt;escaped&gt; &amp; &quot; &apos; characters") ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))
