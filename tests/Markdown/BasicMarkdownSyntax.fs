﻿module MdToHtml.Tests.Markdown.BaseMarkdownSyntaxDocumentation

// TODO: Add tests that will cover all supported markdown syntax examples from the documentation - https://www.markdownguide.org/basic-syntax/

open NUnit.Framework
open MdToHtml.Modules.MarkdownParser

[<SetUp>]
let Setup () = ()

[<Test>]
[<TestCase("# Heading level 1", 1, "Heading level 1")>]
[<TestCase("## Heading level 2", 2, "Heading level 2")>]
[<TestCase("### Heading level 3", 3, "Heading level 3")>]
[<TestCase("#### Heading level 4", 4, "Heading level 4")>]
[<TestCase("##### Heading level 5", 5, "Heading level 5")>]
[<TestCase("###### Heading level 6", 6, "Heading level 6")>]
let ShouldParseHeader (line:string, level:int, text:string) =
    let actual = parseMarkdown [line] // Assuming parseMarkdown accepts a list of strings

    let expected = [ Header(level, [ Text(text) ]) ]
    Assert.That(actual, Is.EquivalentTo(expected))


[<Test>]
let ShouldParseParagraphWithMultipleLines () =
    let content = "This is the first line.  \nAnd this is the second line.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [Paragraph([Text("This is the first line."); LineBreak; Text("And this is the second line.")])]
    Assert.That(actual, Is.EquivalentTo(expected))


[<Test>]
let ShouldParseBold_2starsWithSpace () =
    let content = "I just love **bold text**.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [Paragraph [Text "I just love "; Strong [Text "bold text"]; Text "."]]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseBold_ () =
    let content = "I just love __bold text__.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [Paragraph [Text "I just love "; Strong [Text "bold text"]; Text "."]]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseBold_2starsWithoutSpace () =
    let content = "Love**is**bold".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [Paragraph [Text "Love"; Strong [Text "is"]; Text "bold"]]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseItalic_starWithSpace () =
    let content = "Italicized text is the *cat's meow*.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [Paragraph[Text "Italicized text is the "; Emphasis [Text "cat's meow"]; Text "."]]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseItalic_starWithoutSpace () =
    let content = "A*cat*meow".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [Paragraph[Text "A"; Emphasis [Text "cat"]; Text "meow"]]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseItalic_floor() =
    let content = "Italicized text is the _cat's meow_.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [Paragraph[Text "Italicized text is the "; Emphasis [Text "cat's meow"]; Text "."]]

    Assert.That(actual, Is.EquivalentTo(expected))

//[<Test>]
//let ShouldParseBoldAndItalic() =
//    let content = "This text is ***really important***.".Split([| '\n' |]) |> List.ofArray
//    let actual = parseMarkdown content

//    let expected =
//        [Paragraph [Text "This text is "; Strong [Text "really important"]; Text "."]]

//    Assert.That(actual, Is.EquivalentTo(expected))

//[<Test>]
//let ShouldParseBoldAndItalic_floor() =
//    let content = "This text is ___really important___.".Split([| '\n' |]) |> List.ofArray
//    let actual = parseMarkdown content

//    let expected =
//        [Paragraph [Text "This text is "; Strong [Text "really important"]; Text "."]]

//    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseBoldAndItalic_floorsAndStar() =
    let content = "This text is __*really important*__.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [Paragraph[Text "This text is "; Strong [Emphasis [Text "really important"]]; Text "."]]

    Assert.That(actual, Is.EquivalentTo(expected))


[<Test>]
let ShouldParseBoldAndItalic_floorAndStars() =
    let content = "This text is **_really important_**.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [Paragraph[Text "This text is "; Strong [Emphasis [Text "really important"]]; Text "."]]

    Assert.That(actual, Is.EquivalentTo(expected))
    
    
[<Test>]
let ShouldParseBlockQuote () =
    let content = "> Dorothy followed her through many of the beautiful rooms in her castle.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [BlockQuote [Paragraph [Text "Dorothy followed her through many of the beautiful rooms in her castle."]]]
    
    Assert.That(actual, Is.EquivalentTo(expected))


[<Test>]
let ShouldParseBlockQuoteWtihMultipleLines () =
    let content = "> Dorothy followed her through many of the beautiful rooms in her castle.\n> \n> The Witch bade her clean the pots and kettles and sweep the floor and keep the fire fed with wood.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [BlockQuote[Paragraph[Text "Dorothy followed her through many of the beautiful rooms in her castle."]; Paragraph [Text "The Witch bade her clean the pots and kettles and sweep the floor and keep the fire fed with wood."]]]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseBlockQuoteNastedWtihMultipleLines () =
    let content = "> Dorothy followed her through many of the beautiful rooms in her castle.\n> \n>> The Witch bade her clean the pots and kettles and sweep the floor and keep the fire fed with wood.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [BlockQuote[Paragraph[Text "Dorothy followed her through many of the beautiful rooms in her castle."]; BlockQuote[Paragraph[Text "The Witch bade her clean the pots and kettles and sweep the floor and keep the fire fed with wood."]]]]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseBlockQuoteWithOthers () =
    let content = "> #### The quarterly results look great!> \n> - Revenue was off the chart.\n> - Profits were higher than ever.\n> \n>  *Everything* is going according to **plan**.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [BlockQuote [Header (4, [Text "The quarterly results look great!> "]); UnorderedList [ListItem [Paragraph [Text "Revenue was off the chart."]]; ListItem [Paragraph [Text "Profits were higher than ever."]]]; Paragraph [Emphasis [Text "Everything"]; Text " is going according to "; Strong [Text "plan"]; Text "."]]]
    
    Assert.That(actual, Is.EquivalentTo(expected))
