module MdToHtml.Tests.Markdown.BaseMarkdownSyntaxDocumentation


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
    let actual = parseMarkdown [line] 

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


[<Test>]
let ShouldParseOrderedList () =
    let content = "1. First\n2. Second\n3. Third\n4. Fourth".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ OrderedList(
              [ ListItem([ Paragraph([ Text("First") ]) ])
                ListItem([ Paragraph([ Text("Second") ]) ]) 
                ListItem([ Paragraph([ Text("Third") ]) ])
                ListItem([ Paragraph([ Text("Fourth") ]) ])]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseUnorderedListStars () =
    let content = "* First\n* Second\n* Third\n* Fourth".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ UnorderedList(
              [ ListItem([ Paragraph([ Text("First") ]) ])
                ListItem([ Paragraph([ Text("Second") ]) ])
                ListItem([ Paragraph([ Text("Third") ]) ])
                ListItem([ Paragraph([ Text("Fourth") ]) ])])]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseUnorderedListMinus() =
    let content = "- First\n- Second\n- Third\n- Fourth".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ UnorderedList(
              [ ListItem([ Paragraph([ Text("First") ]) ])
                ListItem([ Paragraph([ Text("Second") ]) ])
                ListItem([ Paragraph([ Text("Third") ]) ])
                ListItem([ Paragraph([ Text("Fourth") ]) ])])]

    Assert.That(actual, Is.EquivalentTo(expected))


[<Test>]
let ShouldParseUnorderedListPlus() =
    let content = "+ First\n+ Second\n+ Third\n+ Fourth".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ UnorderedList(
              [ ListItem([ Paragraph([ Text("First") ]) ])
                ListItem([ Paragraph([ Text("Second") ]) ])
                ListItem([ Paragraph([ Text("Third") ]) ])
                ListItem([ Paragraph([ Text("Fourth") ]) ])])]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseUnorderedListStartsWithNum() =
    let content = "- 1968\. A great year!\n- I think 1969 was second best.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ UnorderedList(
              [ ListItem([ Paragraph ([Text "1968. A great year!"]) ])
                ListItem([ Paragraph ([Text "I think 1969 was second best."]) ])])]

    Assert.That(actual, Is.EquivalentTo(expected))
    


[<Test>]
let ShouldParseUnorderedListParagraph() =
    let content = "* This is the first list item.\n* Here's the second list item.\nI need to add another paragraph below the second list item.\n* And here's the third list item.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ UnorderedList(
              [ListItem ([Paragraph ([Text "This is the first list item."])]);
                ListItem([Paragraph ([Text "Here's the second list item."; Text "I need to add another paragraph below the second list item."])]);
                ListItem([Paragraph ([Text "And here's the third list item."])])])];

    Assert.That(actual, Is.EquivalentTo(expected))



[<Test>]
let ShouldParseInlineCode () =
    let content = "At the command prompt, type `nano`.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [Paragraph [Text "At the command prompt, type "; InlineCode "nano"; Text "."]]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseCodeBlock () =
    let content = "```html\nUse `code` in your Markdown file.\n```".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [CodeBlock ("html", ["Use `code` in your Markdown file."])]
    
    Assert.That(actual, Is.EquivalentTo(expected))


[<Test>]
let ShouldParseHorizontalRuleThreeDashes () =
    let content = "Try to put a blank line before...\n---\n...and after a horizontal rule.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected =     [
        Paragraph [Text "Try to put a blank line before..."]
        HorizontalRule
        Paragraph [Text "...and after a horizontal rule."]
    ]
    
    Assert.That(actual, Is.EquivalentTo(expected))

    
[<Test>]
let ShouldParseHorizontalRuleThreeStars () =
    let content = "Try to put a blank line before...\n***\n...and after a horizontal rule.".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected =     [
        Paragraph [Text "Try to put a blank line before..."]
        HorizontalRule
        Paragraph [Text "...and after a horizontal rule."]
    ]
    
    Assert.That(actual, Is.EquivalentTo(expected))