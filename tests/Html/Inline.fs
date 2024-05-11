module MdToHtml.Tests.Html.Inline

open NUnit.Framework
open modules.MarkdownParser
open modules.HtmlRenderer

[<Test>]
let ShouldRenderEmphasis () =
    let content = [Emphasis([ Text("emphasis") ])]

    let actual = renderHtmlBody content

    let expected = "<em>emphasis</em>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderStrong () =
    let content = [Strong([ Text("strong") ])]

    let actual = renderHtmlBody content

    let expected = "<strong>strong</strong>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderNestedEmphasis () =
    let content = [Emphasis([ Strong([ Text("nested") ]) ])]

    let actual = renderHtmlBody content

    let expected = "<em><strong>nested</strong></em>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderInlineCode () =
    let content = [InlineCode("code")]

    let actual = renderHtmlBody content

    let expected = "<code>code</code>"

    Assert.That(actual, Is.EqualTo(expected))
