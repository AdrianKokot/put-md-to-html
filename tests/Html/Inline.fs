module MdToHtml.Tests.Html.Inline

open NUnit.Framework
open MdToHtml.Modules.MarkdownParser
open MdToHtml.Modules.HtmlRenderer

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

[<Test>]
let ShouldRenderLink () =
    let content = [Link("url", "title", [ Text("content") ])]

    let actual = renderHtmlBody content

    let expected = "<a href=\"url\" title=\"title\">content</a>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderLinkWithoutTitle () =
    let content = [Link("url", "", [ Text("content") ])]

    let actual = renderHtmlBody content

    let expected = "<a href=\"url\">content</a>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderLinkWithStrong () =
    let content = [Link("url", "", [ Strong([ Text("strong") ]) ])]

    let actual = renderHtmlBody content

    let expected = "<a href=\"url\"><strong>strong</strong></a>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderLinkWithEmphasis () =
    let content = [Link("url", "", [ Emphasis([ Text("emphasis") ]) ])]

    let actual = renderHtmlBody content

    let expected = "<a href=\"url\"><em>emphasis</em></a>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderLinkWithInlineCode () =
    let content = [Link("url", "", [ InlineCode("code") ])]

    let actual = renderHtmlBody content

    let expected = "<a href=\"url\"><code>code</code></a>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderLinkWithStrongAndEmphasisAndInlineCode () =
    let content = [Link("url", "", [ Text("content"); Strong([ Text("strong") ]); Text(" and "); Emphasis([ Text("emphasis") ]); Text(" and "); InlineCode("code") ])]

    let actual = renderHtmlBody content

    let expected = "<a href=\"url\">content<strong>strong</strong> and <em>emphasis</em> and <code>code</code></a>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderImage () =
    let content = [Image("path", "title", "alt")]

    let actual = renderHtmlBody content

    let expected = "<img src=\"path\" title=\"title\" alt=\"alt\">"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderImageWithoutTitle () =
    let content = [Image("path", "", "alt")]

    let actual = renderHtmlBody content

    let expected = "<img src=\"path\" alt=\"alt\">"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderImageWithoutAlt () =
    let content = [Image("path", "title", "")]

    let actual = renderHtmlBody content

    let expected = "<img src=\"path\" title=\"title\">"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderImageWithoutTitleAndAlt () =
    let content = [Image("path", "", "")]

    let actual = renderHtmlBody content

    let expected = "<img src=\"path\">"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderLinkWithImage () =
    let content = [Link("url", "title", [ Image("path", "title", "alt") ])]

    let actual = renderHtmlBody content

    let expected = "<a href=\"url\" title=\"title\"><img src=\"path\" title=\"title\" alt=\"alt\"></a>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderLinkWithImageWithoutTitle () =
    let content = [Link("url", "title", [ Image("path", "", "alt") ])]

    let actual = renderHtmlBody content

    let expected = "<a href=\"url\" title=\"title\"><img src=\"path\" alt=\"alt\"></a>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderLinkWithImageWithoutAlt () =
    let content = [Link("url", "title", [ Image("path", "title", "") ])]

    let actual = renderHtmlBody content

    let expected = "<a href=\"url\" title=\"title\"><img src=\"path\" title=\"title\"></a>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderLinkWithImageWithoutTitleAndAlt () =
    let content = [Link("url", "title", [ Image("path", "", "") ])]

    let actual = renderHtmlBody content

    let expected = "<a href=\"url\" title=\"title\"><img src=\"path\"></a>"

    Assert.That(actual, Is.EqualTo(expected))

