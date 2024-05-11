﻿module MdToHtml.Tests.Html.Block

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
let ShouldRenderHeader () =
    let content = [Header(1, [ Text("header") ])]

    let actual = renderHtmlBody content

    let expected = "<h1>header</h1>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderHeaderWithEmphasisAndStrong () =
    let content = [Header(1, [ Text("header "); Emphasis([ Text("emphasis "); Strong([ Text("strong") ]) ]) ])]

    let actual = renderHtmlBody content

    let expected = "<h1>header <em>emphasis <strong>strong</strong></em></h1>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderParagraph () =
    let content = [Paragraph([ Text("paragraph") ])]

    let actual = renderHtmlBody content

    let expected = "<p>paragraph</p>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderParagraphWithMultipleLines () =
    let content = [Paragraph([ Text("line1"); LineBreak; Text("line2") ])]

    let actual = renderHtmlBody content

    let expected = "<p>line1<br>line2</p>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderBlockquote () =
    let content = [BlockQuote([ Paragraph([ Text("quote") ]) ])]

    let actual = renderHtmlBody content

    let expected = "<blockquote><p>quote</p></blockquote>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderParagraphWithEmphasis () =
    let content = [Paragraph([ Text("line1 "); Emphasis([ Text("line2") ]) ])]

    let actual = renderHtmlBody content

    let expected = "<p>line1 <em>line2</em></p>"

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ShouldRenderParagraphWithStrong () =
    let content = [Paragraph([ Text("line1 "); Strong([ Text("line2") ]) ])]

    let actual = renderHtmlBody content

    let expected = "<p>line1 <strong>line2</strong></p>"

    Assert.That(actual, Is.EqualTo(expected))
