module MdToHtml.Tests.Helpers

open NUnit.Framework
open MdToHtml.Modules.MarkdownParser

[<SetUp>]
let Setup () = ()

[<Test>]
[<TestCase("Hello, world!", ",", "Hello")>]
[<TestCase("Hello, world!", "!", "Hello, world")>]
[<TestCase("Hello, world!", "*", "Hello, world!")>]
[<TestCase("Hello, world!", "H", "")>]
[<TestCase("Some, text, with, multiple, separators", ",", "Some")>]
let TakeUntilTest (input: string) (separator: string) (expected: string) =
    let actual =
        takeUntil (separator.ToCharArray() |> List.ofArray) (input.ToCharArray() |> List.ofArray)
        |> System.String.Concat

    Assert.AreEqual(expected, actual)

[<Test>]
[<TestCase("Hello, world!", ",", false)>]
[<TestCase("Hello, world!", "H", true)>]
[<TestCase("Hello, world!", "h", false)>]
let StartsWithTest (input: string) (starts: string) (expected: bool) =
    let actual =
        startsWith (starts.ToCharArray() |> List.ofArray) (input.ToCharArray() |> List.ofArray)

    Assert.AreEqual(expected, actual)

[<Test>]
[<TestCase("Some, text, with, multiple, separators", ",", "Some, text, with, multiple")>]
[<TestCase("Some, text with one separator", ",", "Some")>]
[<TestCase("Some text with no separator", ",", "Some text with no separator")>]
[<TestCase("*some* text with *double* strong", "*", "*some* text with *double")>]
let TakeUntilLastTest (input: string) (separator: string) (expected: string) =
    let actual =
        takeUntilLast (separator.ToCharArray() |> List.ofArray) (input.ToCharArray() |> List.ofArray)
        |> System.String.Concat

    Assert.AreEqual(expected, actual)
