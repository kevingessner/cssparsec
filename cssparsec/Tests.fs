module cssparsec.Tests

open FParsec
open System
open NUnit.Framework

open cssparsec.Main

let go parser text =
    match run parser text with
        | Success (r, _, _) -> r
        | Failure (m, _, _) -> raise (Exception m)

[<Test>]
let testIdSelector() =
    Assert.AreEqual(IdSelector "abc", go parseIdSelector "#abc")

[<Test>]
let testClassSelector() =
    Assert.AreEqual(ClassSelector "class", go parseClassSelector ".class")

[<Test>]
let testTagSelector() =
    Assert.AreEqual(TagSelector "span", go parseTagSelector "span")

[<Test>]
let testSelectorForSpecifiers() =
    Assert.AreEqual(SpecifyingOnlySelector [IdSelector "id"], go parseSingleSelector "#id")
    Assert.AreEqual(SpecifyingOnlySelector [ClassSelector "class"], go parseSingleSelector ".class")
    Assert.AreEqual(SpecifyingOnlySelector [ClassSelector "abc"; ClassSelector "defg"], go parseSingleSelector ".abc.defg")

[<Test>]
let testSelectorForTag() =
    Assert.AreEqual(SpecifiedTagSelector (TagSelector "span", []), go parseSingleSelector "span")

[<Test>]
let testSelectorForSpecifiedTags() =
    Assert.AreEqual(SpecifiedTagSelector (TagSelector "span", [IdSelector "foo"]), go parseSingleSelector "span#foo")
    Assert.AreEqual(SpecifiedTagSelector (TagSelector "span", [ClassSelector "foo"]), go parseSingleSelector "span.foo")
    Assert.AreEqual(SpecifiedTagSelector (TagSelector "span", [ClassSelector "foo"; ClassSelector "bar"]), go parseSingleSelector "span.foo.bar")
    Assert.AreEqual(SpecifiedTagSelector (TagSelector "span", [IdSelector "foo"; ClassSelector "bar"]), go parseSingleSelector "span#foo.bar")

[<Test>]
let testDescendantSelectors() =
    Assert.AreEqual(DescendantSelector [SpecifiedTagSelector (TagSelector "span", []);
                     SpecifiedTagSelector (TagSelector "div", [ClassSelector "foo"]);
                     SpecifyingOnlySelector [IdSelector "bar"]],
                    go parseSelector "span div.foo #bar")

[<EntryPoint>]
let main args = 0