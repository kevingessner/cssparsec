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
let testAttributeSelector() =
    Assert.AreEqual(HasAttributeSelector "attr", go parseAttributeSelector "[attr]")
    Assert.AreEqual(AttributeValueSelector ("attr", IsEqualTo, "val"), go parseAttributeSelector "[attr=val]")
    Assert.AreEqual(AttributeValueSelector ("attr", ContainsWord, "val"), go parseAttributeSelector "[attr~=val]")
    Assert.AreEqual(AttributeValueSelector ("attr", ContainsLang, "val"), go parseAttributeSelector "[attr|=val]")
    ignore (Assert.Throws<Exception>(fun _ -> (ignore (go parseAttributeSelector "[attr^=val]"))))

[<Test>]
let testPseudoSelector() =
    Assert.AreEqual(PseudoSelector "first-line", go parsePseudoSelector ":first-line")
    Assert.AreEqual(PseudoSelectorWithArgument ("lang", "en"), go parsePseudoSelector ":lang(en)")

[<Test>]
let testSelectorForSpecifiers() =
    Assert.AreEqual(SpecifyingOnlySelector [IdSelector "id"], go parseSingleSelector "#id")
    Assert.AreEqual(SpecifyingOnlySelector [ClassSelector "class"], go parseSingleSelector ".class")
    Assert.AreEqual(SpecifyingOnlySelector [ClassSelector "abc"; ClassSelector "defg"], go parseSingleSelector ".abc.defg")
    Assert.AreEqual(SpecifyingOnlySelector [AttributeValueSelector ("foo", ContainsLang, "bar"); ClassSelector "abc"; AttributeValueSelector ("baz", ContainsWord, "quux")], go parseSingleSelector "[foo|=bar].abc[baz~=quux]")
    Assert.AreEqual(SpecifyingOnlySelector [PseudoSelector "foo"; PseudoSelectorWithArgument ("baz", "quux")], go parseSingleSelector ":foo:baz(quux)")

[<Test>]
let testSelectorForTag() =
    Assert.AreEqual(SpecifiedTagSelector (TagSelector "span", []), go parseSingleSelector "span")

[<Test>]
let testSelectorForSpecifiedTags() =
    Assert.AreEqual(SpecifiedTagSelector (TagSelector "span", [IdSelector "foo"]), go parseSingleSelector "span#foo")
    Assert.AreEqual(SpecifiedTagSelector (TagSelector "span", [ClassSelector "foo"]), go parseSingleSelector "span.foo")
    Assert.AreEqual(SpecifiedTagSelector (TagSelector "span", [ClassSelector "foo"; ClassSelector "bar"]), go parseSingleSelector "span.foo.bar")
    Assert.AreEqual(SpecifiedTagSelector (TagSelector "span", [IdSelector "foo"; ClassSelector "bar"]), go parseSingleSelector "span#foo.bar")
    Assert.AreEqual(SpecifiedTagSelector (TagSelector "span", [HasAttributeSelector "foo"; AttributeValueSelector ("bar", IsEqualTo, "quux")]), go parseSingleSelector "span[foo][bar=quux]")
    Assert.AreEqual(SpecifiedTagSelector (TagSelector "span", [PseudoSelector "foo"]), go parseSingleSelector "span:foo")

[<Test>]
let testUniversalSelector() =
    Assert.AreEqual(SpecifyingOnlySelector [], go parseSingleSelector "*")
    Assert.AreEqual(SpecifyingOnlySelector [IdSelector "id"], go parseSingleSelector "*#id")
    Assert.AreEqual(SpecifyingOnlySelector [ClassSelector "class"], go parseSingleSelector "*.class")
    Assert.AreEqual(SpecifyingOnlySelector [ClassSelector "abc"; ClassSelector "defg"], go parseSingleSelector "*.abc.defg")
    Assert.AreEqual(SpecifyingOnlySelector [AttributeValueSelector ("foo", ContainsLang, "bar"); ClassSelector "abc"; AttributeValueSelector ("baz", ContainsWord, "quux")], go parseSingleSelector "*[foo|=bar].abc[baz~=quux]")
    Assert.AreEqual(SpecifyingOnlySelector [PseudoSelector "foo"; PseudoSelectorWithArgument ("baz", "quux")], go parseSingleSelector "*:foo:baz(quux)")

[<Test>]
let testDescendantSelectors() =
    Assert.AreEqual(LoneSelector (SpecifiedTagSelector (TagSelector "span", [])),
                    go parseSelector "span");
    Assert.AreEqual(DescendantSelector (SpecifiedTagSelector (TagSelector "span", []),
                        DescendantSelector (SpecifiedTagSelector (TagSelector "div", [ClassSelector "foo"]),
                            LoneSelector (SpecifyingOnlySelector [IdSelector "bar"]))),
                    go parseSelector "span div.foo #bar")
    Assert.AreEqual(DescendantSelector (SpecifyingOnlySelector [HasAttributeSelector "baz"],
                        LoneSelector (SpecifiedTagSelector (TagSelector "span", []))),
                    go parseSelector "*[baz] span");

[<Test>]
let testSiblingSelectors() =
    Assert.AreEqual(SiblingSelector (SpecifiedTagSelector (TagSelector "div", [ClassSelector "foo"]),
                        LoneSelector (SpecifyingOnlySelector [IdSelector "bar"])),
                    go parseSelector "div.foo + #bar")

[<Test>]
let testChildSelectors() =
    Assert.AreEqual(ChildSelector (SpecifyingOnlySelector [ClassSelector "foo"],
                        LoneSelector (SpecifyingOnlySelector [])),
                    go parseSelector ".foo > *")

[<Test>]
let testMixedSelectors() =
    Assert.AreEqual(DescendantSelector (SpecifiedTagSelector (TagSelector "span", []),
                        SiblingSelector (SpecifiedTagSelector (TagSelector "div", [ClassSelector "foo"]),
                            LoneSelector (SpecifyingOnlySelector [IdSelector "bar"]))),
                    go parseSelector "span div.foo + #bar")
    Assert.AreEqual(SiblingSelector (SpecifiedTagSelector (TagSelector "span", []),
                        DescendantSelector (SpecifiedTagSelector (TagSelector "div", [ClassSelector "foo"]),
                            LoneSelector (SpecifyingOnlySelector [IdSelector "bar"]))),
                    go parseSelector "span + div.foo #bar")
    Assert.AreEqual(ChildSelector (SpecifiedTagSelector (TagSelector "span", []),
                        SiblingSelector (SpecifiedTagSelector (TagSelector "div", [ClassSelector "foo"]),
                            LoneSelector (SpecifyingOnlySelector [IdSelector "bar"]))),
                    go parseSelector "span > div.foo + #bar")
    Assert.AreEqual(ChildSelector (SpecifiedTagSelector (TagSelector "span", []),
                        ChildSelector (SpecifiedTagSelector (TagSelector "div", [ClassSelector "foo"]),
                            LoneSelector (SpecifyingOnlySelector [IdSelector "bar"]))),
                    go parseSelector "span > div.foo > #bar")

[<Test>]
let testToString() =
    let s = "span[baz] > #foo.bar #baz + a hr:first-letter + b[bar~=asdf] div > span[quux|=quux] article:lang(fr-FR)"
    Assert.AreEqual(s, (go parseSelector s).ToString())

[<EntryPoint>]
let main args = 0