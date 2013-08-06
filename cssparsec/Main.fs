module cssparsec.Main

open FParsec
open System

type AttributeValueSelectorOperation =
    | IsEqualTo
    | ContainsWord
    | ContainsLang
    with
        override this.ToString() =
            match this with
            | IsEqualTo -> "="
            | ContainsWord -> "~="
            | ContainsLang -> "|="

type SpecifyingSelector =
    | IdSelector of string
    | ClassSelector of string
    | HasAttributeSelector of string
    | AttributeValueSelector of string * AttributeValueSelectorOperation * string
    | PseudoSelector of string
    | PseudoSelectorWithArgument of (string * string)
    with
        override this.ToString() =
            match this with
            | IdSelector(s) -> "#" + s
            | ClassSelector(s) -> "." + s
            | HasAttributeSelector(s) -> "[" + s + "]"
            | AttributeValueSelector(k, t, v) -> String.Format("[{0}{1}{2}]", k, t, v)
            | PseudoSelector(s) -> ":" + s
            | PseudoSelectorWithArgument(k, v) -> String.Format(":{0}({1})", k, v)

type TagSelector =
    | TagSelector of string
    with
        override this.ToString() =
            match this with
            | TagSelector(s) -> s

type SingleSelector =
    | SpecifyingOnlySelector of SpecifyingSelector list
    | SpecifiedTagSelector of TagSelector * (SpecifyingSelector list)
    with
        override this.ToString() =
            match this with
            | SpecifyingOnlySelector(l) -> String.Join("", l)
            | SpecifiedTagSelector(t, l) -> t.ToString() + String.Join("", l)

type Selector =
    | DescendantSelector of SingleSelector * Selector
    | SiblingSelector of SingleSelector * Selector
    | ChildSelector of SingleSelector * Selector
    | LoneSelector of SingleSelector
    with
        override this.ToString() =
            match this with
            | DescendantSelector(h, t) -> String.Format("{0} {1}", h, t)
            | SiblingSelector(h, t) -> String.Format("{0} + {1}", h, t)
            | ChildSelector(h, t) -> String.Format("{0} > {1}", h, t)
            | LoneSelector(s) -> s.ToString()

let parseIdentifier : Parser<string, unit> =
    many1Satisfy2 isAsciiLower (fun c -> isAsciiLetter c || isDigit c || c = '_' || c = '-')

let parseIdSelector : Parser<SpecifyingSelector, unit> =
    (pchar '#' >>? parseIdentifier) |>> IdSelector

let parseClassSelector : Parser<SpecifyingSelector, unit> =
    (pchar '.' >>? parseIdentifier) |>> ClassSelector

let parseAttributeSelector : Parser<SpecifyingSelector, unit> =
    between (pchar '[') (pchar ']') (parseIdentifier .>>. choice [pchar '=' >>. parseIdentifier |>> (fun v -> (fun k -> AttributeValueSelector (k, IsEqualTo, v)));
                                                                  pstring "~=" >>. parseIdentifier |>> (fun v -> (fun k -> AttributeValueSelector (k, ContainsWord, v)));
                                                                  pstring "|=" >>. parseIdentifier |>> (fun v -> (fun k -> AttributeValueSelector (k, ContainsLang, v)));
                                                                  preturn (fun k -> HasAttributeSelector k)]) |>> (fun (k, f) -> f k)

let parsePseudoSelector : Parser<SpecifyingSelector, unit> =
    (pchar ':' >>? parseIdentifier) .>>. (opt (between (pchar '(') (pchar ')') (many1Satisfy (fun c -> c <> ')'))))
        |>> (fun (k, v) -> match v with
                           | None -> PseudoSelector k
                           | Some v1 -> PseudoSelectorWithArgument (k, v1))

let parseSpecifyingSelectors : Parser<SpecifyingSelector list, unit> =
    many (choice [parseIdSelector; parseClassSelector; parseAttributeSelector; parsePseudoSelector])

let parseTagSelector : Parser<TagSelector, unit> =
    (many1Satisfy2 isAsciiLower (fun c -> isAsciiLower c || isDigit c)) |>> TagSelector

let parseSingleSelector : Parser<SingleSelector, unit> =
    attempt (tuple2 parseTagSelector parseSpecifyingSelectors) |>> SpecifiedTagSelector <|>
        (optional (pstring "*") >>. parseSpecifyingSelectors |>> SpecifyingOnlySelector)

let parseSelectorSeparator : Parser<(Selector -> Selector -> Selector), unit> =
    let _unpackWithSeparatorType f : (Selector -> Selector -> Selector) =
        (fun a b -> match (a, b) with
                    | LoneSelector(s), s2 -> f (s, s2))
    choice [attempt (spaces >>. pchar '+' >>. preturn (_unpackWithSeparatorType SiblingSelector));
            attempt (spaces >>. pchar '>' >>. preturn (_unpackWithSeparatorType ChildSelector));
            spaces1 >>. preturn (_unpackWithSeparatorType DescendantSelector)]

let parseSelector : Parser<Selector, unit> =
    chainr1 (parseSingleSelector |>> (fun s -> LoneSelector s)) (parseSelectorSeparator .>> spaces)