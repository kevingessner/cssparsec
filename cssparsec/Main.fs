module cssparsec.Main

open FParsec
open System
open Microsoft.FSharp.Reflection

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

type TypeSelector =
    | TagSelector of string
    | UniversalSelector
    with
        override this.ToString() =
            match this with
            | TagSelector(s) -> s
            | UniversalSelector -> "*"

type SingleSelector =
    | SingleSelector of TypeSelector * (SpecifyingSelector list)
    with
        override this.ToString() =
            match this with
            | SingleSelector(UniversalSelector, []) -> UniversalSelector.ToString()
            | SingleSelector(UniversalSelector, l) -> String.Join("", l)
            | SingleSelector(t, l) -> t.ToString() + String.Join("", l)

let SpecifyingOnlySelector l =
    SingleSelector (UniversalSelector, l)

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
    let parseAttributeTail (k, t) =
        match t with
        | Some t1 -> parseIdentifier |>> (fun v -> AttributeValueSelector (k, t1, v))
        | None -> preturn (HasAttributeSelector k)
    let operationParsers =
        let cases = [for t in FSharpType.GetUnionCases typeof<AttributeValueSelectorOperation>
                         do yield FSharpValue.MakeUnion(t, [| |]) :?> AttributeValueSelectorOperation]
        [for t in cases do yield pstring (t.ToString()) >>% t]
    between (pchar '[') (pchar ']') (((parseIdentifier .>>. opt (choice operationParsers))) >>= parseAttributeTail)

let parsePseudoSelector : Parser<SpecifyingSelector, unit> =
    (pchar ':' >>? parseIdentifier) .>>. (opt (between (pchar '(') (pchar ')') (many1Satisfy (fun c -> c <> ')'))))
        |>> (fun (k, v) -> match v with
                           | None -> PseudoSelector k
                           | Some v1 -> PseudoSelectorWithArgument (k, v1))

let parseSpecifyingSelectors : Parser<SpecifyingSelector list, unit> =
    many (choice [parseIdSelector; parseClassSelector; parseAttributeSelector; parsePseudoSelector])

let parseTagSelector : Parser<TypeSelector, unit> =
    (many1Satisfy2 isAsciiLower (fun c -> isAsciiLower c || isDigit c)) |>> TagSelector

let parseSingleSelector : Parser<SingleSelector, unit> =
    (parseTagSelector <|> (optional (pstring "*") >>% UniversalSelector)) .>>. parseSpecifyingSelectors |>> SingleSelector

let parseSelectorSeparator : Parser<(Selector -> Selector -> Selector), unit> =
    let _unpackWithSeparatorType f : (Selector -> Selector -> Selector) =
        (fun a b -> match (a, b) with
                    | LoneSelector(s), s2 -> f (s, s2))
    (spaces1 >>% preturn DescendantSelector <|> preturn pzero)
        >>= (fun p -> choice [pchar '+' >>% SiblingSelector;
                              pchar '>' >>% ChildSelector;
                              p]) |>> _unpackWithSeparatorType

let parseSelector : Parser<Selector, unit> =
    chainr1 (parseSingleSelector |>> LoneSelector) (parseSelectorSeparator .>> spaces) .>> eof