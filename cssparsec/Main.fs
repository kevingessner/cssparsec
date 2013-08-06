﻿module cssparsec.Main

open FParsec
open System

type SpecifyingSelector =
    | IdSelector of string
    | ClassSelector of string

type TagSelector =
    | TagSelector of string

type SingleSelector =
    | SpecifyingOnlySelector of SpecifyingSelector list
    | SpecifiedTagSelector of TagSelector * (SpecifyingSelector list)

type Selector =
    | DescendantSelector of SingleSelector list

let parseIdentifier : Parser<string, unit> =
    many1Satisfy2 isAsciiLower (fun c -> isAsciiLetter c || isDigit c || c = '_' || c = '-')

let parseIdSelector : Parser<SpecifyingSelector, unit> =
    (pchar '#' >>? parseIdentifier) |>> IdSelector

let parseClassSelector : Parser<SpecifyingSelector, unit> =
    (pchar '.' >>? parseIdentifier) |>> ClassSelector

let parseSpecifyingSelectors : Parser<SpecifyingSelector list, unit> =
    many (choice [parseIdSelector; parseClassSelector])

let parseTagSelector : Parser<TagSelector, unit> =
    (many1Satisfy isAsciiLower) |>> TagSelector

let parseSingleSelector : Parser<SingleSelector, unit> =
    attempt (tuple2 parseTagSelector parseSpecifyingSelectors) |>> SpecifiedTagSelector <|>
        (parseSpecifyingSelectors |>> SpecifyingOnlySelector)

let parseSelector : Parser<Selector, unit> =
    sepBy1 parseSingleSelector spaces1 |>> DescendantSelector