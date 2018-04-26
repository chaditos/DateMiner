module DateMiner.DateTimeCultureHeuristics

open System

/// Structure used to determine what date lexer to use for a particular piece of text.     
//type LexerContext = LexerContext of Format : DateFormat * Seperator : string

(*
/// Generates a regex 
let regexdatefromformat format sep =
    match format with
    | MDY -> 
        withregexmatch' (sprintf "^(%s)%s(%s)$" month sep day)
    | YMD -> sep
    | DMY -> sep
*)