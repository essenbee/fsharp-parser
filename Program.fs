﻿open System

// Here we define a Parser of *T* as a **Type** that takes a *string* and an *int* (a position
// within that string), and returns an **Option** which is a tuple of (*T* and *int*).
// See also: https://fsharpforfunandprofit.com/posts/designing-with-types-single-case-dus/
// 1+2
type Parser<'T> = P of (string -> int -> Option<'T * int>)

// The basic building block, *pchar*. This fundamental function is of type **P**.
// It takes a string *str* and a position in the string *pos*, and returns
// either Some tuple of the character at *pos* and the incremented *pos*, or None.
// Note that is the _only_ place in the code where we compare *pos* with the
// length of *str*. This test is very neatly encapsulated within this one function.
//
let pchar : Parser<char> =
    P <| fun str pos ->
        if pos < str.Length then
            Some (str.[pos], pos+1) 
        else
            None

//This is a helper function that allows us to run *aParser* simply.
//
let prun (P aParser) str = aParser str 0

// The functions *preturn*, and *pfail* may seem pointless, but are in fact very helpful.
// *preturn* takes a *value* and always returns Some tuple of *value* and an unaltered *pos*.
//
let preturn value : Parser<'T> = 
    P <| fun str pos -> Some (value, pos)

// The function *pfail* takes unit and always returns None. See *psatisfy* later.
//
let pfail () : Parser<'T> = 
    P <| fun str pos -> None

// *pbind* follows a standard functional pattern. Here we want to take the output of
// one parser and feed that as the input into another parser. The problem is the inputs
// and outputs are incompatible! We defined a parser as producing an **Option<'T * int>** 
// and a parser, of course, takes a string and an int as its inputs, not an Option of 
// Some or None (or simply put, success or failure)! 
//` 
//` ![](2E0F27655D9D6D5B08C0D5204ECCFC46.png)

// We need to somehow provide a "combiner" that allows us to "chain" parsers
// together. The function *pbind* provides that combiner:
//
// * *ufunc* is a function that takes a **type** T and returns a parser of some type U
// * *tparser* is a parser of the _same_ type U
// 
// We run *str* and *pos* through the parser *tparser*. If that parser succeeds, it
// returns Some *tvalue* and *tpos*, we pass *tvalue* as an argument to *ufunc*.
// This function gives us back a new parser determined by the **type** of *tvalue*,
// not its value. It is to this new parser (*uparser*) that we subsequently pass *str*
// and the new *tpos*. of course, if *tparser* fails, then we just return None. 
//
let pbind (ufunc : 'T -> Parser<'U>) (P tparser) : Parser<'U> =
    P <| fun str pos ->
        match tparser str pos with
        | None -> None
        | Some (tvalue, newPos) ->
            let (P uparser) = ufunc tvalue
            uparser str newPos 

// Try it out:-
// let p = pchar |> pbind (fun first -> pchar |> pbind (fun second -> preturn (first, second)))
// prun p "Hello Marten from F#!" |> printfn "%A"
//
// This should produce **Some(('H','e'), 2)**, as it "chained" two *pchar* parsers together,
// and so gave us the first and second characters of the string, and moved the position
// in the string to 2.

// Given two parsers *uparser* and *tparser*, combine them using *pbind* as follows.
// Studying the definition of *pbind*, its clear that the lambda **fun _ -> uparser**,
// which is our *ufunc* in the *pbind* function, always gives back a *uparser*,
// no matter the type of the value returned from *tparser* (_ is the discard in F#).
// This means that **tparser |> pcombine uparser** runs *tparser*, discards the result,
// and returns the result of *uparser*.
//
let pcombine (uparser : Parser<'U>) (tparser : Parser<'T>) : Parser<'U> = tparser |> pbind (fun _ -> uparser)
let pkeepRight uparser tparser = pcombine uparser tparser
let pkeepLeft (uparser : Parser<'U>) (tparser : Parser<'T>) : Parser<'T> = 
  tparser |> pbind (fun tv -> uparser |> pbind (fun _ -> preturn tv))

// *pmany* takes a parser of a list of (*tparser*), and returns Some tuple of values and
// the position *pos* at which the parser failed. We can use this to parser 0 or more
// int in a string, for example: *prun pint "42Hello Marten from F#!" |> printfn "%A"*
//
let pmany (P tparser) : Parser<'T list> =
    P <| fun str pos ->
        let rec loop values currentPos =
            match tparser str currentPos with
            | None -> Some (List.rev values, currentPos) // reverse the list to regain proper ordering
            | Some (tvalue, tpos) -> loop (tvalue::values) tpos
        loop [] pos

// ((1 + 2) + 3) + 4
// int -> (string -> float)
let pchainl1 (term : Parser<'T>) (sep : Parser<'T -> 'T -> 'T>) : Parser<'T> =
    let (P termfun) = term
    let (P sepfun) = sep
    P <| fun str pos ->
        let rec loop aggr currentPos =
            match sepfun str currentPos with
            | None -> Some (aggr, currentPos)
            | Some (sepCombiner, sepPos) ->
                match termfun str sepPos with
                | None -> None
                | Some (termValue, termPos) -> loop (sepCombiner aggr termValue) termPos
        match termfun str pos with
        | None -> None
        | Some (termValue, termPos) -> loop termValue termPos


// A computation expression; this will make it simpler for us to build up more 
// complex parsers. This is a standard pattern in functional programming. Notice
// how it uses *pbind*, *pcombine* and *preturn* that we defined earlier.
//
type ParserBuilder () =
    class
        // Enables let! - binds the **result** of a call to another computation expression to a name
        // (compare with *await* in C#, that unwraps a Task<T> to give a result of T)
        member x.Bind (tparser, ufunc) = pbind ufunc tparser
        // Enables do! - used for calling another computation expression that returns a unit-like
        // type (as defined by the x.Zero member below)
        member x.Combine (tparser, uparser) = pcombine uparser tparser
        // Enables return - returns *value*
        member x.Return value = preturn value
        // Enables return! - realizes the value of a computation expression and wraps that result
        // as a Parser<'T>
        member x.ReturnFrom p = p : Parser<'T>
        // Allows if...then expression with no else - returns unit
        member x.Zero () = preturn ()
    end
let parser = ParserBuilder ()

// *pmap* takes a mapping function that maps a type T to a type U, and a parser of T.
// The mapping function is applied to the value parsed and is then returned. An F#
// *map* is functionally equivalent to Select in LINQ.
//
// For an example of using *pmap*, see *pint* below.
//
let pmap mappingFunc tparser =
    parser {
        let! value = tparser
        return mappingFunc value
    }


let ppair uparser tparser =
    parser {
        let! first = tparser
        let! second = uparser
        return first, second
    }
// (\d|\w)

let porElse (P uparser) (P tparser) =
    P <| fun str pos ->
        match tparser str pos with
        | None -> uparser str pos
        | Some (tvalue, tpos) -> Some (tvalue, tpos)


type Parser<'T> with
    static member (>>=) (t, uf) = pbind uf t
    static member (>>.) (t, u) = pkeepRight u t
    static member (.>>) (t, u) = pkeepLeft u t
    static member (.>>.) (t, u) = ppair u t
    static member (|>>) (t, m) = pmap m t
    static member (<|>) (t, u) = porElse u t

// From this point on, we express parsers using the computation expression above, as it
// simplifies combining parsers for many people.

// Similar to *pmany*, this parser works against 1 or more 'T, rather than 0 or more.
// For example, *pmany1 pletter* is a parse of 1 or more consecutive letters in a string.
//
let pmany1 tparser = 
    parser {
        let! head = tparser
        let! tail = pmany tparser
        return head::tail
    }

let pcreateParserForwardedToRef<'T> () : Parser<'T>*Parser<'T> ref =
    let dummyParser =
        P <| fun str pos ->
            failwith "Forwarded parser not initialized"
    let refToParser = ref dummyParser
    let forwardingParser = 
        P <| fun str pos ->
            let (P p) = !refToParser
            p str pos
    forwardingParser, refToParser

let psatisfy (satisfy : char -> bool) : Parser<char> =
    parser {
        let! char = pchar     // parse a char
        if satisfy char then  // if the satisfy function evaluates to true
            return char       // return the char
        else
            return! pfail ()  // else fail
    }

// A parser that parses a digit or fails.
//
let pdigit = psatisfy Char.IsDigit

// A parser that parses a letter or fails.
//
let pletter = psatisfy Char.IsLetter

// A parser that parses whitespace or fails.
//
let pwhitespace = psatisfy Char.IsWhiteSpace


// A useful helper function that, given a list of char digits, returns an single
// integer value. For example: // ['1'; '2'; '0'] becomes 120.
// Think LINQs Aggregate method in C#.
//
let makeint listOfChars =
    let foldingFunc aggr ch = 
        aggr * 10 + (int ch - int '0')
    listOfChars |> List.fold foldingFunc 0

// Pass into *pmap* a mapping function that turns a list of digits into an integer,
// and a parser that can parse one or more digits. The result is a strongly-typed 
// Parser<int> that can parse out a set of consecutive digits in a string, and produce
// an integer; how cool is that?
//
let pint = pmany1 pdigit |> pmap makeint

// A parser of strings, using *pmany1* to get a list of characters, and then *pmap* to
// map that list to a single string. *tparser* could be *tdigit* or *tletter*, for 
// example. Notice how the type of *pstring* is Parser<string>!
//
let pstring (tparser : Parser<char>) : Parser<string> =
    pmany1 tparser |> pmap (fun  charList -> charList |> List.toArray |> String)

// A parser that skips over a given character.
let pskipchar ch =
    parser {
        let! char = pchar
        if ch = char then   
            return ()
        else
            return! pfail ()
    }

//let padd =
//    parser {
//        let! first = pint      // first integer
//        do! pskipchar '+'      // using do! because skipchar returns unit-like value
//        let! second = pint     // second integer
//        return first + second  // return the sum
//    }

// Parser for a key-value pair of the form "<string>=<int>"
let pkeyvalue =
    parser {
        let! first = pstring pletter
        do! pskipchar '='
        let! second = pint
        return (first, second)
    }

// Parser for many key-value pairs e.g. "<string>=<int> <string>=<int> <string>=<int>"
let pkeyvalues =
    let helper =
        parser {
            let! whitespace = pmany1 pwhitespace
            return! pkeyvalue
        }

    parser{
        let! first = pkeyvalue
        let! tail = pmany helper
        return first::tail
    }

type BinaryOp = Add|Subtract|Multiply|Divide

// AST = Abstract Syntax Tree
type AST =
    | Const of int
    | Variable of string
    | BinaryOperation of AST*BinaryOp*AST
// 1+x*(3 + 1)
let x = 
    BinaryOperation (Const 1, Add, BinaryOperation (Variable "x", Multiply, Const 3))

// 1*(1+3)

let pForwardedAST, pRefToForwardedAST = pcreateParserForwardedToRef<AST> ()

let psubExpr = pskipchar '(' >>. pForwardedAST .>> pskipchar ')'

let pconst = pint |>> Const

//let pvariable = pchar |>> (fun c -> Variable (string c))
let pvariable = pstring pletter |>> Variable

let pop opChar operator = 
    pskipchar opChar 
    |>> fun c -> fun leftTree rightTree -> BinaryOperation (leftTree, operator, rightTree)

let padd = pop '+' Add
let psubtract = pop '-' Subtract
let pmultiply = pop '*' Multiply
let pdivide = pop '/' Divide

let pAllOp = padd <|> psubtract <|> pmultiply <|> pdivide

let pmultiOrDivide = pmultiply <|> pdivide
let paddOrSubtract = padd <|> psubtract

let pterm = psubExpr <|> pconst <|> pvariable

let pchainMultiDivide = pchainl1 pterm pmultiOrDivide
let pchainAddSubtract = pchainl1 pchainMultiDivide paddOrSubtract
do
    pRefToForwardedAST := pchainAddSubtract
let pAST = pchainAddSubtract

// https://github.com/stephan-tolksdorf/fparsec
// https://github.com/stephan-tolksdorf/fparsec/blob/master/Samples/JSON/parser.fs
// https://www.quanttec.com/fparsec/tutorial.html#
// https://github.com/mrange/fable-formlets/blob/master/WHY.md
[<EntryPoint>]
let main argv =
    //let p = pchar |> pbind (fun f -> pchar |> pbind (fun s -> preturn (f, s)))
    //let p = pchar |> pcombine pchar
    //let p = 
    //    parser {
    //        let! first = pchar
    //        let! second = pchar
    //        let! third = pchar
    //        return first, second, third
    //    }

    let p = pAST
    prun p "1" |> printfn "%A"
    prun p "1+2" |> printfn "%A"
    prun p "1+2-3*4" |> printfn "%A"
    prun p "(x+(2-3))*y" |> printfn "%A"

    //prun pint "42Hello Marten from F#!" |> printfn "%A"

    //prun pkeyvalues "answer=42 question=22 test=1" |> printfn "%A"



    0 // return an integer exit code

