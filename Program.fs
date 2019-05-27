open System

// Here we define a Parser of *T* as a **Type** that takes a *string* and an *int* (a position
// within that string), and returns an **Option** which is a tuple of (*T* and *int*).
// See also: https://fsharpforfunandprofit.com/posts/designing-with-types-single-case-dus/
//
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
// Some or None! 
//` 
//` ![](2E0F27655D9D6D5B08C0D5204ECCFC46.png)

// We need to somehow provide an "adapter" that allows us to "chain" parsers
// together. The function *pbind* provides that adapter:
//
// * *ufunc* is a function that takes a **type** T and returns a parser of some type U
// * *tparser* is a parser of the _same_ type U
// 
// We run *str* and *pos* through the parser *tparser*. If that parser returns Some
// *tvalue* and *tpos*, we pass *tvalue* as an agrument to *ufunc*. This function
// gives us back a new parser determined by the **type** of *tvalue*, not its value.
// It is to this new parser (*uparser*) that we subsequently pass *str* and the 
// new *tpos*. of course, if *tparser* returns None, then we just return that. 
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

// Given two parsers *uparser* and *tparser*, combine them using *pbind*.
// Note that the lambda **fun _ -> uparser**, which is our *ufunc* in the *pbind*
// function, always gives back a *uparser* no matter the type of the argument
// (_ is the discard in F#).
//
let pcombine uparser tparser = tparser |> pbind (fun _ -> uparser)

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

let padd =
    parser {
        let! first = pint      // first integer
        do! pskipchar '+'      // using do! because skipchar returns unit-like value
        let! second = pint     // second integer
        return first + second  // return the sum
    }

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

[<EntryPoint>]
let main argv =
    //let p = pchar |> pbind (fun f -> pchar |> pbind (fun s -> preturn (f, s)))
    //let p = pchar |> pcombine pchar
    let p = 
        parser {
            let! first = pchar
            let! second = pchar
            let! third = pchar
            return first, second, third
        }
    prun pint "42Hello Marten from F#!" |> printfn "%A"

    prun pkeyvalues "answer=42 question=22 test=1" |> printfn "%A"

    0 // return an integer exit code

