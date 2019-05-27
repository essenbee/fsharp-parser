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

// The function *pfail* takes unit and always returns None.
//
let pfail () : Parser<'T> = 
    P <| fun str pos -> None

// *pbind* follows a standard functional pattern. Here we want to take the output of
// one parser and feed that as the input into another parser. The problem is, we
// have defined a parser as producing an **Option<'T * int>** rather than a simple value.
// A parser, of course, takes a string and an int, not values wrapped up in an Option!
//
// * *ufunc* is a function that takes a **type** T and returns a parser of some type U
// * *tparser* is a parser of the same type U
// 
// We run *str* and *pos* through the parser *tparser*. If that parser returns Some
// *tvalue* and *tpos*, we pass *tvalue* as an agrument to *ufunc*. This function
// gives us back a new parser determined by the **type** of *tvalue*, not its value.
// It is to this new parser (*uparser*) that we subsequently pass *str* and *tpos*
// in order to return the **Option<'U * int>** we expect. 
//
let pbind (ufunc : 'T -> Parser<'U>) (P tparser) : Parser<'U> =
    P <| fun str pos ->
        match tparser str pos with
        | None -> None
        | Some (tvalue, tpos) ->
            let (P uparser) = ufunc tvalue
            uparser str tpos 

// Try it out: let p = pchar |> pbind (fun f -> pchar |> pbind (fun s -> preturn (f, s)))
//             prun p "Hello Marten from F#!" |> printfn "%A"
//
// This should produce **Some(('H','e'), 2)**, as it bound two pchar parsers together.

// Given two parsers *uparser* and *tparser*, combine then using *pbind* them.
// Note that the lambda fun _ -> uparser, which is our *ufunc* in the *pbind*
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

// A computational expression; this will make it simpler for us to build up more 
// complex parsers. This is a standard pattern in functional programming.
//
type ParserBuilder () =
    class
        // Enables let!
        member x.Bind (tparser, ufunc) = pbind ufunc tparser
        // Enables do!
        member x.Combine (tparser, uparser) = pcombine uparser tparser
        // Enables return
        member x.Return value = preturn value
        // Enables return!
        member x.ReturnFrom p = p : Parser<'T>
        // allows if x then expr with no else
        member x.Zero () = preturn ()
    end
let parser = ParserBuilder ()

let psatisfy (satisfy : char -> bool) : Parser<char> = 
    parser {
        let! char = pchar
        if satisfy char then   
            return char
        else
            return! pfail ()
    }

let pdigit = psatisfy Char.IsDigit

let pletter = psatisfy Char.IsLetter

let pwhitespace = psatisfy Char.IsWhiteSpace

// Similar to *pmany*, this parser works gainst 1 or more 'T, rather than 0 or more.
//
let pmany1 t = 
    parser {
        let! head = t
        let! tail = pmany t
        return head::tail
    }

// *pmap* is functionally equeivalent to SelectMany in C# LINQ.
//
let pmap mappingFunc t =
    parser {
        let! v = t 
        return mappingFunc v
    }

// Given a list of char digits, returns an single integer value. For example:
// ['1'; '2'; '0'] becomes 120. Think LINQs Aggregate method in C#.
//
let makeint listOfChars =
    let foldingFunc aggr ch = 
        aggr * 10 + (int ch - int '0')
    listOfChars |> List.fold foldingFunc 0

let pint = pmany1 pdigit |> pmap makeint

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
        let! first = pint
        do! pskipchar '+'
        let! second = pint
        return first + second
    }

// A parser of strings, using *pmany1* to get a list of characters, and then *pmap* to
// map that list to a single string.
//
let pstring (t : Parser<char>) : Parser<string> =
    pmany1 t |> pmap (fun  charList -> charList |> List.toArray |> String)

let pkeyvalue =
    parser {
        let! first = pstring pletter
        do! pskipchar '='
        let! second = pint
        return (first, second)
    }

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

