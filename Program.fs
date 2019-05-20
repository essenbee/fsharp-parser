// Task: parse (a + 3)*4 + z
// a + 3
// FParsec
// https://www.quanttec.com/fparsec/tutorial.html#parsing-a-float-between-brackets
// http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf

open System

type Parser<'T> = P of (string -> int -> Option<'T * int>)

let pchar : Parser<char> =
    P <| fun s i ->
        if i < s.Length then
            Some (s.[i], i+1) 
        else
            None

let prun (P p) s = p s 0

let preturn v : Parser<'T> = 
    P <| fun s i -> Some (v, i)

let pfail () : Parser<'T> = 
    P <| fun s i -> None

let pbind (uf : 'T -> Parser<'U>) (P t) : Parser<'U> =
    P <| fun s pos ->
        match t s pos with
        | None -> None
        | Some (tvalue, tpos) ->
            let (P uparser) = uf tvalue
            uparser s tpos 
        
let pcombine u t = t |> pbind (fun _ -> u)

let pmany (P t) : Parser<'T list> =
    P <| fun s pos ->
        let rec loop vs currentPos =
            match t s currentPos with
            | None -> Some (List.rev vs, currentPos)
            | Some (tvalue, tpos) -> loop (tvalue::vs) tpos
        loop [] pos

type ParserBuilder () =
    class
        // Enables let!
        member x.Bind (t, uf) = pbind uf t
        // Enables do!
        member x.Combine (t, u) = pcombine u t
        // Enables return
        member x.Return v = preturn v
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

let pmany1 t = 
    parser {
        let! head = t
        let! tail = pmany t
        return head::tail
    }

let pmap mapping t =
    parser {
        let! v = t 
        return mapping v
    }

let makeint vs =
    let foldingFunc aggr ch = 
        aggr * 10 + (int ch - int '0')
    vs |> List.fold foldingFunc 0

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
            let! ws = pmany1 pwhitespace
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

