open System.Collections.Generic

let isIn (l : 'a list) (i : 'a) = List.contains i l
let center (s : string) (w : int) =
    let len = s.Length
    if w > len then s.PadLeft(((w - len) / 2) + len).PadRight(w) else s

let combine m = 
    m |> List.reduce (fun m m' ->
        match (m, m') with
            | None, Some _ -> m
            | Some _, None -> m'
            | Some _, Some _ -> m'
            | None, None -> m'
        )

let (>>=) m f = Option.bind f m

let digits = "123456789" |> Seq.toList
let rows = "ABCDEFGHI" |> Seq.toList
let cols = digits

let cross (rows : char list) (cols : char list) : (char * char) list = 
    [for ch in rows do for d in cols -> ch, d]

let squares = cross rows cols
let unitlist = 
    [for d in cols -> cross rows [d]] @
    [for ch in rows -> cross [ch] cols] @
    [for r in [rows.[0..2]; rows.[3..5]; rows.[6..8]] do for c in [cols.[0..2]; cols.[3..5]; cols.[6..8]] -> cross r c]

//  units is a dictionary where each square maps to the list of units that contain the square  
let units = dict [for s in squares -> s, [for u in unitlist do if s |> isIn u then yield u]]

//  peers is a dictionary where each square s maps to the set of squares formed by the union of the squares in the units of s, but not s itself 
let peers = dict [for s in squares -> s, [for u in units.[s] do for s' in u do if s' <> s then yield s']]

let display (values : IDictionary<(char * char), char list>) : Option<IDictionary<(char * char), char list>> =
    let pvalues = dict[for s in squares -> s, new string (Array.ofList values.[s])]  
    let width = ([for s in squares -> String.length pvalues.[s]] |> List.max) + 1
    let line = String.concat "+" [for _ in 1 .. 3 -> String.replicate (width * 3) "-"]
    for ch in rows do
        printfn "%s" (String.concat "" [for d in digits -> center pvalues.[ch, d] width + (if d |> isIn ['3'; '6'] then "|" else "")]) 
        if ch |> isIn ['C'; 'F'] then printfn "%s" line
    printfn "%s" ""
    Some values

let grid_values (grid : string) : Option<IDictionary<(char * char), char list>> =
//  Convert grid into a dict of (square, char list) with '0' or '.' for empties.
    let chars = [for ch in grid do if ch |> isIn digits || ch |> isIn ['.'; '0'] then yield [ch]]
    assert (List.length chars = 81)
    dict [for z in List.zip squares chars -> z] |> display


let rec assign (values : IDictionary<(char * char), char list>) (s : char * char) (d : char) : Option<IDictionary<(char * char), char list>> =
    
    let rec eliminate (values : IDictionary<(char * char), char list>) (s : char * char) (d : char) : Option<IDictionary<(char * char), char list>> =
    
        let rule1  (values : IDictionary<(char * char), char list>) : Option<IDictionary<(char * char), char list>> =
        //  (1) If a square s is reduced to one value d', then eliminate d' from the peers.
            match List.length values.[s] with
                | 0 -> None         // Contradiction: removed last value
                | 1 -> 
                    let d' = values.[s].[0]
                    [for s' in peers.[s] -> eliminate values s' d'] |> combine
                | _ -> Some values

        let rule2 (values : IDictionary<(char * char), char list>) : Option<IDictionary<(char * char), char list>> =
        //  (2) If a unit u is reduced to only one place for a value d, then put it there.
            [for u in units.[s] ->
                let dplaces = [for s' in u do if d |> isIn values.[s'] then yield s']  
                match List.length dplaces with
                    | 0 -> None  // Contradiction: no place for this value
                    | 1 -> assign values dplaces.[0] d
                    | _ -> Some values
            ] |> combine

    (*  Eliminate d from values[s]; propagate when values or places <= 2.
        Return Some values, except return None if a contradiction is detected. *)        
        if not (d |> isIn values.[s]) then 
            Some values        // Already eliminated
        else
            values.[s] <- values.[s] |> List.filter (fun d' -> d' <> d)
            values |> rule1 >>= rule2

(*  Assign a value d by eliminating all the other values (except d) from values[s] and propagate.  
    Return Some values, except return None if a contradiction is detected. *)   
    let other_values = values.[s] |> List.filter (fun d' -> d' <> d) 
    match List.length other_values with
        | 0 -> Some values      // Already assigned
        | _ -> [for d' in other_values -> eliminate values s d'] |> combine


let parse_grid (grid : string) : Option<IDictionary<(char * char), char list>> =
(*  Convert grid to Some dict of possible values, square, digits}, or
    return None if a contradiction is detected. *)
    let values = new Dictionary<(char * char), char list>()    // mutable dictionary
    seq {for s in squares do yield s, digits} |> Seq.iter values.Add
   
    let assignGrid (gvalues : IDictionary<(char * char), char list>)  =
        [for s in squares do for d in gvalues.[s] do if d |> isIn digits then yield assign values s d] |> combine

    grid_values grid >>= assignGrid

let search (values : IDictionary<(char * char), char list>) : Option<IDictionary<(char * char), char list>> =
    Some values

let solve (grid : string) : Option<IDictionary<(char * char), char list>> = 
    grid |> parse_grid >>= search


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    assert (Array.length argv = 1)
    argv.[0] |> solve >>= display |> ignore

    0 // retourne du code de sortie entier

