// A translation of Peter Norvig’s Sudoku solver from Python to F#     http://www.norvig.com/sudoku.html
open System.IO

let inline isIn (l : 'a list) (i : 'a) = List.exists ((=) i) l   // exists is ~10X faster than contains
let center (s : string) (w : int) =
    let len = s.Length
    if w > len then s.PadLeft(((w - len) / 2) + len).PadRight(w) else s

let inline (>>=) m f = Option.bind f m

let rec allSome (values :  Option<'a>) (fList : list<'a -> Option<'a>>) : Option<'a> =
    match fList with
        | [] -> values
        | f::fl when Option.isSome values -> allSome (values >>= f) fl
        | _ -> values

let firstSome (values :  Option<'a>) (fList : list<'a -> Option<'a>>) : Option<'a> =
    let rec firstSomeRec (values :  Option<'a>) (initVal : Option<'a>) (fList : list<'a -> Option<'a>>) : Option<'a> =
         match fList with
            | [] -> values
            | f::fl when Option.isNone values -> firstSomeRec (initVal >>= f) initVal fl
            | _ -> values

    firstSomeRec None values fList

let digits = ['1' .. '9']
let rows = ['A' .. 'I']
let cols = digits

let cross (rows : char list) (cols : char list) : string list = 
    [for ch in rows do for d in cols -> ch.ToString() + d.ToString()]  // a string is ~45% faster than a tuple as an HashMap key

let squares = cross rows cols
let unitlist = 
    [for d in cols -> cross rows [d]] @
    [for ch in rows -> cross [ch] cols] @
    [for r in [rows.[0..2]; rows.[3..5]; rows.[6..8]] do for c in [cols.[0..2]; cols.[3..5]; cols.[6..8]] -> cross r c]

//  units is a dictionary where each square maps to the list of units that contain the square  
let units = HashMap [for s in squares -> s, unitlist |> List.filter (fun u -> s |> isIn u)]

//  peers is a dictionary where each square s maps to the set of squares formed by the union of the squares in the units of s, but not s itself 
let peers = HashMap [for s in squares -> s, set(units.[s] |> List.concat |> List.filter ((<>) s))]

let test : unit =
//  A set of unit tests.
    assert (squares.Length = 81)
    assert (unitlist.Length = 27)
    assert ([for s in squares -> units.[s]] |> Seq.forall (fun u -> u.Length = 3))
    assert ([for s in squares -> peers.[s]] |> Seq.forall (fun p -> p.Count = 20))
    assert (units.["C2"] = [["A2"; "B2"; "C2"; "D2"; "E2"; "F2"; "G2"; "H2"; "I2"];
                            ["C1"; "C2"; "C3"; "C4"; "C5"; "C6"; "C7"; "C8"; "C9"];
                            ["A1"; "A2"; "A3"; "B1"; "B2"; "B3"; "C1"; "C2"; "C3"]])
    assert (peers.["C2"] = set(["A2"; "B2"; "D2"; "E2"; "F2"; "G2"; "H2"; "I2";
                                "C1"; "C3"; "C4"; "C5"; "C6"; "C7"; "C8"; "C9";
                                "A1"; "A3"; "B1"; "B3"]))
    printfn "All tests pass."

let display (values : HashMap<string, char list>) : Option<HashMap<string, char list>> =
    let pvalues = dict[for s in squares -> s, new string (Array.ofList values.[s])]  
    let width = ([for s in squares -> String.length pvalues.[s]] |> List.max) + 1
    let line = String.concat "+" [for _ in 1 .. 3 -> String.replicate (width * 3) "-"]
    for ch in rows do
        printfn "%s" (String.concat "" [for d in digits -> center pvalues.[ch.ToString() + d.ToString()] width + (if d |> isIn ['3'; '6'] then "|" else "")]) 
        if ch |> isIn ['C'; 'F'] then printfn "%s" line
    printfn "" 
    Some values

let grid_values (grid : string) : Option<HashMap<string, char list>> =
//  Convert grid into a dict of (square, char list) with '0' or '.' for empties.
    let chars = [for ch in grid do if ch |> isIn digits || ch |> isIn ['.'; '0'] then yield [ch]]
    assert (chars.Length = 81)
    Some (HashMap (List.zip squares chars))

let rec assign (s : string) (d : char) (values : HashMap<string, char list>) : Option<HashMap<string, char list>> =

    let rec eliminate (s : string) (d : char) (values : HashMap<string, char list>) : Option<HashMap<string, char list>> =
  
        let rule1  (values : HashMap<string, char list>) : Option<HashMap<string, char list>> =
        //  (1) If a square s is reduced to one value d', then eliminate d' from the peers.
            match values.[s].Length with
                | 0 -> None         // Contradiction: removed last value
                | 1 -> let d' = values.[s].[0]
                       [for s' in peers.[s] -> eliminate s' d'] |> allSome (Some values)
                | _ -> Some values

        let rule2 (values : HashMap<string, char list>) : Option<HashMap<string, char list>> =
        //  (2) If a unit u is reduced to only one place for a value d, then put it there.
            [for u in units.[s] -> fun (v : HashMap<string, char list>) ->
                let dplaces = u |> List.filter (fun s' -> d |> isIn v.[s']) 
                match dplaces.Length with
                    | 0 -> None  // Contradiction: no place for this value
                    | 1 -> assign dplaces.[0] d v   //  # d can only be in one place in unit; assign it there
                    | _ -> Some v
            ] |> allSome (Some values)

    //  Eliminate d from values.[s] and propagate. Return Some values, except return None if a contradiction is detected.        
        if not (d |> isIn values.[s]) then 
            Some values        // Already eliminated
        else
            let values' = values |> HashMap.add s (values.[s] |> List.filter ((<>) d))
            values' |> rule1 >>= rule2

(*  Assign a value d by eliminating all the other values (except d) from values[s] and propagate.  
    Return Some values, except return None if a contradiction is detected. *)   
    let other_values = values.[s] |> List.filter ((<>) d)
    [for d' in other_values -> eliminate s d'] |> allSome (Some values)

let parse_grid (grid : string) : Option<HashMap<string, char list>> =
//  Convert grid to Some dict of possible values, [square, digits], or return None if a contradiction is detected. 
    let assignGrid (gvalues : HashMap<string, char list>)  =
        let values = HashMap (squares |> List.map (fun s -> s, digits))
        [for s in squares do for d in gvalues.[s] do if d |> isIn digits then yield assign s d] |> allSome (Some values)
    grid_values grid >>= assignGrid

let rec search (values : HashMap<string, char list>) : Option<HashMap<string, char list>> =
//  Using depth-first search and propagation, try all possible values.
    if seq {for s in squares -> values.[s].Length = 1} |> Seq.forall (id) then
        Some values   //    Solved!
    else
//      Choose the unfilled square s with the fewest possibilities
        let _, s = seq {for s in squares do if values.[s].Length > 1 then yield values.[s].Length, s} |> Seq.min
        [for d in values.[s] -> fun v -> assign s d v >>= search] |> firstSome (Some values) 
 
let solve (grid : string) : Option<HashMap<string, char list>> = 
    grid |> parse_grid >>= search

let solved (values : HashMap<string, char list>) : Option<HashMap<string, char list>> =
//  A puzzle is solved if each unit is a permutation of the digits 1 to 9
    let isUnitSolved u = Set (seq {for s in u -> values.[s]}) = Set (seq {for d in digits -> [d]})
    match seq {for u in unitlist -> isUnitSolved u} |> Seq.forall (id) with
        | true -> Some values
        | false -> None
    
let rnd = System.Random()

let rec random_puzzle (N : int) : string =
(* Make a random puzzle with N or more assignments. Restart on contradictions.
    Note the resulting puzzle is not guaranteed to be solvable, but empirically
    about 99.8% of them are solvable. Some have multiple solutions.    *)
    let shuffled (l : 'a list) : 'a list =
    //  https://rosettacode.org/wiki/Knuth_shuffle#F.23      
        let lst = List.toArray l
        let Swap i j =                                   // Standard swap
            let item = lst.[i]
            lst.[i] <- lst.[j]
            lst.[j] <- item
        let ln = lst.Length
        [0..(ln - 2)]                                     // For all indices except the last
        |> Seq.iter (fun i -> Swap i (rnd.Next(i, ln)))   // swap th item at the index with a random one following it (or itself)
        lst |> Array.toList

    let inline choice (l : 'a list) : 'a = l.[rnd.Next(l.Length)]
    
    let rec findPuzzle (values : Option<HashMap<string, char list>>) (sList : list<string>) : Option<HashMap<string, char list>> =
        match values with
            | None -> None
            | Some v -> 
                let ds = [for s in squares do if v.[s].Length = 1 then yield v.[s]]
                if ds.Length >= N && (List.distinct ds).Length >= 8 then
                    values
                else
                    match sList with
                        | [] -> None
                        | s::sl -> findPuzzle (assign s (v.[s] |> choice) v) sl

    let values = HashMap [for s in squares -> s, digits]
    match shuffled squares |> findPuzzle (Some values) with
        | None -> random_puzzle N
        | Some v -> String.concat "" [for s in squares -> if v.[s].Length = 1 then v.[s].ToString() else "."]

let solve_all (grids : seq<string>) (name) (showif : float Option) : unit =
(*  Attempt to solve a sequence of grids. Report results.
    When showif is a number of seconds, display puzzles that take longer.
    When showif is None, don't display any puzzles.     *)
    let time_solved grid showif =
        let timer = System.Diagnostics.Stopwatch.StartNew()
        let values = solve grid
        timer.Stop()
        let t = timer.Elapsed.TotalSeconds
        if showif |> Option.exists ((>) t) then
            grid |> grid_values >>= display |> ignore
            values >>= display |> ignore
            printfn "(%.2f seconds)" t
        t, values >>= solved

    let times, results = [for grid in grids -> time_solved grid showif] |> List.unzip
    let N = grids |> Seq.length
    if N > 1 then
        printfn "Solved %d of %d %s puzzles (avg %.2f secs (%.0f Hz), max %.2f secs)." 
                (results |> List.countWith (Option.isSome)) N name ((times |> List.sum) / float N)
                (float N / (times |> List.sum)) (times |> List.max)
[<EntryPoint>]
let main argv = 
    test
    solve_all (File.ReadLines "easy50.txt") "easy" None
    solve_all (File.ReadLines "top95.txt") "hard" None
    solve_all (File.ReadLines "hardest.txt") "hardest" None
    solve_all ([for _ in [1 .. 99] -> random_puzzle 17]) "random" (Some 0.02)
    0

