// A translation of Peter Norvig’s Sudoku solver from Python to F#     http://www.norvig.com/sudoku.html
open System.Collections.Generic
open System.IO

let isIn (l : 'a list) (i : 'a) = List.contains i l
let center (s : string) (w : int) =
    let len = s.Length
    if w > len then s.PadLeft(((w - len) / 2) + len).PadRight(w) else s

let (>>=) m f = Option.bind f m

let rec allSome (values :  Option<'a>) (fList : list<'a -> Option<'a>>) : Option<'a> =
    if Option.isNone values || List.isEmpty fList then
        values
    else
      allSome (values >>= (List.head fList)) (List.tail fList)  

let firstSome (values :  Option<'a>) (fList : list<'a -> Option<'a>>) : Option<'a> =
    let rec firstSomeRec (values :  Option<'a>) (state : Option<'a>) (fList : list<'a -> Option<'a>>) : Option<'a> =
        if Option.isSome values || List.isEmpty fList then
            values
        else
          firstSomeRec (state >>= (List.head fList)) state (List.tail fList)  
    firstSomeRec None values fList

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
let units = HashMap [for s in squares -> s, [for u in unitlist do if s |> isIn u then yield u]]

//  peers is a dictionary where each square s maps to the set of squares formed by the union of the squares in the units of s, but not s itself 
let peers = HashMap [for s in squares -> s, [for u in units.[s] do for s' in u do if s' <> s then yield s']]

let display (values : HashMap<(char * char), char list>) : Option<HashMap<(char * char), char list>> =
    let pvalues = dict[for s in squares -> s, new string (Array.ofList values.[s])]  
    let width = ([for s in squares -> String.length pvalues.[s]] |> List.max) + 1
    let line = String.concat "+" [for _ in 1 .. 3 -> String.replicate (width * 3) "-"]
    for ch in rows do
        printfn "%s" (String.concat "" [for d in digits -> center pvalues.[ch, d] width + (if d |> isIn ['3'; '6'] then "|" else "")]) 
        if ch |> isIn ['C'; 'F'] then printfn "%s" line
    printfn "%s" ""
    Some values

let grid_values (grid : string) : Option<HashMap<(char * char), char list>> =
//  Convert grid into a dict of (square, char list) with '0' or '.' for empties.
    let chars = [for ch in grid do if ch |> isIn digits || ch |> isIn ['.'; '0'] then yield [ch]]
    assert (chars.Length = 81)
    Some (HashMap [for z in List.zip squares chars -> z]) 

let rec assign (s : char * char) (d : char) (values : HashMap<(char * char), char list>) : Option<HashMap<(char * char), char list>> =
    
    let rec eliminate (s : char * char) (d : char) (values : HashMap<(char * char), char list>) : Option<HashMap<(char * char), char list>> =
  
        let rule1  (values : HashMap<(char * char), char list>) : Option<HashMap<(char * char), char list>> =
        //  (1) If a square s is reduced to one value d', then eliminate d' from the peers.
            match values.[s].Length with
                | 0 -> None         // Contradiction: removed last value
                | 1 -> 
                    let d' = values.[s].[0]
                    [for s' in peers.[s] -> eliminate s' d'] |> allSome (Some values)
                | _ -> Some values

        let rule2 (values : HashMap<(char * char), char list>) : Option<HashMap<(char * char), char list>> =
        //  (2) If a unit u is reduced to only one place for a value d, then put it there.
            [for u in units.[s] -> fun v ->
                let dplaces = [for s' in u do if d |> isIn values.[s'] then yield s']  
                match dplaces.Length with
                    | 0 -> None  // Contradiction: no place for this value
                    | 1 -> assign dplaces.[0] d v   //  # d can only be in one place in unit; assign it there
                    | _ -> Some values
            ] |> allSome (Some values)

    (*  Eliminate d from values[s]; propagate when values or places <= 2.
        Return Some values, except return None if a contradiction is detected. *)        
        if not (d |> isIn values.[s]) then 
            Some values        // Already eliminated
        else
            let values' = values |> HashMap.add s (values.[s] |> List.filter (fun d' -> d' <> d))
            values' |> rule1 >>= rule2

(*  Assign a value d by eliminating all the other values (except d) from values[s] and propagate.  
    Return Some values, except return None if a contradiction is detected. *)   
    let other_values = values.[s] |> List.filter (fun d' -> d' <> d) 
    match other_values.Length with
        | 0 -> Some values      // Already assigned
        | _ -> [for d' in other_values -> eliminate s d'] |> allSome (Some values)
        | _ -> Some values

let parse_grid (grid : string) : Option<HashMap<(char * char), char list>> =
(*  Convert grid to Some dict of possible values, [square, digits], or
    return None if a contradiction is detected. *)
    let assignGrid (gvalues : HashMap<(char * char), char list>)  =
        let values = HashMap [for s in squares do yield s, digits]
        [for s in squares do for d in gvalues.[s] do if d |> isIn digits then yield assign s d] |> allSome (Some values)
    
    grid_values grid >>= assignGrid

let rec search (values : HashMap<(char * char), char list>) : Option<HashMap<(char * char), char list>> =
//  Using depth-first search and propagation, try all possible values.
    if seq {for s in squares -> values.[s].Length = 1} |> Seq.forall (fun b -> b) then
        Some values   //    Solved!
    else
//      Chose the unfilled square s with the fewest possibilities
        let _, s = seq {for s in squares do if values.[s].Length > 1 then yield values.[s].Length, s} |> Seq.min
        [for d in values.[s] -> fun v -> assign s d v >>= search] |> firstSome (Some values) 
 
let solve (grid : string) : Option<HashMap<(char * char), char list>> = 
    grid |> parse_grid >>= search

let solved (values : HashMap<(char * char), char list>) : Option<HashMap<(char * char), char list>> =
//  A puzzle is solved if each unit is a permutation of the digits 1 to 9
    let isUnitSolved u = Set (seq {for s in u -> values.[s]}) =  Set (seq {for d in digits -> [d]})
    match seq {for u in unitlist -> isUnitSolved u} |> Seq.forall (fun b -> b) with
        | true -> Some values
        | false -> None
    
let rnd = System.Random()

let rec random_puzzle (N : int) : string =
(* Make a random puzzle with N or more assignments. Restart on contradictions.
    Note the resulting puzzle is not guaranteed to be solvable, but empirically
    about 99.8% of them are solvable. Some have multiple solutions.    *)
    let shuffled (lst : 'a array) : 'a array =
//  https://rosettacode.org/wiki/Knuth_shuffle#F.23      
        let Swap i j =                                   // Standard swap
            let item = lst.[i]
            lst.[i] <- lst.[j]
            lst.[j] <- item

        let ln = lst.Length
        [0..(ln - 2)]                                     // For all indices except the last
        |> Seq.iter (fun i -> Swap i (rnd.Next(i, ln)))   // swap th item at the index with a random one following it (or itself)
        lst 

    let choice (l : 'a list) : 'a =
        l.[rnd.Next(l.Length)]
    
    let rec findPuzzle (values : Option<HashMap<(char * char), char list>>) (sList : list<char * char>) : Option<HashMap<(char * char), char list>> =
        match values with
            | None -> None
            | Some v -> 
                let ds = [for s in squares do if v.[s].Length = 1 then yield v.[s]]
                if ds.Length >= N && (List.distinct ds).Length >= 8 then
                    values
                else
                    if Seq.isEmpty sList then
                        None
                    else
                        let s = List.head sList
                        findPuzzle (assign s (v.[s] |> choice) v) (List.tail sList)
 
    let values = HashMap [for s in squares do yield s, digits]
    let squaresArray = List.toArray squares
    match [for s in shuffled squaresArray -> s] |> findPuzzle (Some values) with
        | None -> random_puzzle N
        | Some v -> String.concat "" [for s in squares -> if v.[s].Length = 1 then v.[s].ToString() else "."]

let solve_all (grids : seq<string>) (name) (showif : float Option) : unit =
(*  Attempt to solve a sequence of grids. Report results.
    When showif is a number of seconds, display puzzles that take longer.
    When showif is None, don't display any puzzles.     *)
    let time_solved grid showif =
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let values = solve grid
        timer.Stop()
        let t = timer.Elapsed.TotalSeconds
        if showif |> Option.exists (fun showif -> t > showif) then
            grid |> grid_values >>= display |> ignore
            values >>= display |> ignore
            printfn "(%.2f seconds)" t
        t, values >>= solved

    let times, results = [for grid in grids -> time_solved grid showif] |> List.unzip
    let N = grids |> Seq.length
    if N > 1 then
        printfn "Solved %d of %d %s puzzles (avg %.2f secs (%.0f Hz), max %.2f secs)." 
                (results |> List.countWith (fun o -> Option.isSome o)) N name ((times |> List.sum) / float N)
                (float N / (times |> List.sum)) (times |> List.max)
[<EntryPoint>]
let main argv = 
    solve_all (File.ReadLines "easy50.txt") "easy" None
    solve_all (File.ReadLines "top95.txt") "hard" None
    solve_all (File.ReadLines "hardest.txt") "hardest" None
    solve_all ([for _ in [1 .. 99] -> random_puzzle 17]) "random" (Some 100.0)
    System.Console.Read() |> ignore

    0

