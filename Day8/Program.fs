let myInput = """
..............U.............c.....3...............
.....p.........F..................................
.....m..7....................4x............3......
..e.............F..........c...YH..3..............
.......e...................................c..E..8
................a...U................8............
..............................4.F...8....x........
............7.....4............Hc..E.......x......
........p..............................E..........
.............U.e....................x....t........
.7..........................Z.H....g..............
.........7..m.....S.........................E.....
...F.....p...........6...SY.......................
.................6..k...................g.........
..........m......a........................g.......
.......M.......................................g..
..............a............Y....C........H........
....u.......6........a.........C.GY...............
.....M..................S......................2..
..........M........S.....................2........
........M.......................5.........z..f....
.....................................Z........t.2.
..........6.......................................
......................................G...........
.........................A.........G9....Z........
........................C.........................
.....k......................G......z..t...........
.......k......................zs....f........5...9
................h........................9....2...
.............h.....0...........f.....K..ZX........
..................................f...............
.......1....................9.........Xz..........
...............1......B.s......X..................
............h...............B.....................
..T.........k..................b..................
...............u..................................
.........u.............h..................0.......
..............y...................................
...............................t....X......5......
.................A............................5...
................u..................s..............
.T..........b....y................................
............y............................K........
..1...............................s....B..........
..............Ay.............B...P................
..........T.......................K...........0...
.............T..................P.........K.......
......A....P......................................
....b.........1...................................
.........b................................P.......
"""

let testInput = """
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"""

let input = myInput

let toMap (str:string) = 
    str.Trim().Split(System.Environment.NewLine)
    |> array2D

let map =
    input
    |> toMap

let findAntennas (str:string): (char*((int*int) list)) list =
    let ys = 
        str.Trim().Split(System.Environment.NewLine)
        |> List.ofArray
        |> List.mapi(fun y line -> (y, line))

    let posChars = 
        ys
        |> List.collect(fun (y, line) ->
            line.Trim()
            |> List.ofSeq
            |> List.mapi(fun x c -> (x, c))
            |> List.collect(fun (x, c) ->
                if c <> '.' then
                    [(x, y, c)]
                else []
            )
        )

    posChars
    |> List.groupBy(fun (x, y, c) -> c)
    |> List.map(fun (c, list) -> (c, list |> List.map(fun (x, y, _) -> (x, y))))

let findResonanceSpots (coordinates:(int*int) list): (int*int) list =
    let pairs = 
        (coordinates, coordinates)
        ||> List.allPairs 
        |> List.filter(fun (a, b) -> a <> b)
    
    //printfn $"%A{pairs}"

    pairs
    |> List.map(fun ((x1, y1), (x2, y2)) ->
        let dx = x2 - x1
        let dy = y2 - y1
        let r = (x2 + dx, y2 + dy)
        
        //printfn $"{(x1, y1)} + {(x2, y2)} = {r}"
       
        r
    )

let isInBoundsOf (map:char[,]) (coordinates:int*int) =
    let (x, y) = coordinates
    x >= 0 && x < map.GetLength(1) && y >= 0 && y < map.GetLength(0)

let antennas = 
    input
    |> findAntennas

//printfn $"%A{antennas}"

let spots = 
    antennas 
    |> List.map(fun a ->
        a
        |> snd
        |> findResonanceSpots
        |> List.filter(fun c -> c |> isInBoundsOf map)
        
    )

let spotCount =
    spots
    |> List.collect(fun l -> l)
    |> List.distinct
    |> List.length

printfn $"%A{spots}"

printfn $"{spotCount}"


// Part 2

let add t1 t2 = ((fst t1) + (fst t2), (snd t1) + (snd t2))

let harmonicSeries (start:(int*int)) (delta:(int*int)) = seq {
    let mutable pos = start
    while pos |> isInBoundsOf map do
        yield pos
        pos <- pos |> add delta
}

let findHarmonicResonanceSpots (coordinates:(int*int) list): (int*int) list =
    let pairs = 
        (coordinates, coordinates)
        ||> List.allPairs 
        |> List.filter(fun (a, b) -> a <> b)
    
    //printfn $"%A{pairs}"

    pairs
    |> List.collect(fun ((x1, y1), (x2, y2)) ->
        let dx = x2 - x1
        let dy = y2 - y1
        let series = 
            ((x1, y1), (dx, dy)) 
            ||> harmonicSeries
            |> List.ofSeq
        
        //printfn $"%A{series}"

        series
    )

let spots2 = 
    antennas 
    |> List.map(fun a ->
        a
        |> snd
        |> findHarmonicResonanceSpots
        |> List.filter(fun c -> c |> isInBoundsOf map)
        
    )

let spotCount2 =
    spots2
    |> List.collect(fun l -> l)
    |> List.distinct
    |> List.length

printfn $"%A{spots2}"

printfn $"{spotCount2}"
