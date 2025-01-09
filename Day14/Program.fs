﻿open System
open System.Text.RegularExpressions

let myInput = """
p=58,88 v=-38,-20
p=50,67 v=-42,-90
p=54,63 v=60,46
p=56,0 v=20,48
p=23,40 v=-80,-95
p=28,102 v=1,79
p=26,72 v=-96,46
p=38,22 v=-90,19
p=80,29 v=38,80
p=13,47 v=-67,-15
p=7,74 v=62,-21
p=61,102 v=65,-6
p=72,57 v=-56,37
p=96,60 v=-61,44
p=68,31 v=20,-72
p=65,6 v=-41,22
p=48,8 v=-43,66
p=69,15 v=-31,-17
p=97,37 v=85,6
p=10,26 v=49,5
p=40,7 v=-89,-73
p=48,99 v=31,-73
p=57,34 v=64,35
p=2,34 v=90,24
p=28,81 v=-95,92
p=92,84 v=5,47
p=67,32 v=-29,-21
p=47,83 v=-4,96
p=33,98 v=44,-68
p=39,70 v=-93,67
p=81,14 v=29,91
p=69,42 v=-72,-43
p=58,96 v=6,-73
p=31,60 v=-17,41
p=73,79 v=-22,72
p=83,72 v=18,-27
p=64,21 v=-84,81
p=14,2 v=35,-22
p=91,27 v=5,73
p=11,9 v=94,-68
p=80,96 v=82,27
p=24,97 v=47,-37
p=55,84 v=-89,51
p=70,89 v=90,91
p=97,93 v=18,90
p=51,1 v=-39,-86
p=88,61 v=74,-93
p=60,51 v=20,-25
p=45,22 v=-36,45
p=53,75 v=7,-5
p=65,98 v=60,-83
p=22,57 v=-81,11
p=82,19 v=26,-71
p=64,13 v=-83,94
p=92,53 v=-20,72
p=21,43 v=74,-77
p=76,94 v=-96,-30
p=42,52 v=-2,-7
p=91,13 v=-69,-32
p=6,1 v=84,-40
p=87,38 v=-21,96
p=62,13 v=66,17
p=13,100 v=-56,94
p=99,16 v=-22,-21
p=2,55 v=8,66
p=37,59 v=-93,-26
p=67,78 v=-84,59
p=85,37 v=77,-56
p=5,86 v=45,82
p=63,68 v=24,60
p=89,16 v=30,45
p=88,87 v=-21,15
p=79,8 v=-18,27
p=96,28 v=-20,-25
p=13,19 v=-10,-25
p=0,100 v=-23,-55
p=24,7 v=95,-55
p=46,37 v=57,-51
p=18,67 v=-54,-46
p=36,32 v=-51,6
p=81,14 v=-26,17
p=30,68 v=78,35
p=31,5 v=-47,-37
p=31,57 v=-22,83
p=14,40 v=85,42
p=96,74 v=86,-65
p=89,102 v=57,-62
p=99,3 v=83,79
p=96,22 v=-76,-81
p=32,34 v=-49,83
p=3,7 v=85,17
p=12,31 v=-12,-17
p=2,26 v=33,40
p=83,90 v=-32,-8
p=47,1 v=59,-55
p=9,84 v=-63,20
p=80,39 v=37,-41
p=94,40 v=-20,-38
p=100,90 v=-69,-52
p=81,30 v=66,-90
p=40,20 v=5,-9
p=13,102 v=89,43
p=3,82 v=-64,79
p=14,8 v=-57,70
p=43,63 v=49,93
p=92,31 v=6,-3
p=89,51 v=32,3
p=65,90 v=-27,59
p=23,23 v=-45,-6
p=89,69 v=18,-37
p=7,16 v=89,-45
p=47,59 v=-40,77
p=31,14 v=-48,-58
p=33,49 v=59,-46
p=61,60 v=13,26
p=69,78 v=13,-76
p=8,70 v=-25,53
p=0,75 v=32,45
p=50,33 v=7,-97
p=84,75 v=-99,-36
p=4,41 v=-16,-27
p=18,64 v=-75,-35
p=86,36 v=20,-15
p=49,64 v=-90,-98
p=24,45 v=-95,-34
p=61,30 v=-35,19
p=19,30 v=-7,6
p=53,26 v=-58,27
p=20,31 v=43,-35
p=50,55 v=-27,-84
p=36,2 v=56,66
p=85,93 v=-23,-42
p=14,16 v=-6,22
p=26,61 v=28,-3
p=12,42 v=91,21
p=32,63 v=8,-28
p=60,75 v=-94,-16
p=12,102 v=-16,27
p=16,20 v=-15,-50
p=65,77 v=-85,-34
p=81,15 v=80,-99
p=11,16 v=42,-81
p=75,83 v=-36,-6
p=47,44 v=26,-94
p=22,60 v=98,85
p=30,43 v=-5,37
p=55,75 v=8,23
p=49,92 v=-63,38
p=67,94 v=-74,-6
p=76,2 v=-71,40
p=29,73 v=-34,63
p=67,65 v=43,7
p=72,87 v=72,-57
p=19,50 v=-65,-4
p=78,25 v=-90,97
p=60,98 v=40,53
p=71,83 v=20,-21
p=23,28 v=-8,-71
p=1,98 v=34,-29
p=97,61 v=99,-27
p=17,65 v=32,-28
p=63,55 v=-42,93
p=16,6 v=40,-99
p=84,7 v=25,-86
p=3,53 v=35,8
p=78,27 v=-70,1
p=41,91 v=33,-62
p=28,14 v=-24,83
p=21,94 v=42,-16
p=100,74 v=-13,-49
p=4,84 v=-13,-70
p=36,57 v=-46,-90
p=4,62 v=-86,52
p=78,38 v=-77,-48
p=22,69 v=-55,41
p=59,61 v=31,-86
p=2,52 v=-7,-54
p=58,0 v=61,-78
p=93,52 v=58,75
p=30,64 v=-50,31
p=97,54 v=-5,63
p=14,59 v=91,-36
p=12,90 v=90,61
p=76,102 v=-73,27
p=32,26 v=53,-48
p=42,20 v=2,-76
p=94,31 v=-21,-12
p=51,23 v=-39,-53
p=37,0 v=52,-73
p=55,82 v=-19,-1
p=49,25 v=-40,-40
p=99,79 v=-3,67
p=91,70 v=-21,95
p=80,22 v=-27,99
p=3,1 v=-63,-32
p=14,98 v=10,88
p=89,25 v=-34,-57
p=51,10 v=63,-29
p=67,58 v=41,-61
p=75,19 v=-29,-25
p=51,58 v=12,-2
p=64,23 v=-32,81
p=99,48 v=19,81
p=7,13 v=87,58
p=22,43 v=-53,65
p=29,39 v=47,78
p=92,33 v=-18,-97
p=59,15 v=75,68
p=83,31 v=25,-12
p=44,95 v=56,79
p=85,74 v=-73,-39
p=88,11 v=-25,4
p=52,71 v=-40,-54
p=79,70 v=81,-34
p=1,16 v=37,94
p=40,61 v=7,-59
p=60,101 v=60,21
p=25,40 v=-54,-61
p=41,94 v=-47,-69
p=99,96 v=-64,-78
p=45,53 v=7,-15
p=86,88 v=-7,16
p=95,66 v=-19,31
p=99,74 v=-71,-65
p=90,74 v=-18,18
p=64,58 v=15,62
p=2,45 v=-12,73
p=93,13 v=-12,55
p=74,81 v=13,-5
p=21,37 v=-7,-20
p=10,95 v=-16,46
p=65,22 v=-81,-22
p=74,85 v=-37,-68
p=36,55 v=54,-64
p=35,64 v=6,44
p=16,98 v=26,70
p=15,46 v=-8,-10
p=34,78 v=19,67
p=93,34 v=6,-4
p=27,8 v=-97,86
p=87,33 v=80,-82
p=20,32 v=32,59
p=90,65 v=-21,31
p=8,37 v=41,6
p=69,83 v=16,-74
p=42,93 v=-96,-55
p=22,52 v=3,-51
p=32,66 v=94,49
p=100,72 v=64,-48
p=63,17 v=60,-19
p=75,61 v=-67,-38
p=79,99 v=-12,4
p=30,97 v=-99,-83
p=35,6 v=-80,70
p=44,79 v=-45,-34
p=62,88 v=13,56
p=64,20 v=-82,81
p=20,88 v=45,-24
p=49,49 v=-92,70
p=62,80 v=-66,47
p=49,28 v=88,-81
p=14,62 v=42,31
p=98,14 v=-64,4
p=2,53 v=82,52
p=40,77 v=-46,-93
p=45,98 v=-92,-24
p=78,13 v=-26,-14
p=25,33 v=96,78
p=47,95 v=-92,2
p=59,26 v=-36,-30
p=49,65 v=59,54
p=63,3 v=-36,38
p=37,8 v=-49,-58
p=37,18 v=-44,27
p=53,26 v=-39,37
p=94,93 v=-19,30
p=24,56 v=-11,-18
p=21,2 v=-10,-59
p=7,90 v=86,2
p=36,34 v=-1,-84
p=49,76 v=-40,15
p=59,16 v=57,73
p=55,26 v=62,-43
p=56,44 v=13,39
p=63,94 v=-78,28
p=94,46 v=9,35
p=87,102 v=89,-22
p=0,1 v=47,-19
p=20,101 v=47,-42
p=84,59 v=77,54
p=7,15 v=41,-94
p=43,45 v=13,31
p=51,29 v=68,-79
p=24,59 v=97,-41
p=41,28 v=63,27
p=44,97 v=-1,-73
p=12,20 v=-66,-7
p=24,7 v=82,41
p=18,10 v=-57,-71
p=51,33 v=-51,71
p=27,54 v=10,93
p=1,65 v=-77,38
p=20,0 v=88,-76
p=24,70 v=23,54
p=32,29 v=50,60
p=48,69 v=-43,-62
p=35,70 v=-13,-29
p=6,54 v=-72,-50
p=85,89 v=85,-44
p=39,32 v=57,-25
p=25,96 v=48,64
p=44,55 v=6,44
p=6,9 v=-2,-73
p=8,42 v=-4,60
p=22,100 v=-51,7
p=26,85 v=-6,-3
p=68,78 v=-59,76
p=12,47 v=-58,57
p=67,85 v=17,64
p=96,73 v=76,-41
p=19,14 v=-93,-34
p=41,31 v=-38,6
p=40,63 v=4,-36
p=20,100 v=20,-59
p=95,52 v=27,87
p=10,10 v=61,-49
p=25,49 v=47,66
p=27,68 v=-55,49
p=96,46 v=-66,65
p=26,14 v=-55,-40
p=90,22 v=18,56
p=58,9 v=-61,-66
p=78,9 v=-26,-22
p=37,93 v=55,7
p=86,80 v=-21,82
p=48,97 v=-96,56
p=17,77 v=-9,-44
p=42,87 v=-94,-88
p=99,97 v=-18,61
p=4,92 v=-21,-83
p=50,74 v=-42,80
p=43,16 v=7,-17
p=57,26 v=11,-89
p=75,62 v=65,-72
p=56,10 v=-90,-84
p=9,12 v=-14,53
p=63,1 v=12,79
p=29,44 v=3,51
p=55,6 v=-92,-53
p=13,101 v=-50,-54
p=0,70 v=-15,-18
p=14,33 v=-37,33
p=85,65 v=-69,21
p=37,43 v=31,69
p=100,91 v=-65,69
p=1,9 v=-66,-50
p=27,45 v=56,-23
p=80,69 v=25,-23
p=18,52 v=1,18
p=57,96 v=-88,-73
p=25,49 v=98,-2
p=1,96 v=-58,92
p=17,1 v=94,2
p=55,81 v=-38,56
p=35,32 v=54,65
p=21,20 v=5,97
p=94,59 v=-69,57
p=11,99 v=-47,-74
p=90,25 v=-87,65
p=97,95 v=36,-42
p=99,41 v=80,30
p=82,18 v=-76,55
p=8,67 v=40,23
p=76,83 v=-27,87
p=40,12 v=-89,30
p=56,94 v=-96,-91
p=76,66 v=19,-18
p=31,55 v=4,75
p=56,62 v=-36,-75
p=17,3 v=-60,99
p=84,27 v=72,58
p=41,28 v=-75,51
p=25,50 v=-61,-74
p=100,0 v=90,91
p=17,48 v=77,35
p=5,6 v=-63,-91
p=48,82 v=-88,18
p=91,10 v=79,53
p=65,2 v=75,-9
p=44,65 v=-97,85
p=91,19 v=73,58
p=37,93 v=20,-88
p=27,54 v=-53,29
p=79,35 v=-78,11
p=68,46 v=17,21
p=81,47 v=72,6
p=0,96 v=-68,-42
p=94,30 v=39,6
p=44,100 v=-43,33
p=25,35 v=89,71
p=88,7 v=-69,66
p=97,91 v=-64,77
p=53,14 v=-90,45
p=36,73 v=-5,15
p=11,2 v=-12,-24
p=89,68 v=-29,72
p=12,72 v=93,-26
p=65,88 v=65,-52
p=88,7 v=81,-32
p=19,38 v=92,60
p=31,80 v=50,-98
p=33,44 v=-99,13
p=41,50 v=-58,-55
p=95,51 v=-33,-9
p=64,42 v=-13,81
p=73,48 v=22,70
p=23,47 v=88,-84
p=34,87 v=9,66
p=70,5 v=83,50
p=17,73 v=-65,54
p=54,59 v=65,81
p=25,62 v=-56,70
p=78,102 v=31,-68
p=3,31 v=-67,-97
p=69,33 v=28,-19
p=95,12 v=-78,-79
p=36,2 v=-99,97
p=32,1 v=-7,4
p=49,88 v=-43,-21
p=45,91 v=3,15
p=18,87 v=-56,2
p=89,11 v=-73,63
p=17,80 v=-9,-25
p=93,55 v=-48,-74
p=82,69 v=-45,-99
p=16,14 v=-60,-94
p=14,7 v=88,66
p=77,7 v=66,32
p=1,72 v=-94,72
p=66,23 v=62,89
p=79,50 v=90,5
p=11,99 v=-62,-73
p=37,81 v=-48,5
p=36,32 v=3,-56
p=3,22 v=6,-29
p=66,82 v=-91,5
p=55,67 v=-42,15
p=27,86 v=99,77
p=46,38 v=-8,-60
p=96,47 v=-15,65
p=63,5 v=69,-1
p=20,88 v=-57,-3
p=30,84 v=50,87
p=57,55 v=11,30
p=6,55 v=88,13
p=96,43 v=-42,-21
p=14,64 v=-8,98
p=68,86 v=68,33
p=19,67 v=42,-31
p=60,7 v=18,48
p=61,96 v=-88,64
p=78,85 v=-83,-73
p=14,28 v=52,16
p=82,78 v=-75,-93
p=28,67 v=-50,-5
p=10,68 v=-45,-76
p=64,34 v=21,19
p=44,29 v=87,-63
p=36,14 v=3,50
p=27,50 v=-51,60
p=67,54 v=-82,93
p=85,69 v=32,-72
p=36,33 v=91,17
p=63,92 v=17,25
p=81,99 v=-47,51
p=92,49 v=-44,27
p=28,63 v=97,-34
p=74,46 v=-2,58
p=7,62 v=38,31
p=73,25 v=-29,91
p=32,80 v=-93,54
p=77,93 v=29,47
p=8,45 v=-19,-56
p=68,35 v=56,-75
p=27,46 v=-49,-12
p=86,102 v=26,97
p=74,63 v=-75,46
p=3,32 v=88,60
p=95,73 v=82,10
p=51,59 v=-33,16
p=52,77 v=-45,72
p=69,77 v=77,-88
p=59,81 v=-29,38
p=65,12 v=23,97
p=14,55 v=-9,-15
p=2,35 v=7,-37
p=27,47 v=84,-76
p=84,45 v=74,65
p=9,2 v=-11,-42
p=76,14 v=25,-94
"""

let testInput = """
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
"""

//let input = testInput
//let mapDim = (11,7)

let input = myInput
let mapDim = (101, 103)

type Position = int*int
type Velocity = int*int
type Robot = 
    {
        Pos: Position
        Vel: Velocity
    }
    member inline this.px = fst this.Pos
    member inline this.py = snd this.Pos
    member inline this.vx = fst this.Vel
    member inline this.vy = snd this.Vel

let parse (input: string): Robot list = 
    input.Trim().Split(Environment.NewLine)
    |> List.ofArray
    |> List.map (fun line ->
        match Regex(@"p=(\d+),(\d+) v=(-?\d+),(-?\d+)").Match(line) with
        | m when m.Success -> {Pos = (int m.Groups[1].Value, int m.Groups[2].Value); Vel = (int m.Groups[3].Value, int m.Groups[4].Value)}
        | _ -> failwith $"Invalid format in line: {line}."
    )

let robots = input |> parse

// Part 1

let addMod ((px, py): Position) ((vx, vy): Velocity) ((mw, mh): int*int): Position = 
    ((px + mw + vx) % mw, (py + mh + vy) % mh)

let update (robots:Robot list) (mapDim: int*int): Robot list =
    robots
    |> List.map (fun r -> { Pos = addMod r.Pos r.Vel mapDim; Vel = r.Vel} )

let run (secs: int) (mapDim: int*int) (robots: Robot list): Robot list =
    (robots, [1..secs])
    ||> List.fold (fun robots _ -> update robots mapDim)

let quadrant ((mw, mh): int*int) (robot: Robot): int =
    match robot.Pos with
    | (px:int, py:int) when px < mw / 2 && py < mh / 2 -> 1
    | (px:int, py:int) when px < mw / 2 && py > mh / 2 -> 2
    | (px:int, py:int) when px > mw / 2 && py < mh / 2 -> 3
    | (px:int, py:int) when px > mw / 2 && py > mh / 2 -> 4
    | _ -> 0

let postState = robots |> run 100 mapDim
let safetyScore = 
    postState 
    |> List.groupBy (quadrant mapDim)
    |> List.filter (fun (q, _) -> q <> 0)
    |> List.map (fun (_, rs) -> rs.Length)
    |> List.fold (fun s v -> s * v) 1

printfn $"%A{safetyScore}"

// Part 2

let print (robots:Robot list) ((mw, mh): int*int) =
    let map = Array2D.create mh mw " "
    robots |> List.iter (fun r -> map[r.py, r.px] <- "X")

    [0..mh - 1]
    |> List.iter (fun r -> 
        let line = map[r,*] |> String.concat ""
        printfn $"{line}"
    )

let clumpyness (robots:Robot list) =
    robots
    |> List.groupBy _.px
    |> List.map (fun (_, g) -> g.Length)
    |> List.max

let mutable i = 0
let mutable state = robots
while true do
    print state mapDim
    printfn $"{i}"
   
    Console.ReadKey true |> ignore
    
    let mutable clumpSize = 0
    while clumpSize < 30 do
        state <- update state mapDim
        i <- i + 1
        clumpSize <- state |> clumpyness

   
