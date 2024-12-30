﻿let myInput = """
1098921121187650126589432104301010017898
2347810030090543237676546765218921326323
3256723345121789078545345896237635495410
0189654656230650129454210910146546786898
1018706787649843212323407893056544576781
0123215498556764501012216324567033675410
1054912389440189650983345413498122189323
2367804322339218761874214102439232075014
3494565011878307010965302101521001456985
4583876910965498123434456517617652327876
9672978894328767894525467898908543410434
8701569765419456987616321010119654306523
7616054100300345865407890121236701217810
4567123231201210870301456290547896332912
3258834998303456961210387787678987441003
4109985867214327898341295689432196556764
3457876754321016987654254776501001105895
2568965698130123216510163897567232234996
1077654147010154105425672198498143497887
2089503056923269012334789010398056786546
1123412147874678901109011001267049805430
0109892130965165210278921123452121012321
1236783021089014321367630038983430328901
0345634569870156752456541127604589437610
1267825478763247843898430934510678576523
3216910089654130956707321874321987689430
4505432198703021013210012365899654238321
3699801789012982787309898456718723148980
2789789678101276896456721032100210057671
1008650521010345785454434549321321060362
2210541430121289890365410678732639871250
4341232510537656701274320521548747898341
1056341423498545432789201230699656743432
2967650345567230101687112345788745234569
3878981236750121211096001296787230199678
4589870109889032349125410187590123288767
5679665010976541498934231095691054177678
3038754129889650587432145654782567065549
2125603236778765676501098723123478450030
3034512345654656565410181010010989321121
"""

let testInput = """
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
"""

let input = myInput

type Direction = | Up | Down | Left | Right
let directions = [Up; Down; Left; Right]

// Part 1

let toMap (str:string) = 
    str.Trim().Split(System.Environment.NewLine)
    |> Array.map(fun line -> line |> Seq.map(fun c -> int (string c)))
    |> array2D

let isInBoundsOf (map:int[,]) (x, y) = x >= 0 && x < map.GetLength(1) && y >= 0 && y < map.GetLength(0)

let findHeads (map: int[,]): (int*int) list =
    [0..map.GetLength(0) - 1]
    |> Seq.mapi(fun y _ -> y)
    |> Seq.collect(fun y ->
        [0..map.GetLength(1) - 1] 
        |> Seq.mapi(fun x _ -> x)
        |> Seq.collect(fun x ->
            if map[y,x] = 0 then [(x, y)]
            else []
        )
    ) 
    |> List.ofSeq

let directionDelta direction =
    match direction with
    | Up -> (0, -1)
    | Down -> (0, 1)
    | Left -> (-1, 0)
    | Right -> (1, 0)

let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let validStepsFrom ((x, y) as currentPos) (map:int[,]) =
    let currentVal = map[y, x]
    directions
    |> List.collect(fun dir ->
        let p = currentPos |> add (dir |> directionDelta)
        if p |> isInBoundsOf map && map[snd p, fst p] = currentVal + 1 then [p]
        else []
    )

let reachablePeaks (head: int*int) (map:int[,]) =
    let rec loop (x, y) =
        match map[y, x] with
        | 9 -> [(x, y)]
        | _ -> 
            match map |> validStepsFrom (x, y) with
            | [] -> []
            | list -> list |> List.collect(fun pos -> loop pos)
    loop head

let headScore (head: int*int) (map:int[,]) = map |> reachablePeaks head |> List.distinct |> List.length

let map =
    input
    |> toMap

let heads = 
    map |> findHeads

let score = 
    heads
    |> List.map(fun head -> map |> headScore head)
    |> List.sum

printfn $"%A{score}"

// Part 2

let headRank (head: int*int) (map:int[,]) =
    let rec loop (x, y) =
        match map[y, x] with
        | 9 -> 1
        | _ -> 
            match map |> validStepsFrom (x, y) with
            | [] -> 0
            | list -> list |> List.map(fun pos -> loop pos) |> List.sum
    loop head

let rankSum = 
    heads
    |> List.map(fun head -> map |> headRank head)
    |> List.sum

printfn $"{rankSum}"