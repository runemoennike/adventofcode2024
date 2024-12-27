let myInput = """
112 1110 163902 0 7656027 83039 9 74
"""

let testInput = """
125 17
"""


let input = myInput

let parse (str:string) =
    str.Trim().Split(' ')
    |> List.ofSeq
    |> List.map int64

let halves number =
    let str = string number
    [int64 str[..str.Length/2-1]; int64 str[str.Length/2..]]

let isEvenLength number = (string number).Length % 2 = 0

let evolve (state: int64 list) =
    state
    |> List.collect(fun n ->
        match n with
        | 0L -> [1L]
        | _ when n |> isEvenLength -> n |> halves
        | _ -> [n * 2024L]
    )

let evolveN count (state: int64 list) =
    [0..count-1]
    |> List.fold(fun state' i -> evolve state') state 

let list = input |> parse
let finalLength = list |> evolveN 25 |> List.length

printfn $"%A{finalLength}"

// Part 2

let tupleHalves number =
    let str = string number
    (int64 str[..str.Length/2-1], int64 str[str.Length/2..])

let cache = System.Collections.Generic.Dictionary<int*int64, int64>() 

let countEvolutions count (value: int64) =   
    printf $"Evolving {value}..."

    let rec loop depth value =
        if depth > 0 then
            match cache.TryGetValue((depth, value)) with
            | true, cached -> cached
            | _ ->
                let r = 
                    match value with
                    | 0L -> loop (depth - 1) 1L
                    | _ when value |> isEvenLength -> 
                        let (a, b) = value |> tupleHalves
                        (loop (depth - 1) a) + (loop (depth - 1) b)
                    | _ -> loop (depth - 1) (value * 2024L)
                cache.Add((depth, value), r) |> ignore
                r
        else
            1
    loop count value

let countAllEvolutions count (state: int64 list) =
    state
    |> List.map (countEvolutions count)
    |> List.sum


let finalLength2 = list |> countAllEvolutions 75

printfn $"%A{finalLength2}"
