﻿﻿// Learn more about F# at http://fsharp.org

open System

let generatePythagorean m n = [pown m 2 - pown n 2; 2*m*n; pown m 2 + pown n 2]

let rec sum list =
    match list with
    | head :: tail -> abs head + sum tail
    | [] -> 0

let rec product list =
    match list with
    | head :: tail -> abs head * product tail
    | [] -> 1

let loop() =
    for i = 1 to 50 do
        for j = 1 to 50 do
            let leList = generatePythagorean i j
            let sumList = sum leList
            let productList = product leList
            if sumList = 1000 then printfn "%d" productList
    printfn ""

[<EntryPoint>]

let main argv =
    printfn "Hello World from F#!"
    loop()
    0 // return an integer exit code
