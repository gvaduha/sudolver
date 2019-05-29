open System

type Value = Value of int | None
type Cell = (Value * int list)
//type Board = int * Cell list

let getEmptyBoard size =
    [0..(size*size-1)]
    |> List.map (fun x -> None, [1..size])

(* transform initial value list ((x,y),val) to (pos,val) *)
let initialValues size values =
    values
    |> List.map (fun x -> (fst (fst x) * size + snd (fst x), snd x))
    |> Map.ofList

(* create and initialize board *)
let createBoard board initials =
    let (_, newBoard) = board |> List.fold (fun (i, b) e -> 
                                                (i+1, (match Map.tryFind i initials with
                                                        | Some n -> (Value n, [])::b
                                                        | _ -> e::b))) (1,[])
    newBoard

(* prints board (ad-hoc)*)
let printBoard size board =
    board |> List.fold (fun i x ->  match (fst x) with
                                        | None -> printf " |"
                                        | v -> printf "%d|" ((fun (Value x) -> x) v)
                                    if i % size = 0 then printfn ""
                                    i+1) (1)

(* coordinates a values of known numbers ((x,y),val) *)
let gameInitials = [
        ((0,0),2); ((0,1),5); ((0,5),3); ((0,7),9); ((0,8),1);
        ((1,0),3); ((1,2),9); ((1,6),7); ((1,7),2);
        ((2,2),1); ((2,5),6); ((2,6),3);
        ((3,4),6); ((3,5),8); ((3,8),3);
        ((4,1),1); ((4,4),4);
        ((5,0),6); ((5,2),3); ((5,7),5);
        ((6,0),1); ((6,1),3); ((6,2),2); ((6,7),7);
        ((7,5),4); ((7,7),6);
        ((8,0),7); ((8,1),6); ((8,2),4); ((8,4),1);
        ]

[<EntryPoint>]
let main argv =
    let boardSize = 9
    let emptyBoard = getEmptyBoard boardSize
    //printf "%A" (emptyBoard)
    let board = createBoard emptyBoard (initialValues boardSize gameInitials)

    let p = printBoard boardSize
    p board
    0 // return an integer exit code
