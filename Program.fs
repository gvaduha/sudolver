(* expand list to a new list applying function to elements *)
let expand = fun f lit l ->
    l
    |> List.fold (fun acc i -> (List.map (fun x -> f i x) lit) :: acc) [] 
    |> List.rev
    |> List.concat

let expandr f len l = expand f [0..len-1] l
    
type Value = Value of int | None
type Cell = (Value * int list)
//type Board = int * Cell list

let getEmptyBoard size =
    [0..(size*size-1)]
    |> List.map (fun x -> None, set [1..size])

(* transform initial value list ((x,y),val) to (pos,val) *)
let initialValues size values =
    values
    |> List.map (fun x -> (fst (fst x) * size + snd (fst x), snd x))
    |> Map.ofList

(* create and initialize board *)
let initBoard board initials =
    let (_, newBoard) = board |> List.fold (fun (i, b) e -> 
                                                (i+1, (match Map.tryFind i initials with
                                                       | Some n -> (Value n, set [])::b
                                                       | _ -> e::b))) (0, [])
    newBoard |> List.rev

(* board raw *)
let getRow size board n =
    board |> List.skip (size*n) |> List.take size

(* board column *)
let getCol size board n =
    let (_, col) = [0..size-1]
                   |> List.fold (fun (sl, dl) e -> let e = sl |> List.skip n |> List.take 1
                                                   (sl |> List.skip size, dl @ e))
                                                   (board,[])
    col

(* board box *)
let getBox size board n =
    let h = int (sqrt (float size))
    let w = int (size / h)
    let ndx shft = 
        let rs = shft % h
        let cs = int (shft / h)
        //printfn "%A*%A+%A*%A=%A" (size*h) cs w rs (size*h*cs + w*rs)
        [0..h-1] |> List.fold (fun ndx j -> ndx @ ([0..w-1] |> List.fold (fun row i -> (j*w*h + i + size*h*cs + w*rs)::row) [] |> List.rev)) []
    (ndx n) |> List.fold (fun v i -> (board |> List.skip i |> List.head)::v) [] |> List.rev

let fromBox size board =
    let h = int (sqrt (float size))
    let w = int (size / h)
    let gendx w h =
        [0..h-1]
        |> expandr (fun i _ -> i*(w*h)*h) 1
        |> expandr (fun i x -> x*w+i) h
        |> expandr (fun i x -> x*(w*h)+i) h
        |> expandr (fun i x -> x+i) w
    let ndx = gendx w h
    ndx |> List.fold (fun acc i -> (board |> List.skip i |> List.head) :: acc) [] |> List.rev

(* prints board (ad-hoc)*)
let printBoard size board =
    let _ = board |> List.fold (fun i x ->  match (fst x) with
                                            | None -> printf " |"
                                            | Value v -> printf "%d|" v
                                            if i % size = 0 then printfn ""
                                            i+1) (1)
    printfn ""

(* in every cell removes possible numbers that already solved in group *)
let reducePossibilities group = 
    let nums = group |> List.fold (fun l x -> match fst x with
                                              | Value x -> x::l
                                              | None -> l) ([])
    group |> List.map (fun x -> let y = Set.difference (snd x) (Set.ofList nums)
                                (fst x, y))

(* reduce possible numbers in raws *)
let rawScan size board =
    [0..size-1]
    |> List.rev
    |> List.fold (fun newb i -> (reducePossibilities (getRow size board i)) @ newb) ([])

(* reduce possible numbers in columns *)
let colScan size board =
    let newcols = [0..size-1]
                  |> List.fold (fun cols i -> cols @ (reducePossibilities (getCol size board i))) ([])
    let coltr l shift size = [0..size-1] |> List.fold (fun nl x -> (l |> List.skip (x*size+shift) |> List.head) :: nl) [] |> List.rev
    [0..size-1]
    |> List.fold (fun nl s -> nl @ coltr newcols s size) []

(* reduce possible numbers in boxes *)
let boxScan size board =
    let newboxes = [0..size-1]
                   |> List.fold (fun b i -> b @ (reducePossibilities (getBox size board i))) ([])
    newboxes
    |> fromBox size

(* set numbers in cells if there is only one possibility *)
let reduceSolved board =
    board |> List.fold (fun b x -> match x with
                                     | (None, v) -> match List.ofSeq v with
                                                    | h::[] -> (Value h, Set[])::b
                                                    | _ -> x::b
                                     | _ -> x::b) []
    |> List.rev

(* check if there are any empty (None) cells *)
let isIncompleted board =
    let res = List.tryFind (fun x -> if fst x = None then true else false) board
    match res with
    | Some _ -> true
    | _ -> false

(* solver loop *)
let rec simpleREPL size board =
    let b = board |> rawScan size |>  colScan size |> boxScan size |> reduceSolved
    printBoard size b

    match (isIncompleted b) with
    | false -> b
    | true -> simpleREPL size b

(**)
let parallelREPL =
    ()

(**)
let simpleBoard = [
        ((0,0),4); ((0,1),1); ((0,4),6); ((0,7),7);
        ((1,2),3); ((1,4),8); ((1,5),5); ((1,8),9);
        ((2,1),2); ((2,3),3); ((2,4),7); ((2,6),5); ((2,8),1);
        ((3,1),3); ((3,3),6); ((3,5),9); ((3,6),2); ((3,7),5);
        ((4,0),6); ((4,3),5); ((4,5),1);
        ((5,2),9); ((5,4),2); ((5,8),3);
        ((6,2),6); ((6,3),2); ((6,6),7); ((6,7),4); ((6,8),5);
        ((7,3),4); ((7,5),6); ((7,6),8);
        ((8,0),2); ((8,1),8); ((8,2),4); ((8,6),1); ((8,7),9); ((8,8),6);
        ]

(**)
let expertBoard = [
        ((0,1),1); ((0,2),7); ((0,4),5); ((0,6),9);
        ((1,8),1);
        ((2,0),2); ((2,6),4);
        ((3,4),3); ((3,5),1); ((3,6),5); ((3,7),6);
        ((4,0),9); ((4,3),6); ((4,7),4);
        ((5,2),8); ((5,7),9);
        ((6,1),4); ((6,2),6); ((6,4),8);
        ((7,2),3); ((7,3),4)
        ((8,1),8); ((8,5),6);
        ]

(* coordinates a values of known numbers ((x,y),val) *)
let gameInitials = simpleBoard
//let gameInitials = expertBoard


[<EntryPoint>]
let main _ =
    let boardSize = 9
    let emptyBoard = getEmptyBoard boardSize
    let board = initBoard emptyBoard (initialValues boardSize gameInitials)
    printBoard boardSize board

    //let y = rawScan boardSize board
    //let y = colScan boardSize board
    //let y = boxScan boardSize board
    //printBoard boardSize y
    //printfn "%A" (y)
    let y = simpleREPL boardSize board
    //printfn "%A" (isIncompleted boardSize board)

    0