module App

open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Browser
open Browser.Types

let random = System.Random()

[<Literal>]
let ModuloN = 5

let GridRows = 5
let GridColumns = 5
let GridCells = GridRows * GridColumns

[<Struct>]
type Direction =
    | Up
    | Down
    | Left
    | Right

[<Struct>]
type UserInteraction = 
    | KeyUp
    | KeyDown
    | KeyLeft
    | KeyRight

[<Struct>]
type Square =   
    | Empty
    | Occupied of int

type GameBoard = Square array

[<Struct>]
type State =
    | Playing
    | GameOver

type Game = {
    Board : GameBoard
    GameState : State
}

type Msg =
    | Input of UserInteraction

let subscribeInput dispatch =
    let processInput (e : KeyboardEvent) =
        match e.code with
        | "ArrowUp" | "w" -> KeyUp |> Input |> dispatch
        | "ArrowDown" | "s" -> KeyDown |> Input |> dispatch
        | "ArrowLeft" | "a" -> KeyLeft |> Input |> dispatch
        | "ArrowRight" | "d" -> KeyRight |> Input |> dispatch
        | _ -> ()
    document.addEventListener("keydown", fun e -> processInput (e :?> _))


let freeLocations (board:GameBoard) : int array =
    board 
    |> Seq.mapi (fun i s -> (i, s))
    |> Seq.filter (fun (i,s) -> match s with | Empty -> true | Occupied _ -> false)
    |> Seq.map fst
    |> Array.ofSeq

[<Struct>]
type SpawnedValue = {
    Position : int
    Value : int
}

let spawnValue (board:GameBoard) : SpawnedValue =
    let freeSquares = freeLocations board
    let freeItem = random.Next(0, Array.length freeSquares)
    let position = freeSquares.[freeItem]
    let value = random.Next(0, ModuloN+3)
    {Position = position; Value = value}

// Impure! Mutates board
let addValueToBoard (board:GameBoard) (spawnedValue:SpawnedValue) =
    board.[spawnedValue.Position] <- Occupied spawnedValue.Value

let rec spawnValuesOntoBoard (board:GameBoard) (count:int) : GameBoard =
    match count with
    | 0 -> board
    | count -> 
        let spawnedValue = spawnValue board
        let newBoard = Array.copy board
        addValueToBoard newBoard spawnedValue
        spawnValuesOntoBoard newBoard (count-1)

let init () =
    let game = {
        Board = Array.init GridCells (fun _ -> Empty);
        GameState = Playing
    }
    { game with Board = spawnValuesOntoBoard game.Board 2}

let areCongruent (x:int) (y:int) (modulo:int) : bool =
    (abs (x - y) % modulo) = 0
   
let rec shiftRowHorizontal (direction:Direction) (row:Square array) : Square array =
    let newRow = 
        match direction with
        | Left -> Array.copy row
        | Right -> Array.rev row
        | _ -> 
            assert false
            [||]

    let lower = 0
    let upper = Array.length newRow - 1
    for i=lower to upper-1 do
        let x = newRow.[i]
        let y = newRow.[i+1]
        match (x, y) with
        | (Empty, Empty) -> ()
        | (Occupied u, Empty) -> ()
        | (Empty, Occupied v) -> 
            newRow.[i] <- y
            newRow.[i+1] <- Empty
        | (Occupied u, Occupied v) ->
            if areCongruent u v ModuloN then
                newRow.[i] <- Occupied (u+v)
                newRow.[i+1] <- Empty

    let newRow =
        match direction with
        | Left -> newRow
        | Right -> Array.rev newRow
        | _ -> 
            assert false
            [||]

    if newRow = row then
        newRow
    else
        shiftRowHorizontal direction newRow

let transpose (board:GameBoard) : GameBoard =
    let newBoard = Array.copy board
    for j = 0 to GridRows-1 do
        let pos = j * GridColumns
        for i=0 to GridColumns-1 do
            let transposedPos = i * GridColumns
            newBoard.[transposedPos + j] <- board.[i+pos]
    newBoard

let shiftBoardHorizontal (board:GameBoard) (direction:Direction) : GameBoard =
    let newBoard = Array.copy board
    for i in 0..(GridRows-1) do
        newBoard.[i*GridColumns..(i+1)*GridColumns-1] <- shiftRowHorizontal direction (newBoard.[i*GridColumns..(i+1)*GridColumns-1])
    newBoard

let shiftBoardVertical (board:GameBoard) (direction:Direction) : GameBoard =
    let newBoard = transpose board
    let direction = 
        match direction with
        | Up -> Left
        | Down -> Right
        | _ ->
            assert false
            Left
    transpose (shiftBoardHorizontal newBoard direction)

let isBoardFull (board:GameBoard) : bool =
    not (Array.contains Empty board)

let isGameOver (board:GameBoard) : bool =
    let mutable gameOver = isBoardFull board
    gameOver <- gameOver && ((shiftBoardVertical board Up) = board)
    gameOver <- gameOver && ((shiftBoardVertical board Down) = board)
    gameOver <- gameOver && ((shiftBoardHorizontal board Left) = board)
    gameOver <- gameOver && ((shiftBoardHorizontal board Right) = board)
    gameOver

let newGameState (gameStateCurrentState:Game) (newBoard:GameBoard) : Game =
    let newestBoard = 
        if isBoardFull newBoard then
            newBoard
        else
            spawnValuesOntoBoard newBoard 1
    if not (isGameOver newestBoard) then
        { gameStateCurrentState with Board = newestBoard}
    else 
        { Board = newestBoard; GameState = GameOver}
    

let update (msg:Msg) (gameState:Game) =
    match gameState.GameState with
    | Playing ->
        match msg with
        | Input KeyUp ->
            let newBoard = shiftBoardVertical gameState.Board Up
            newGameState gameState newBoard
        | Input KeyDown ->
            let newBoard = shiftBoardVertical gameState.Board Down
            newGameState gameState newBoard
        | Input KeyLeft ->
            let newBoard = shiftBoardHorizontal gameState.Board Left
            newGameState gameState newBoard
        | Input KeyRight ->
            let newBoard = shiftBoardHorizontal gameState.Board Right
            newGameState gameState newBoard
    | GameOver -> gameState

let squareText gameState sq =
    match gameState.Board.[sq] with
    | Empty -> str ""
    | Occupied i -> str (i.ToString())

let drawSquares gameState =
    [for i in 0..gameState.Board.Length - 1 ->
        match gameState.Board.[i] with
        | Empty -> div [ Class "grid-item" ] [squareText gameState i] 
        | Occupied _ -> div [ Class "grid-item grid-item-occupied" ] [squareText gameState i]
        ]

let drawBoard gameState = 
    div [ Class "center-grid" ]
        [ div [ Class "grid-container" ] (drawSquares gameState) ]


let view (gameState:Game) dispatch =
    match gameState.GameState with
    | Playing ->
        drawBoard gameState
    | GameOver ->
        div [Id "area"; Class "area"] 
            [ div [Class "cover"] [str "Game Over"]
              drawBoard gameState 
            ]

Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withSubscription  (fun _ -> Cmd.ofSub subscribeInput)
|> Program.run

