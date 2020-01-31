module Ant

type Coordinate =
    { X: int
      Y: int }

type Direction =
    | North
    | West
    | South
    | East

type CellColor =
    | Black
    | White

type Ant =
    { Coordinate: Coordinate
      Direction: Direction }

type Square =
    | Square of CellColor [] []
    //todo amelioration traiter le plateau comme une liste de coord de carré noir
    //todo et consider les autres commes des blancs et ainsi
    
    member this.ColorPosition coordinate =
        let (Square s) = this
        s.[coordinate.X].[coordinate.Y]

type Game =
    { Square: Square
      Ant: Ant
      IterationNumber: int }

let GenerateSquare size =
    let createLine = Array.init size (fun _ -> White)
    Array.init size (fun _ -> createLine) |> Square


let SwitchColor (coordinate: Coordinate) (square: Square): Square =
    let mapping =
        function
        | c when c = coordinate ->
            c
            |> square.ColorPosition
            |> function
                | Black -> White
                | White -> Black
        | c -> c |> square.ColorPosition

    let (Square s) = square

    s
    |> Array.mapi (fun x row ->
        row
        |> Array.mapi (fun y cell ->
            mapping
                { X = x
                  Y = y }))
    |> Square



let Push (ant: Ant) (square: Square) =
    let rotate (ant: Ant) =
        let rotateLeft =
            function
            | North -> West
            | West -> South
            | South -> East
            | East -> North

        let rotateRight =
            function
            | North -> East
            | East -> South
            | South -> West
            | West -> North

        ant
        |> (fun a -> square.ColorPosition a.Coordinate)
        |> function
        | White -> rotateLeft
        | Black -> rotateRight
        |> (fun rot -> { ant with Direction = rot ant.Direction })

    let inc a = a + 1
    let dec a = a - 1

    let push =
        function
        | { Direction = North } -> { ant with Coordinate = { ant.Coordinate with Y = inc ant.Coordinate.Y } }
        | { Direction = West } -> { ant with Coordinate = { ant.Coordinate with X = dec ant.Coordinate.X } }
        | { Direction = South } -> { ant with Coordinate = { ant.Coordinate with Y = dec ant.Coordinate.Y } }
        | { Direction = East } -> { ant with Coordinate = { ant.Coordinate with X = inc ant.Coordinate.X } }

    ant
    |> rotate
    |> push

let Play(state: Game) =
    { IterationNumber = state.IterationNumber - 1
      Ant = Push state.Ant state.Square
      Square = SwitchColor state.Ant.Coordinate state.Square }
