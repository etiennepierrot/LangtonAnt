module Ant

type Coordinate = {
    X : int
    Y : int
}

type Direction = North | West | South | East
type Color = Black | White
type Ant = {
    Coordinate : Coordinate
    Direction : Direction
}
type Square = Square of Color[][]

type State =
    {
        Square : Square
        Ant : Ant
        IterationNumber : int
    }

let GenerateSquare size =
    let createLine = Array.init size (fun  _ -> White )
    Array.init size (fun _ -> createLine ) |> Square
    
let ColorPosition (coordinate: Coordinate) (Square square) = square.[coordinate.X].[coordinate.Y]
    
let SwitchColor (coordinate : Coordinate) (square : Square) : Square =
    let inverseColor = function
                      | Black -> White
                      | White -> Black                     
    let mapping = function
                 | c when c = coordinate -> (ColorPosition c square) |> inverseColor
                 | c -> ColorPosition c square
    let (Square s) = square
    
    s
    |> Array.mapi (fun x row -> row |>  Array.mapi(fun y cell -> mapping { X = x; Y = y;}) )
    |> Square
    

   
let Push (ant : Ant) (square : Square) =
    let Rotate (ant : Ant) (square : Square) : Ant =
        let RotateLeft (ant : Ant) = match ant with
                                         | {Coordinate = _ ; Direction = North} -> {ant with Direction = West}
                                         | {Coordinate = _ ; Direction = West}  -> {ant with Direction = South}
                                         | {Coordinate = _ ; Direction = South} -> {ant with Direction = East}
                                         | {Coordinate = _ ; Direction = East}  -> {ant with Direction = North}
                             
        let RotateRight (ant : Ant) = match ant with
                                         | {Coordinate = _ ; Direction = North} -> {ant with Direction = East}
                                         | {Coordinate = _ ; Direction = East}  -> {ant with Direction = South}
                                         | {Coordinate = _ ; Direction = South} -> {ant with Direction = West}
                                         | {Coordinate = _ ; Direction = West}  -> {ant with Direction = North}
        match ColorPosition ant.Coordinate square with
        | White -> RotateLeft ant
        | Black -> RotateRight ant
        
    let Move (ant : Ant) = match ant with
                            | {Coordinate = _; Direction = North} ->
                                { ant with Coordinate = { ant.Coordinate with Y = ant.Coordinate.Y + 1 } }
                            | {Coordinate = _; Direction = West} ->
                                { ant with Coordinate = { ant.Coordinate with X = ant.Coordinate.X - 1 } }
                            | {Coordinate = _; Direction = South} ->
                                { ant with Coordinate = { ant.Coordinate with Y = ant.Coordinate.Y - 1 } }
                            | {Coordinate = _; Direction = East}  ->
                                { ant with Coordinate = { ant.Coordinate with X = ant.Coordinate.X + 1 } }
    Rotate ant square |> Move


let NewState (state : State) =
    {
        IterationNumber = state.IterationNumber - 1
        Ant = Push state.Ant state.Square
        Square = SwitchColor state.Ant.Coordinate state.Square
    }
    
let rec PlayRec (state : State) =
    if (state.IterationNumber = 0)
        then state
        else state |> NewState |> PlayRec



    