module Ant

open Shortcuts

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

type Board =
    | Board of Coordinate list

    member this.ColorPosition coordinate =
        let (Board b) = this
        if (List.contains coordinate b) then Black
        else White

    member this.SwitchColor coordinate =
        let (Board b) = this
        match this.ColorPosition coordinate with
        | White -> coordinate :: b |> Board
        | Black -> remove coordinate b |> Board

type Game =
    { Board: Board
      Ant: Ant
      IterationNumber: int }

let GenerateBoard = Board []

let Push (board: Board) (ant: Ant)  =
    
    let rotate (board: Board) (ant: Ant) =
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
        |> (fun a -> board.ColorPosition a.Coordinate)
        |> function
        | White -> rotateLeft
        | Black -> rotateRight
        |> (fun rot -> { ant with Direction = rot ant.Direction })
    
    let push =
        function
        | { Direction = North } as a -> { a with Coordinate = { a.Coordinate with Y = inc a.Coordinate.Y } }
        | { Direction = West } as a -> { a with Coordinate = { a.Coordinate with X = dec a.Coordinate.X } }
        | { Direction = South } as a -> { a with Coordinate = { a.Coordinate with Y = dec a.Coordinate.Y } }
        | { Direction = East }  as a -> { a with Coordinate = { a.Coordinate with X = inc a.Coordinate.X } }

    ant
    |> rotate board
    |> push

let Play(state: Game) =
    { IterationNumber = state.IterationNumber - 1
      Ant = Push state.Board state.Ant
      Board = state.Board.SwitchColor state.Ant.Coordinate }
