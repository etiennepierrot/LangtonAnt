module Ant

open Shortcuts
open System

type Coordinate =
    { X: int
      Y: int }

type Direction =
    | North
    | West
    | South
    | East

type ColorFloor =
    | Black
    | White

type ColorDisplay = ColorFloor of ColorFloor | Red

type Ant =
    { Coordinate: Coordinate
      Direction: Direction }

type ExtremePosition = {
            MinX : int
            MinY : int
            MaxY : int
            MaxX : int
        }


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


       
let GenerateGrid (game : Game) = 
        let extremePosition (b : Coordinate list) = {
                                            MinX = b |> List.fold (fun acc c -> Math.Min(acc, c.X)) 0
                                            MaxX = b |> List.fold (fun acc c -> Math.Max(acc, c.X)) 0
                                            MinY = b |> List.fold (fun acc c -> Math.Min(acc, c.Y)) 0
                                            MaxY = b |> List.fold (fun acc c -> Math.Max(acc, c.Y)) 0
                                        }
        let generateSquare (extremePosition : ExtremePosition) (g : Game) =
            let transform idx size = idx - (size / 2)
            let sizeX = extremePosition.MaxX - extremePosition.MinX
            let sizeY = extremePosition.MaxY - extremePosition.MinY
            
            let colorDisplay game coord  = 
                match coord with
                | c when c = game.Ant.Coordinate -> Red
                | _  -> game.Board.ColorPosition coord |> ColorDisplay.ColorFloor

            let createLine y = Array.init sizeX (fun x -> colorDisplay g {
                X = transform x sizeX
                Y = transform y sizeY
            })
            Array.init sizeY createLine
        let (Board b) = game.Board
        let minmaxGrid = extremePosition (game.Ant.Coordinate :: b)
        generateSquare minmaxGrid game



let GenerateBoard = Board []

let Push (board: Board) (ant: Ant)  =
    
    let rotate (board: Board) (ant: Ant) =
        let turn ant rot = { ant with Direction = rot ant.Direction }
        ant.Coordinate
        |> board.ColorPosition
        |> function
            | White -> function
                        | North -> West
                        | West -> South
                        | South -> East
                        | East -> North
            | Black -> function
                        | North -> East
                        | East -> South
                        | South -> West
                        | West -> North
        |> turn ant
    
    let moveForward =
        function
        | { Direction = North } as a -> { a with Coordinate = { a.Coordinate with Y = inc a.Coordinate.Y } }
        | { Direction = West } as a -> { a with Coordinate = { a.Coordinate with X = dec a.Coordinate.X } }
        | { Direction = South } as a -> { a with Coordinate = { a.Coordinate with Y = dec a.Coordinate.Y } }
        | { Direction = East }  as a -> { a with Coordinate = { a.Coordinate with X = inc a.Coordinate.X } }

    ant
    |> rotate board
    |> moveForward

let Play(game: Game) =
    { IterationNumber = game.IterationNumber - 1
      Ant = Push game.Board game.Ant
      Board = game.Board.SwitchColor game.Ant.Coordinate }
    
let rec PlayRec (game : Game ) (iteration : int) =
    if(iteration = 0 )
    then
        game
    else
        PlayRec (Play game) (dec iteration)
    
