module App

open System
open Ant
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React


let board = GenerateBoard

type State =
    { Game: Game
      IsRunning: bool }

let initialState =
    { Game =
          { Board = board
            Ant = {
                    Direction = North
                    Coordinate = {
                        X = 0
                        Y = 0
                      }
                    }
            IterationNumber = 100
          }
      IsRunning = false }

let NewState(state: State) = { state with Game = Play state.Game }

type Message =
    | Move of State
    | Run
    | Stop
    | SetIterationNumber of int

let init() = initialState, Cmd.none

let update (msg: Message) (model: State) =
    match msg with
    | Move m -> m, Cmd.none
    | Stop -> model, Cmd.none
    | SetIterationNumber i -> { model with Game = { model.Game with IterationNumber = i } }, Cmd.none
    | Run -> { model with IsRunning = true }, Cmd.none
 
let ConvertBoardToArray2D (board : Board)  =
    let maxabs (a:Coordinate) = Math.Max( Math.Abs(a.X), Math.Abs(a.Y))
    let (Board b) = board    
    let extremePosition = b
                          |> List.map maxabs
                          |> List.fold (fun a b -> Math.Max(a, b) ) 0
    let generateSquare size (board : Board) =
        let transform idx size = idx - (size / 2)
        let createLine y = Array.init size (fun x -> board.ColorPosition {
            X = transform x size
            Y = transform y size
        })
        Array.init size createLine
    generateSquare (extremePosition * 2) board
    
let view (model: State) (dispatch: Message -> unit) =
    let tableRow xs =
        tr [] [ for x in xs -> td [] [ x ] ]

    div []
        [ div [ Class "calc" ]
              [ label [] [ str "Iterations" ]
                input
                    [ Class "input"
                      Value model.Game.IterationNumber
                      OnChange(fun ev ->
                          ev.target?value
                          |> int
                          |> SetIterationNumber
                          |> dispatch) ]
              

                button [ OnClick(fun _ -> dispatch Run) ] [ str "Run" ]

                table []
                    [ for row in model.Game.Board |> ConvertBoardToArray2D ->
                        tableRow
                            [ for n in row ->
                                div
                                    [ Style
                                        [ BackgroundColor n
                                          Width "10px"
                                          Height "10px" ] ] [ str ("") ] ] ] ]
          div
              [ Id "Chart1"
                Ref(fun element ->
                    // Ref is trigger with null once for stateless element so we need to wait for the second trigger
                    if not (isNull element) then
                        // The div has been mounted check if this is the first time
                        if (model.IsRunning && model.Game.IterationNumber > 0) then
                            model
                            |> NewState
                            |> Move
                            |> dispatch) ] [] ]



// App
Program.mkProgram init update view
|> Program.withReactBatched "elmish-app"
|> Program.withConsoleTrace
|> Program.run


//let state = initialState |> NewState |> NewState |> NewState
//ConvertBoardToArray2D state.Game.Board