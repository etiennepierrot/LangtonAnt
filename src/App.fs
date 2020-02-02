module App

open Ant
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Fulma


let board = GenerateBoard

type State =
    { Game: Game
      IsRunning: bool }

let initialState =
    { Game =
          { Board = board
            Ant =
                { Direction = North
                  Coordinate =
                      { X = 0
                        Y = 0 } }
            IterationNumber = 100 }
      IsRunning = false }

let NewState(state: State) = { state with Game = Play state.Game }

let RunSimulation (state: State) (iteration: int) =
    let game = PlayRec state.Game iteration
    { state with Game = game }

type Message =
    | Move of State
    | Run
    | Stop
    | SetIterationNumber of int
    | RunWithoutAnimation

let init() = initialState, Cmd.none

let update (msg: Message) (model: State) =
    match msg with
    | Move m -> m, Cmd.none
    | Stop -> { model with IsRunning = false }, Cmd.none
    | SetIterationNumber i -> { model with Game = { model.Game with IterationNumber = i } }, Cmd.none
    | Run -> { model with IsRunning = true }, Cmd.none
    | RunWithoutAnimation -> (RunSimulation model model.Game.IterationNumber), Cmd.none


let view (model: State) (dispatch: Message -> unit) =
    
    let game = model.Game
    let array2D = game.Board.ConvertBoardToArray2D
    let sizeArray =  (array2D.Length * 10).ToString() + "px"
    let tableRow xs = tr [] [ for x in xs -> td [ Class "is-paddingless" ][x] ]
    
    div []
        [
          Content.content []
              [
                div [
                        Ref(fun element ->
                        // Ref is trigger with null once for stateless element so we need to wait for the second trigger
                        if not (isNull element) then
                            // The div has been mounted check if this is the first time
                            if (model.IsRunning && model.Game.IterationNumber > 0) then
                                model
                                |> NewState
                                |> Move
                                |> dispatch
                             else if( model.IsRunning && model.Game.IterationNumber = 0)
                                then dispatch Stop
                         )                               
                        ]
                    [
                      Field.div []
                          [ Label.label [] [ str "Iterations :" ]
                            Control.div [] [ Input.text [
                                                          Input.Placeholder "Ex: 100"
                                                          Input.Value (game.IterationNumber |> string )
                                                          Input.OnChange (fun ev -> ev.target?value
                                                                                    |> int
                                                                                    |> SetIterationNumber
                                                                                    |> dispatch)
                                                          ] ] ]
                     
                      Button.button
                          [
                            Button.OnClick (fun _ -> dispatch Run)
                          ] [str "Run"]
                      Button.button
                          [ Button.Color IsSuccess
                            Button.OnClick (fun _ -> dispatch RunWithoutAnimation)  
                          ] [str "Run without animation"]
                      Button.button
                          [ Button.Color IsDanger
                            Button.OnClick (fun _ -> dispatch Stop)  
                          ] [str "Stop"]
                      
                    ]
                    
                table[
                    Class "table"    
                    Style [
                            Width sizeArray
                            Height sizeArray]]
                    [tbody [] [ for row in game.Board.ConvertBoardToArray2D ->
                                tableRow [ for color in row ->
                                            div [Style
                                                    [
                                                      BackgroundColor color
                                                      Width "10px"
                                                      Height "10px"
                                                      ]] [str ""] ] ]
                      ]
              ]
                    
                     
        ]
        
Program.mkProgram init update view
|> Program.withReactBatched "elmish-app"
|> Program.withConsoleTrace
|> Program.run
