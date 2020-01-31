module App

open Ant
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React


let square = GenerateSquare 0

type State =
    { Game: Game
      IsRunning: bool }

let initialState =
    { Game =
          { Square = square
            Ant = {
                    Direction = North
                    Coordinate = {
                        X = 0
                        Y = 0
                      }
                    }
            IterationNumber = 0 }
      IsRunning = false }

let NewState(state: State) = { state with Game = Play state.Game }

type Message =
    | Move of State
    | Run
    | Stop
    | SetIterationNumber of int
    | SetSize of int

let init() = initialState, Cmd.none

let modifySize size game =
    { game with
          Square = GenerateSquare size
          Ant =
              { game.Ant with
                    Coordinate =
                        { X = size / 2
                          Y = size / 2 } } }
let test model size  =  { model with Game = modifySize size model.Game }

let update (msg: Message) (model: State) =
    match msg with
    | Move m -> m, Cmd.none
    | Stop -> model, Cmd.none
    | SetIterationNumber i -> { model with Game = { model.Game with IterationNumber = i } }, Cmd.none
    | SetSize i -> { model with Game = modifySize i model.Game }, Cmd.none
    | Run -> { model with IsRunning = true }, Cmd.none



let view (model: State) (dispatch: Message -> unit) =
    let tableRow xs =
        tr [] [ for x in xs -> td [] [ x ] ]

    let (Square square) = model.Game.Square
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
                label [] [ str "Size Array" ]
                input
                    [ Class "input"
                      Value square.Length
                      OnChange(fun ev ->
                          ev.target?value
                          |> int
                          |> SetSize
                          |> dispatch) ]

                button [ OnClick(fun e -> dispatch Run) ] [ str "Run" ]

                table []
                    [ for row in square ->
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
