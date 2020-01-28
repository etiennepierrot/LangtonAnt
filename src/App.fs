module App

open Ant
open Fable.Import

open Fable.React
open Fable.React.Props
open Elmish
open Elmish
open Elmish
open Elmish.React


let steps = 60
let square = GenerateSquare steps
let ant = {
    Direction = North
    Coordinate =
        {
            X = steps / 2
            Y = steps / 2
        }
    }

let initialState = {
    Square = square
    Ant = ant
    IterationNumber = 10000
}
type Msg =
        | Continue of State
        | Stop 
    
let init() = initialState,  Cmd.ofMsg (Continue initialState)

let update (msg : Msg) (model: State) = match msg with
                                        | Continue m -> m, Cmd.ofMsg Stop
                                        | Stop -> failwithf "end of loop"
    

let view model (dispatch : Msg -> unit) =
    let tableRow xs = tr [] [ for x in xs -> td [] [x] ]
    let (Square square) = model.Square
    div
      []     
      [ div
          [ Class "calc" ]
          [ table []
                [ for row in square ->
                    tableRow [
                        for n in row ->
                            div [  Style [BackgroundColor n; Width "10px"; Height "10px" ] ] [
                                str ("") ] ]              
                    ]
          ]
        div [
              Id "Chart1"
              Ref (fun element ->
                  // Ref is trigger with null once for stateless element so we need to wait for the second trigger
                  if not (isNull element) then
                      // The div has been mounted check if this is the first time
                      if model.IterationNumber > 0 then model |> NewState |> Continue |> dispatch
                  )
        ] []
       ]
     
      
   
// App
Program.mkProgram init update view
|> Program.withReactBatched "elmish-app"
|> Program.withConsoleTrace
|> Program.run

