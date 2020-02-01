module Tests
open Expecto
open Ant

let origin = { X = 0; Y = 0; }
let board = Board []
let initialAnt = {
        Coordinate = origin
        Direction = North
      }

[<Tests>]
let tests =
  testList "A test group"[
    test "Push Ant" {      
      let antPushed = Push board initialAnt
      Expect.equal antPushed  {
        Coordinate = { X  = -1; Y = 0  }
        Direction = West
      } "Success"
    }] |> testLabel "Ant Testing"

[<EntryPoint>]
let main args =
  runTestsInAssemblyWithCLIArgs [] args