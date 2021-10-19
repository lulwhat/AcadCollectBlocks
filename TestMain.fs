namespace AcadCollectBlocks

open Autodesk.AutoCAD.Runtime

// Program entry point
module TestMain =
    [<CommandMethod("TestingCollect")>]
    let main () = TestModule.testing ()