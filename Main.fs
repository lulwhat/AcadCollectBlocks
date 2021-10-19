namespace AcadCollectBlocks

open Autodesk.AutoCAD.Runtime

// Program entry point
module Main =
    [<CommandMethod("CollectBlocks")>]
    let main () = Collect.collect ()
