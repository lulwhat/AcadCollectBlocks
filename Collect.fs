namespace AcadCollectBlocks

open System
open System.Linq
open System.Collections.Generic
open System.Collections
open System.Threading
open System.ComponentModel
open System.Linq.Expressions
open Autodesk.AutoCAD.ApplicationServices
open Autodesk.AutoCAD.BoundaryRepresentation
open Autodesk.AutoCAD.DatabaseServices
open Autodesk.AutoCAD.EditorInput
open Autodesk.AutoCAD.Geometry
open Autodesk.AutoCAD.Runtime
open Autodesk.AutoCAD.Colors


module Collect =
    let doc =
        Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument

    let db = doc.Database
    let ed = doc.Editor

    let getCircles (brep: Brep) =
        try
            brep
                .Edges
                .Select(fun edge ->
                    edge.Curve :?> ExternalCurve3d
                    |> function
                        | ec3d -> ec3d.NativeCurve :?> CircularArc3d)
                .Where(fun circle -> circle <> null)
                .ToArray()
        with
        | _ -> [||]

    let isCylinder (solid: Solid3d) =
        use brep = new Brep(solid)

        (let surfaceGeometryMatch =
            brep.Faces
            |> Seq.map
                (fun face ->
                    face.Surface :?> ExternalBoundedSurface
                    |> function
                        | ebs -> ebs.IsCylinder)
            |> Seq.filter ((&&) true)
            |> Seq.length > 0

         let geometryRulesMatch =
             let circles = getCircles brep

             try
                 brep.Complexes.Count() = 1
                 && brep.Faces.Count() = 3
                 && brep.Edges.Count() = 2
                 && circles.Length = 2
                 && circles.[0].Radius = circles.[1].Radius
                 && circles.[0]
                     .Normal.IsParallelTo(circles.[1].Normal)
             with
             | _ -> false

         surfaceGeometryMatch && geometryRulesMatch)

    let isCone (solid: Solid3d) =
        use brep = new Brep(solid)

        (let surfaceGeometryMatch =
            brep.Faces
            |> Seq.map
                (fun face ->
                    face.Surface :?> ExternalBoundedSurface
                    |> function
                        | ebs -> ebs.IsCone)
            |> Seq.filter ((&&) true)
            |> Seq.length > 0

         let geometryRulesMatch =
             let circles = getCircles brep

             try
                 brep.Complexes.Count() = 1
                 && brep.Faces.Count() = 2
                 && brep.Edges.Count() = 1
                 && circles.Length = 1
             with
             | _ -> false

         surfaceGeometryMatch && geometryRulesMatch)

    let isTorus (solid: Solid3d) =
        use brep = new Brep(solid)

        (let surfaceGeometryMatch =
            brep.Faces
            |> Seq.map
                (fun face ->
                    face.Surface :?> ExternalBoundedSurface
                    |> function
                        | ebs -> ebs.IsTorus)
            |> Seq.filter ((&&) true)
            |> Seq.length > 0

         let geometryRulesMatch =
             try
                 brep.Complexes.Count() = 1
                 && brep.Faces.Count() = 1
             with
             | _ -> false

         surfaceGeometryMatch && geometryRulesMatch)

    let collect () =
        let psr =
            [| TypedValue(0, "3DSOLID") |]
            |> SelectionFilter
            |> ed.SelectAll

        //let pso = new PromptSelectionOptions()
        //pso.MessageForAdding <- "\nClick that thing"
        //pso.AllowDuplicates <- false
        //pso.AllowSubSelections <- true
        //pso.RejectObjectsFromNonCurrentSpace <- true
        //pso.RejectObjectsOnLockedLayers <- true
        //let psr = ed.GetSelection(pso)

        if psr.Status = PromptStatus.OK then
            use tr = db.TransactionManager.StartTransaction()

            (let solids =
                psr.Value
                |> Seq.cast<SelectedObject>
                |> Seq.map (fun (so: SelectedObject) -> tr.GetObject(so.ObjectId, OpenMode.ForRead) :?> Solid3d)


             ed.WriteMessage(
                 "Cones N: "
                 + string (solids |> Seq.filter (isCone) |> Seq.length)
                 + "\n"
             )

             ed.WriteMessage(
                 "Toruses N: "
                 + string (solids |> Seq.filter (isTorus) |> Seq.length)
                 + "\n"
             )

             ed.WriteMessage(
                 "Cylinders N: "
                 + string (solids |> Seq.filter (isCylinder) |> Seq.length)
                 + "\n"
             )

            // tr.Commit()
            )
