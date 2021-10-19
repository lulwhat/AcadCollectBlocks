namespace AcadCollectBlocks

open System.Linq
open Autodesk.AutoCAD.BoundaryRepresentation
open Autodesk.AutoCAD.DatabaseServices
open Autodesk.AutoCAD.EditorInput
open Autodesk.AutoCAD.Geometry
open Autodesk.AutoCAD.Colors
open Collect

module TestModule =
    let testing () =
        let doc =
            Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument

        let db = doc.Database
        let ed = doc.Editor

        use docLock = doc.LockDocument()

        (let pso = new PromptSelectionOptions()
         pso.MessageForAdding <- "\nPick that"
         pso.AllowDuplicates <- false
         pso.AllowSubSelections <- true
         pso.RejectObjectsFromNonCurrentSpace <- true
         pso.RejectObjectsOnLockedLayers <- false

         let psr = ed.GetSelection(pso)

         if psr.Status = PromptStatus.OK then
             let psar =
                 [| TypedValue(0, "3DSOLID") |]
                 |> SelectionFilter
                 |> ed.SelectAll

             if psar.Status = PromptStatus.OK then
                 use tr = db.TransactionManager.StartTransaction()

                 (let solids =
                     psar.Value
                     |> Seq.cast<SelectedObject>
                     |> Seq.map (fun (so: SelectedObject) -> tr.GetObject(so.ObjectId, OpenMode.ForRead) :?> Solid3d)
                     |> Seq.toList

                  let cones =
                      solids
                      |> List.filter
                          (fun sol ->
                              tr.GetObject(sol.LayerId, OpenMode.ForRead) :?> LayerTableRecord
                              |> function
                                  | ltr -> ltr.Name = "задвижка1" && isCone (sol))

                  let toruses =
                      solids
                      |> List.filter
                          (fun sol ->
                              tr.GetObject(sol.LayerId, OpenMode.ForRead) :?> LayerTableRecord
                              |> function
                                  | ltr -> ltr.Name = "задвижка1" && isTorus (sol))

                  let cylinders =
                      solids
                      |> List.filter
                          (fun sol ->
                              tr.GetObject(sol.LayerId, OpenMode.ForRead) :?> LayerTableRecord
                              |> function
                                  | ltr -> ltr.Name = "задвижка1" && isCylinder (sol))

                  let selectedSol =
                      psr.Value
                      |> Seq.cast<SelectedObject>
                      |> Seq.item (0)
                      |> function
                          | so -> tr.GetObject(so.ObjectId, OpenMode.ForRead) :?> Solid3d


                  ed.WriteMessage(
                      "Tors found: "
                      + (string <| List.length toruses)
                      + "\n"
                      + "Cyls found: "
                      + (string <| List.length cylinders)
                      + "\n"
                      + "Cones found: "
                      + (string <| List.length cones)
                      + "\n"
                  )

                  let distances = 
                      cylinders
                      |> Seq.map(fun cyl -> cyl.Bounds.Value.MaxPoint.DistanceTo(cyl.Bounds.Value.MinPoint))

                  ed.WriteMessage("min cyl height: " + string (Seq.min distances) + "\n")
                  ed.WriteMessage("max cyl height: " + string (Seq.max distances) + "\n")

                  use brep = new Brep(selectedSol)

                  (let surfaceGeometryMatch =
                      brep.Faces
                      |> Seq.map
                          (fun face ->
                              face.Surface :?> ExternalBoundedSurface
                              |> function
                                  | ebs -> ebs.IsCone)
                      |> Seq.filter ((&&) true)
                      |> Seq.length > 0

                   let circles = getCircles brep

                   ed.WriteMessage("comps: " + string(brep.Complexes.Count()) + "\n")
                   ed.WriteMessage("faces: " + string(brep.Faces.Count()) + "\n")
                   ed.WriteMessage("edges: " + string(brep.Edges.Count()) + "\n")
                   //ed.WriteMessage("circles: " + string(circles.Length))
                   //ed.WriteMessage("surfacegeometrymatch: " + string surfaceGeometryMatch)
                   //ed.WriteMessage(
                   //    "point on surface: "
                   //    + string ((ptOnSurface selectedSol).X, (ptOnSurface selectedSol).Y, (ptOnSurface selectedSol).Z)
                   //)

                  )

                  docLock.Dispose()

                 )

        )

