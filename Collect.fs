namespace AcadCollectBlocks

open System.Linq
open Autodesk.AutoCAD.BoundaryRepresentation
open Autodesk.AutoCAD.DatabaseServices
open Autodesk.AutoCAD.EditorInput
open Autodesk.AutoCAD.Geometry
open Autodesk.AutoCAD.Colors


module Collect =
    // get circle faces on solids
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
            try
                brep.Faces
                |> Seq.map
                    (fun face ->
                        face.Surface :?> ExternalBoundedSurface
                        |> function
                            | ebs -> ebs.IsCylinder)
                |> Seq.filter ((&&) true)
                |> Seq.length > 0
            with
            | _ -> false

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
            try
                brep.Faces
                |> Seq.map
                    (fun face ->
                        face.Surface :?> ExternalBoundedSurface
                        |> function
                            | ebs -> ebs.IsCone)
                |> Seq.filter ((&&) true)
                |> Seq.length > 0
            with
            | _ -> false

         let geometryRulesMatch =
             let circles = getCircles brep

             try
                 (brep.Complexes.Count() = 1 // first case for normal autocad cones
                  && brep.Faces.Count() = 2
                  && brep.Edges.Count() = 1
                  && circles.Length = 1)
                 || (brep.Complexes.Count() = 1 // second case for weird cones imported from Cyclone
                     && brep.Faces.Count() = 3
                     && brep.Edges.Count() = 2
                     && circles.Length = 2)
             with
             | _ -> false

         surfaceGeometryMatch && geometryRulesMatch)

    let isTorus (solid: Solid3d) =
        use brep = new Brep(solid)

        (let surfaceGeometryMatch =
            try
                brep.Faces
                |> Seq.map
                    (fun face ->
                        face.Surface :?> ExternalBoundedSurface
                        |> function
                            | ebs -> ebs.IsTorus)
                |> Seq.filter ((&&) true)
                |> Seq.length > 0
            with
            | _ -> false

         let geometryRulesMatch =
             try
                 brep.Complexes.Count() = 1
                 && brep.Faces.Count() = 1
             with
             | _ -> false

         surfaceGeometryMatch && geometryRulesMatch)

    let isBox (solid: Solid3d) =
        use brep = new Brep(solid)

        (try
            brep.Complexes.Count() = 1
            && brep.Faces.Count() = 6
            && brep.Edges.Count() = 12

         with
         | _ -> false)

    let ptOnSurface (solid: Solid3d) =
        use brep = new Brep(solid)

        (brep.Faces
         |> Seq.item (0)
         |> function
             | face ->
                 face
                     .GetSurfaceAsNurb()
                     .GetClosestPointTo(new Point3d(0., 0., 0.))
                     .GetPoint())

    // main function
    let collect () =
        let doc =
            Autodesk.AutoCAD.ApplicationServices.Application.DocumentManager.MdiActiveDocument

        let db = doc.Database
        let ed = doc.Editor

        use docLock = doc.LockDocument()

        (let pStrO =
            new PromptStringOptions("Введите название слоя")

         pStrO.Message <- "\nВведите название слоя: "
         pStrO.AllowSpaces <- true
         let pStrORes = ed.GetString(pStrO)
         let layerName = pStrORes.StringResult

         if pStrORes.Status = PromptStatus.OK then
             let psr =
                 [| TypedValue(0, "3DSOLID") |]
                 |> SelectionFilter
                 |> ed.SelectAll

             if psr.Status = PromptStatus.OK then
                 use tr = db.TransactionManager.StartTransaction()

                 (let docLt =
                      tr.GetObject(db.LayerTableId, OpenMode.ForWrite) :?> LayerTable

                  // check if input layer exists, exit otherwise
                  if docLt.Has(layerName) then
                      let docBtr =
                          tr.GetObject(db.CurrentSpaceId, OpenMode.ForWrite) :?> BlockTableRecord

                      let solidsLtr =
                          docLt
                          |> Seq.cast<ObjectId>
                          |> Seq.map (fun id -> tr.GetObject(id, OpenMode.ForRead) :?> LayerTableRecord)
                          |> Seq.find (fun ltr -> ltr.Name = layerName)

                      let solids =
                          psr.Value
                          |> Seq.cast<SelectedObject>
                          |> Seq.map (fun (so: SelectedObject) -> tr.GetObject(so.ObjectId, OpenMode.ForRead) :?> Solid3d)
                          |> Seq.toList
                          |> List.filter
                              (fun sol ->
                                  tr.GetObject(sol.LayerId, OpenMode.ForRead) :?> LayerTableRecord
                                  |> function
                                      | ltr -> ltr.Name = layerName)

                      let cones =
                          solids |> List.filter (fun sol -> isCone sol)

                      let toruses =
                          solids |> List.filter (fun sol -> isTorus sol)

                      let cylinders =
                          solids |> List.filter (fun sol -> isCylinder sol)

                      let boxes =
                          solids |> List.filter (fun sol -> isBox sol)


                      let solidsIndicesByDistance (solid: Solid3d) (comparedSolids: Solid3d list) =
                          List.mapi (fun i el -> i, el) comparedSolids
                          |> List.map (fun (i, el) -> i, el.Bounds.Value.MaxPoint.DistanceTo(solid.Bounds.Value.MaxPoint))
                          |> List.sortBy snd


                      let rec findValves
                          (valves: Solid3d list list)
                          (toruses: Solid3d list)
                          (cylinders: Solid3d list)
                          (cones: Solid3d list)
                          (boxes: Solid3d list)
                          =
                          match (toruses, cylinders) with
                          | [], _ | _, [] -> valves
                          | (head :: tail), _ ->
                              let valveTorCylinder =
                                  try
                                      [ head
                                        cylinders.[(solidsIndicesByDistance head cylinders).[0] |> fst] ]
                                  with
                                  | _ -> []

                              let valveCones =
                                  try
                                      ((solidsIndicesByDistance head cones).[0], (solidsIndicesByDistance head cones).[1])
                                      |> function
                                          | (i1, dist1), (i2, dist2) when (dist1 <= 1.7) && (dist2 <= 1.7) ->
                                              [ cones.[i1]; cones.[i2] ]
                                          | _ -> []
                                  with
                                  | _ -> []

                              let valveBox =
                                  try
                                      (solidsIndicesByDistance head boxes).[0]
                                      |> function
                                          | (i, dist) when (dist <= 1.7) -> [ boxes.[i] ]
                                          | _ -> []
                                  with
                                  | _ -> []

                              let valveSolids =
                                  valveTorCylinder
                                  @ valveCones
                                  @ if valveCones.Length = 0 then
                                        valveBox
                                    else
                                        []

                              findValves (valveSolids :: valves) tail cylinders cones boxes


                      let groupedValvesSolids: Solid3d list list =
                          findValves [] toruses cylinders cones boxes

                      let bt =
                          db.BlockTableId.GetObject(OpenMode.ForWrite) :?> BlockTable

                      let rec createBlock (valveNumber: int) (valveSolids: Solid3d list) =
                          bt.Has($"задвижка{valveNumber}")
                          |> function
                              | true -> createBlock (valveNumber + 1) valveSolids
                              | false ->
                                  let nbtr = new BlockTableRecord()

                                  // add blocks layer to drawing
                                  if not (docLt.Has(layerName + "_блоки_")) then
                                      let blocksLayer = new LayerTableRecord()
                                      blocksLayer.Name <- layerName + "_блоки_"
                                      blocksLayer.Color <- solidsLtr.Color
                                      docLt.Add(blocksLayer) |> ignore
                                      tr.AddNewlyCreatedDBObject(blocksLayer, true)

                                  let oIdCol = new ObjectIdCollection()

                                  valveSolids
                                  |> List.iter (fun sol -> oIdCol.Add(sol.Id) |> ignore)

                                  nbtr.Origin <- valveSolids.[0].Bounds.Value.MaxPoint

                                  nbtr.Name <- $"задвижка{valveNumber}"
                                  nbtr.BlockScaling <- BlockScaling.Uniform
                                  nbtr.Explodable <- true

                                  bt.Add(nbtr) |> ignore
                                  tr.AddNewlyCreatedDBObject(nbtr, true)

                                  let map = new IdMapping()
                                  db.DeepCloneObjects(oIdCol, nbtr.ObjectId, map, false)

                                  let coll = new ObjectIdCollection()

                                  // add entities to blockspace
                                  map
                                  |> Seq.cast<IdPair>
                                  |> Seq.iter
                                      (fun pair ->
                                          if pair.IsPrimary then
                                              let ent =
                                                  tr.GetObject(pair.Value, OpenMode.ForWrite) :?> Entity

                                              if ent <> null then
                                                  ent.Layer <- layerName + "_блоки_"
                                                  ent.ColorIndex <- 0
                                                  coll.Add(ent.ObjectId) |> ignore)

                                  nbtr.AssumeOwnershipOf(coll)

                                  // add block to drawing space
                                  let br = new BlockReference(nbtr.Origin, nbtr.Id)
                                  br.Layer <- layerName + "_блоки_"
                                  br.Color <- Color.FromColorIndex(ColorMethod.ByLayer, solidsLtr.Color.ColorIndex)

                                  docBtr.AppendEntity(br) |> ignore
                                  tr.AddNewlyCreatedDBObject(br, true)

                      if (List.length groupedValvesSolids) > 0 then
                          groupedValvesSolids
                          |> List.iter (fun group -> createBlock 0 group)

                          ed.WriteMessage(
                              $"Создано {List.length groupedValvesSolids} блоков в слое "
                              + layerName
                              + "_блоки_\n"
                          )
                      else
                          ed.WriteMessage("Не найдено ни одной задвижки\n")

                  else
                    ed.WriteMessage("Выбранный слой не найден\n")

                  tr.Commit())

             docLock.Dispose())
