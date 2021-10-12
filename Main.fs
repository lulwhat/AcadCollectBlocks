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

// Program entry point
module Main =
    [<CommandMethod("CollectBlocks")>]
    let main () = Collect.collect ()
