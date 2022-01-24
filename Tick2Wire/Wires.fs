module Wires

open WireHelpers
open EEExtensions
open TimeHelpers

//----------------------------------------------------------------------------------------//
//---------------------------------------Wire types---------------------------------------//

type Segment = { Start: XYPos; End: XYPos }

/// list form wires
type WireL =
    { Segs: Segment list
      RelSegs: float list
      StartPos: XYPos }

// array form wires
type WireA =
    { Segs: Segment array
      RelSegs: float array
      StartPos: XYPos }

//----------------------------------------------------------------------------------------//
//------------------------------Wire functions - list form-------------------------------//


/// write the Segs field of WireL to make it consistent with RelSegs and StartPos fields
/// NB scan here is slightly easier to use here than fold (if you can remember it).
/// scanBack
let writeSegsFromRelSegsL (w: WireL) : WireL =
    failwithf "not implemented"

/// Return a RelSegs field of WireL consistent with Segs fields
/// Return Error <error message> or RelSegs
/// The error checking means this function is not as fast as it could be
/// NB scan here is slightly easier to use than fold (if you can remember it).
let relSegsFromSegsL (segs: Segment list) : Result<float list, SegmentError> =
    failwithf "Not implemented"

/// return either (list form) from a list of vertices, or an error if the vertices do
/// not represent a wire with manhattan routed (perpendicular) segments in the correct form.
let createWireFromVerticesL (vertices: XYPos list) : Result<WireL, SegmentError> =
    let segs =
        vertices
        |> List.pairwise
        |> List.map (fun (a, b) -> { Start = a; End = b })

    relSegsFromSegsL segs
    |> Result.map
        (fun relSegs ->
            { Segs = segs
              RelSegs = relSegs
              StartPos = vertices[ 0 ] })

//----------------------------------------------------------------------------------------//
//------------------------------Wire functions - array form ------------------------------//

/// write the Segs field of WireA to make it consistent with RSegs and StartPos fields
/// NB scan here is slightly easier to use here than fold (if you can remember it).
/// scanBack
let writeSegsFromRelSegsA (w: WireA) : WireA =
    failwithf "Not implemented"

/// Return a RelSegs field of WireL consistent with Segs fields
/// Return Error <error message> or RelSegs
/// The error checking means this function is not as fast as it could be
/// NB scan here is slightly easier to use than fold (if you can remember it).
let relSegsFromSegsA (segs: Segment array) : Result<float array, SegmentError> =
    failwithf "Not implemented"

/// return either (array form) wire from a list of vertices, or an error if the vertices do
/// not represent a wire with manhattan routed (perpendicular) segments in the correct form.
let createWireFromVerticesA (vertices: XYPos array) : Result<WireA, SegmentError> =
    let segs =
        vertices
        |> Array.pairwise
        |> Array.map (fun (a, b) -> { Start = a; End = b })

    relSegsFromSegsA segs
    |> Result.map
        (fun relSegs ->
            { Segs = segs
              RelSegs = relSegs
              StartPos = vertices[ 0 ] })

//----------------------------------------------------------------------------------------//
//------------------------------Functions to support performance testing -----------------//

/// Create a random list-type wire
let wireL n () =
    makeWireWithRandomVerticesL n
    |> createWireFromVerticesL
    |> (function
    | Ok w -> w
    | _ -> failwithf "")

/// Create a random array-type wire
let wireA n () =
    makeWireWithRandomVerticesA n
    |> createWireFromVerticesA
    |> (function
    | Ok w -> w
    | _ -> failwithf "")

/// return a new list-type wire moved by (1,1) from the input
let translateL (w: WireL) =
    { w with
          StartPos =
              { X = w.StartPos.X + 1.
                Y = w.StartPos.Y + 1. } }
    |> writeSegsFromRelSegsL

/// return a new array-type wire moved by (1,1) from the input
let translateA (w: WireA) =
    { w with
          StartPos =
              { X = w.StartPos.X + 1.
                Y = w.StartPos.Y + 1. } }
    |> writeSegsFromRelSegsA

/// read all the values in a list-type wire by summing them.
/// (the value does not matter)
let readerL (w: WireL) =
    let mutable x = 0.
    let sumXY (pos: XYPos) = pos.X + pos.Y
    let sumSeg (seg: Segment) = sumXY seg.Start + sumXY seg.End
    // this is a dummy assignmnet - made to ensure the compiler does not
    // optimise the unused reduce addition and remove it
    x <-
        List.reduce (+) w.RelSegs
        + List.reduce (+) (w.Segs |> List.map sumSeg)

    ()

/// read all the values in an array-type wire by summing them
/// (the value does not matter)
let readerA (w: WireA) =
    let mutable x = 0.
    let sumXY (pos: XYPos) = pos.X + pos.Y
    let sumSeg (seg: Segment) = sumXY seg.Start + sumXY seg.End
    // this is a dummy assignmnet - made to ensure the compiler does not
    // optimise the unused reduce addition and remove it
    x <-
        Array.reduce (+) w.RelSegs
        + Array.reduce (+) (w.Segs |> Array.map sumSeg)

    ()

/// top-level function to test the speed of the wire functions
/// n specifies the number of segments in the wires.
let testSpeed n =
    let doTest testFn =
        for n in [ 10; 10; 1000; 1000; 100000; 100000 ] do
            testFn n

        printf "\n"
    let wL = wireL n
    let wA = wireA n

    doTest <| makeTranslateWiresTest wL translateL
    doTest <| makeTranslateWiresTest wA translateA
    doTest <| makeReadWiresTest wL readerL
    doTest <| makeReadWiresTest wA readerA
