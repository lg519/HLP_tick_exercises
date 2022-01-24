module WireHelpers

/// position on SVG canvas
type XYPos =
    {
        X : float
        Y : float
    }

/// the integer is the segment index
type SegmentError =
    | SegmentNotJoinedToPreviousError of int
    | SegmentOrientationError of int


[<Literal>]
let maxFloatArithError = 0.0000001

let isClose f1 f2 = abs (f1-f2) < maxFloatArithError
let posIsClose (p1:XYPos) (p2:XYPos) = isClose p1.X p2.X && isClose p1.Y p2.Y

let randomGen = System.Random()

let randomSegLength() = (randomGen.NextDouble() - 0.5) * 2.

/// given a list of results, return OK the list of values, if all are OK.
/// or the first Error item
let firstErrorOrOkValuesL (lst: Result<'V,'E> list) : Result<'V list,'E> =
    (Ok [],lst)
    ||> List.fold (fun okVals resItem ->
        match okVals,resItem with
        | Error err, _ -> Error err
        | Ok vals, Error firstErr -> Error firstErr
        | Ok vals, Ok x -> Ok (x :: vals))
    |> Result.map List.rev

/// given an array of results, return OK the array of values, if all are OK,
/// or the first Error item
let firstErrorOrOkValuesA (arr: Result<'V,'E> array) : Result<'V array,'E> =
    arr
    |> Array.toList
    |> firstErrorOrOkValuesL
    |> Result.map List.toArray


/// generate wire vertices for a numSegments connected segments
/// first segment must be horizontal in + x direction
/// segments must alternate horizontal and vertical
let makeWireWithRandomVerticesL numSegments =
    let start = {X=randomSegLength(); Y = randomSegLength()}
    let SecondVertex = {start with X = start.X + randomGen.NextDouble()}
    (SecondVertex, [2..numSegments])
    ||> List.scan (fun lastV segNum ->
        let isHoriz = segNum % 2 = 0
        match isHoriz with 
        | true -> {lastV with X = lastV.X + randomSegLength()}
        | false -> {lastV with Y = lastV.Y + randomSegLength()})
 
let makeWireWithRandomVerticesA = makeWireWithRandomVerticesL >> List.toArray

let printVerticesL vL =
    vL
    |> List.map (fun v -> $"(%.2f{v.X},%.2f{v.Y})")
    |> String.concat " "
    |> printfn "%s"

let printVerticesA = Array.toList >> printVerticesL

