module TimeHelpers

//-----------------Code to record and print execution time statistics-------//

/// returns absolute time in ms, works under both .Net and Fable
let getTimeMs() = 
#if FABLE_COMPILER
    Fable.Core.JS.Constructors.Date.now()
#else
    let start = System.DateTime.UtcNow;           
    float start.Ticks / float 10000
#endif

/// returns elapsed time from startTime
let getInterval (startTime:float) =
    getTimeMs() - startTime

//------------------Code to measure time of wire read and update------------//

/// record controlling the operation run in a performance test
type TestParams<'WIRET> = {
    Name: string
    AddToMap: ('WIRET -> 'WIRET) option // transform each wire to a different one, output the wire in the map of wires
    IterOverMap: ('WIRET -> unit) option // read the each wire, throw away the result
    WireGen: unit -> 'WIRET // create a randomly generated wire
    LimitMs: float // how long to run the test for (multiple times) to determine average time
}

/// Create initial parameters for a test
/// Either AddToMap or InterOverMap must be 
let makeInitTestPars wireGen =
    {
        Name="Initial (non-working)"; 
        AddToMap = None
        IterOverMap = None
        WireGen = wireGen
        LimitMs = 200.
    }  

/// Return time taken by thunk()
/// Run thunk() as many times as is needed
/// for total elapsed time in ms to be  > limitMs.
/// Return average time of all runs.
/// To minimise cache effects run thunk() once before
/// starting to time.
let getTimeOfInMs (limitMs: float) (thunk:Unit -> Unit) =
    thunk()
    let startT = getTimeMs()
    let mutable i = 0
    while getInterval startT < limitMs do
        i <- i+1
        thunk()
    getInterval startT / float i

/// print the time of the operation defined by testPars when run on a Map containing mapSize wires.
let timeWireFn (mapSize: int) (testPars: TestParams<'WIRET>) =
    // create a Map of wires, map wireFn over it to generate a new map
    let mutable wMap = 
        Array.init mapSize (fun _ -> testPars.WireGen())
        |> Array.indexed
        |> Map.ofArray

    let thunk() =
        match testPars.AddToMap,testPars.IterOverMap with
        | Some adder,None -> 
            (wMap,wMap)
            ||> Map.fold (fun wm k w ->
                    Map.add k (adder w) wm)
            |> (fun wm -> wMap <- wm)
        | None, Some summer ->
            Map.iter (fun _ v -> summer v) wMap
        | _ -> failwithf "Bad parameters: Exactly one of AddToMap and IterOverMap must be None"
    printfn $"{testPars.Name} = %.2f{1000.0 * getTimeOfInMs testPars.LimitMs thunk / float mapSize}us/wire"


/// run a test that translates wires (writing them back into a modified map)
/// wireGen - genrate the wires
/// translater: create changed wire from old one.
let makeTranslateWiresTest  (wireGen: unit -> 'WIRET) (translater: 'WIRET -> 'WIRET) (mapSize:int) =
    let  tt = typeof<'WIRET>.FullName
    makeInitTestPars wireGen
    |> (fun pars -> {
        pars with 
            AddToMap = Some translater
            Name = $"Translate %6d{mapSize} {tt}"
        })
    |> timeWireFn mapSize

/// run a test that reads wires
/// wireGen - generate the wires
/// reader: read the wires (typically read all of its coordinates)
let makeReadWiresTest  (wireGen: unit -> 'WIRET) (reader: 'WIRET -> unit) (mapSize:int) =
    let  tt = typeof<'WIRET>.FullName
    makeInitTestPars wireGen
    |> (fun pars -> {
        pars with 
            IterOverMap = Some reader
            Name = $"Read %6d{mapSize} {tt}"
        })
    |> timeWireFn mapSize