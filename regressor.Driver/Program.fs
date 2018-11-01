open System
open System.IO

open Types
open Regression

let printRow (row : Row) =
    let desc = Descriptives.describe row
    let joiner = fun (xs : 't array) -> String.Join(", ", xs)
    printfn "Row %s: %i values (%O...)" row.name row.data.Length (Array.truncate 5 row.data |> joiner)
    printfn "    Mean:     %5.3f" desc.mean
    printfn "    St. Dev.: %5.3f" desc.stdev
    printfn "    Range:    %5.3f to %5.3f (%5.3f)" desc.min desc.max (Descriptives.range desc)
    printfn ""

let save (file : FileInfo) (data : string) =
    let newName = Path.ChangeExtension (file.FullName, ".html")
    File.WriteAllText (newName, data)

[<EntryPoint>]
let main argv =
    let files = argv |> 
                    Seq.filter (fun x -> x.EndsWith "csv") |> 
                    Seq.map FileInfo |> Seq.toArray

    let processFile (file : FileInfo) = 
        let rows = CsvIO.readCsv file.FullName
        printfn "-------------------------------"
        printfn "File: %s" file.FullName
        printfn "-------------------------------"
        match rows with
        | [] -> printf "no data found in file."
        | y :: xs ->
            List.iter printRow rows
            let xxs = Descriptives.enrich xs 3
            let reg = regression y xxs
            let res = calculateResults reg
            let err = errorMeasures res
            printfn "%s = %s" y.name (regressionToString reg)
            printfn "    MAPE: %5.3f%%" (err.mape * 100.0)
            printfn "    MAD : %5.3f" err.mad
            printfn "    MSE : %5.3f" err.mse
            let chart = Plots.chart reg res
            chart.GetHtml() |> save file
        printfn ""
        printfn ""

    Array.iter processFile files
    
    0 // return an integer exit code
