module CsvIO

open System
open System.Collections.Generic
open Csv

open Types

type private RowBuilder(name : string) = 
    member val Name = name with get, set
    member val Items = new List<double>() with get

let private build (rb : RowBuilder) =
    { name = rb.Name; data = Seq.toArray rb.Items }

let private readHeader (line : ICsvLine) =
    let columnCount = line.ColumnCount
    List.map (fun (i : int) -> line.Headers.[i]) [0..columnCount-1]

let private readNumber (text : string) =
    if String.IsNullOrWhiteSpace text then 
        Double.NaN 
    else 
        Double.Parse(text, Globalization.CultureInfo.InvariantCulture)

let readCsv file : Row list = 
    let options = new CsvOptions(HeaderMode = HeaderMode.HeaderPresent)
    let lines = CsvReader.Read (file |> IO.File.OpenText, options) |> Seq.toArray
    if lines.Length = 0 then [] else

    let names = readHeader lines.[0]
    let rowBuilders = List.map RowBuilder names

    Seq.iter (fun (line : ICsvLine) ->
            List.iter (fun (rb : RowBuilder) -> 
                    line.[rb.Name] |> Convert.ToDouble |> rb.Items.Add
                ) rowBuilders
        ) lines
    List.map build rowBuilders