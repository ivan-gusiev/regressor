module Descriptives

open System
open MathNet.Numerics.Statistics;

open Types

let describe (row : Row) =
    let stats = DescriptiveStatistics row.data
    { 
        mean = stats.Mean;
        stdev = stats.StandardDeviation;
        skewness = stats.Skewness;
        kurtosis = stats.Kurtosis;
        min = stats.Minimum;
        max = stats.Maximum;
    }

let enrich (rows : Row list) (powers : int) =
    let rowPower row i =
        let newName = sprintf "%s^%i" row.name i
        { name = newName; data = Array.map (fun x -> Math.Pow(x, float i)) row.data }
    let enrichRow row =
        row :: List.map (rowPower row) [2..powers]
    List.map enrichRow rows |> List.concat

let range (stats : RowDescriptives) = stats.max - stats.min