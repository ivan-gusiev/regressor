module Regression

open System
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearRegression
open MathNet.Numerics.Statistics

open Types

let private elemCount (reg : Regression) = 
    reg.y.data.Length

let private onesFor (r : Row) = 
    Double.Vector.Build.Dense(r.data.Length, 1.0)

let private rowToVector (r : Row) =
    Double.Vector.Build.DenseOfArray r.data

let private rowsToMatrix (rs : Row list) =
    Double.Matrix.Build.DenseOfColumnVectors (List.map rowToVector rs)

let regression (y : Row) (xs : Row list) =
    let yvec = rowToVector y
    let xmat = (rowsToMatrix xs).InsertColumn(0, onesFor y)
    
    let ks = MultipleRegression.Svd (xmat, yvec)

    { y = y; xs = List.toArray xs; intercept = ks.[0]; slopes = Seq.toArray (Seq.skip 1 ks) }

let regressionToString (reg : Regression) = 
    let componentToString (k : float, name : string) =
        let basic = sprintf "%5.3f" (Math.Abs k) + 
                        if String.IsNullOrWhiteSpace name then "" else "*" + name
        (k, basic)
    
    let joinComponents (k1, n1) (k2, n2) =
        if k2 >= 0.0 then  
            (k1, n1 + " + " + n2)
        else 
            (k1, n1 + " - " + n2)

    let slopes = Seq.map (fun x -> x.name) reg.xs |> Seq.zip reg.slopes
    let components = Seq.concat [slopes; List.toSeq [(reg.intercept, "")]]
    let componentStrings = Seq.map componentToString components
    
    let (_, result) = (Seq.reduce joinComponents componentStrings)
    result

let calculateResults (reg : Regression) =
    let pointEstimate i =
        let xs_i = Array.map (fun arr -> arr.data.[i]) reg.xs
        reg.intercept + (Array.zip xs_i reg.slopes |> Array.map (fun (x,y) -> x * y) |> Array.reduce (+))
    
    let getResult i =
        let estimated = pointEstimate i
        let actual = reg.y.data.[i]
        let error = actual - estimated;
        let eSquared = error * error
        { estimated = estimated; actual = actual; error = error; eSquared = eSquared }

    Array.map getResult [|0..elemCount reg - 1|]

let errorMeasures (results : Result array) =
    let absolutePercentageError = Array.map (fun res -> Math.Abs(res.error)/res.actual) results
    let absoluteDeviation = Array.map (fun res -> Math.Abs(res.error)) results
    let squaredError = Array.map (fun res -> res.eSquared) results
    
    let mean xs = Array.reduce (+) xs / float results.Length

    let mape = mean absolutePercentageError
    let mad = mean absoluteDeviation
    let mse = mean squaredError
    
    { mape = mape; mad = mad; mse = mse }