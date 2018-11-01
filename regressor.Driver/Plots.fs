module Plots

open XPlot.Plotly

open Types

let chart (reg : Regression) (res : Result array) = 
    let x = reg.xs.[0] // using only first regressor to build charts
    let estimated = 
        res |> Array.map (fun r -> r.estimated)

    let actual = 
        Scatter(
            x = x.data,
            y = reg.y.data,
            mode = "markers",
            name = reg.y.name)
            
    let estimated = 
        Scatter(
            x = x.data,
            y = estimated,
            mode = "lines+markers",
            name = "Estimated " + reg.y.name)

    [estimated; actual]
        |> Chart.Plot
        |> Chart.WithWidth 700
        |> Chart.WithHeight 500