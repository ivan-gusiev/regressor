module Types

open System.IO

type Row = 
    { 
        name: string; 
        data : double array 
    }

type RowDescriptives =
    {
        mean     : float;
        stdev    : float;
        skewness : float;
        kurtosis : float;
        min      : float;
        max      : float;
    }

type Regression = 
    {
        y  : Row;
        xs : Row array;
        intercept : float;
        slopes    : float array;
    }

type Result = 
    {
        estimated : float;
        actual    : float;
        error     : float;
        eSquared  : float;
    }

type ErrorMeasures = 
    {
        mape : float;
        mad  : float;
        mse  : float;
    }