﻿namespace FredholmSolver

open FSharp.Collections

module Primitives =

    type LinearDataType =
        | ScalarData
        | VectorData of int
        | MatrixData of int * int


    type LinearDataInfo =
        {
            start : int
            dataType : LinearDataType
        }


    /// A collection of various data (of the same type) packed into an array to be used with FORTRAN DLSODE ODE solver.
    /// The type is generic to simplify tests.
    type LinearData<'K, 'T when 'K : comparison> =
        {
            dataTypes : Map<'K, LinearDataInfo>
            data : 'T[]
            start : int
        }

        static member defaultValue : LinearData<'K, 'T> =
            {
                dataTypes = Map.empty<'K, LinearDataInfo>
                data = [||]
                start = 0
            }

        member d1.append (k : 'K, d2 : 'T) : LinearData<'K, 'T> =
            if d1.dataTypes.ContainsKey k
            then failwith $"Cannot add the same key: '{k}' to the data collection."
            else
                {
                    d1 with
                        dataTypes = d1.dataTypes |> Map.add k { start = d1.start; dataType = ScalarData }
                        data = Array.append d1.data [| d2  |]
                        start = d1.start + 1
                }

        member d1.append (k : 'K, d2 : 'T[]) : LinearData<'K, 'T> =
            if d1.dataTypes.ContainsKey k
            then failwith $"Cannot add the same key: '{k}' to the data collection."
            else
                {
                    d1 with
                        dataTypes = d1.dataTypes |> Map.add k { start = d1.start; dataType = VectorData d2.Length }
                        data = Array.append d1.data d2
                        start = d1.start + d2.Length
                }

        member d1.append (k : 'K, d2 : 'T[][]) : LinearData<'K, 'T> =
            if d1.dataTypes.ContainsKey k
            then failwith $"Cannot add the same key: '{k}' to the data collection."
            else
                let n1 = d2.Length
                let n2 = d2[0].Length

                {
                    d1 with
                        dataTypes = d1.dataTypes |> Map.add k { start = d1.start; dataType = MatrixData (n1, n2) }
                        data = Array.append d1.data (d2 |> Array.concat)
                        start = d1.start + n1 * n2
                }


    /// Rectangular representation of a matrix.
    type Matrix<'T> =
        | Matrix of 'T[][]

        member r.value = let (Matrix v) = r in v


    /// Linear representation of a matrix to use with FORTRAN DLSODE ODE solver.
    type LinearMatrix<'T> =
        {
            start : int // Beginning of the matrix data in the array.
            d1 : int // Size of the fist dimension.
            d2 : int  // Size of the second dimension.
            x : 'T[] // Array of d1 * d2 length.
        }

        member m.getValue i j  = m.x[m.start + i * m.d1 + j]

        static member create (x : 'T[][]) =
            let d1 = x.Length
            let d2 = x[0].Length

            {
                start = 0
                d1 = d1
                d2 = d2
                x = [| for a in x do for b in a do yield b |]
            }

        member m.toMatrix() = [| for i in 0..(m.d1 - 1) -> [| for j in 0..(m.d2 - 1) -> m.getValue i j |] |] |> Matrix


    type Matrix<'T>
        with
        member m.toLinearMatrix() = LinearMatrix<'T>.create m.value


    /// Representation of 2D values.
    /// It is convenient to store them as [][] rather than as [,] to leverage build in F# array manipulations.
    type XY =
        | XY of Matrix<double>

        member r.value = let (XY v) = r in v.value

        static member create v = v |> Matrix |> XY


    /// Representation of non-zero value in a sparse array.
    type SparseValue<'T> =
        {
            i : int
            value1D : 'T
        }


    [<RequireQualifiedAccess>]
    type SparseArray<'T> =
        | SparseArray of SparseValue<'T>[]

        member r.value = let (SparseArray v) = r in v

        static member create z v =
            v
            |> Array.mapi (fun i e -> if e >= z then Some { i = i; value1D = e } else None)
            |> Array.choose id
            |> SparseArray


    /// Representation of non-zero value in a sparse 2D array.
    type SparseValue2D<'T> =
        {
            i : int
            j : int
            value2D : 'T
        }

    // let inline multiply (a : 'T) (b : 'T) : 'T = a * b


    /// See: https://github.com/dotnet/fsharp/issues/3302 for (*) operator.
    [<RequireQualifiedAccess>]
    type SparseArray2D<'T when ^T: (static member ( * ) : ^T * ^T -> ^T) and ^T: (static member ( + ) : ^T * ^T -> ^T)> =
        | SparseArray2D of SparseValue2D<'T>[]

        member inline r.value = let (SparseArray2D v) = r in v

        /// Multiplies a 2D sparse array by a 2D array (Matrix) element by element.
        /// This is NOT a matrix multiplication.
        /// Returns a 2D sparse array.
        static member inline (*) (a : SparseArray2D<'T>, b : Matrix<'T>) : SparseArray2D<'T> =
            let v =
                a.value
                |> Array.map (fun e -> { e with value2D = e.value2D * b.value[e.i][e.j] })
                |> SparseArray2D

            v

        /// Multiplies a 2D sparse array by a 2D array (LinearMatrix) element by element.
        /// This is NOT a matrix multiplication.
        /// Returns a 2D sparse array.
        static member inline (*) (a : SparseArray2D<'T>, b : LinearMatrix<'T>) : SparseArray2D<'T> =
            let v =
                a.value
                |> Array.map (fun e -> { e with value2D = e.value2D * (b.getValue e.i e.j) })
                |> SparseArray2D

            v

        static member inline create z v =
            v
            |> Array.mapi (fun i e -> e |> Array.mapi (fun j v -> if v >= z then Some { i = i; j = j; value2D = v } else None))
            |> Array.concat
            |> Array.choose id
            |> SparseArray2D


    /// Representation of non-zero value in a sparse 4D array.
    type SparseValue4D<'T when ^T: (static member ( * ) : ^T * ^T -> ^T) and ^T: (static member ( + ) : ^T * ^T -> ^T)> =
        {
            i : int
            j : int
            i1 : int
            j1 : int
            value4D : 'T
        }

        /// Uses (i, j) as the first pair of indexes.
        static member inline create i j (v : SparseValue2D<'T>) =
            {
                i = i
                j = j
                i1 = v.i
                j1 = v.j
                value4D = v.value2D
            }

        /// Uses (i, j) as the second pair of indexes.
        static member inline createTransposed i j (v : SparseValue2D<'T>) =
            {
                i = v.i
                j = v.j
                i1 = i
                j1 = j
                value4D = v.value2D
            }

        static member inline createArray i j (x : SparseArray2D<'T>) = x.value |> Array.map (SparseValue4D.create i j)


    /// A 4D representation of 4D sparse tensor where the first two indexes are full ([][] is used)
    /// and the last two are in a SparseArray2D.
    type SparseArray4D<'T when ^T: (static member ( * ) : ^T * ^T -> ^T) and ^T: (static member ( + ) : ^T * ^T -> ^T)> =
        | SparseArray4D of SparseArray2D<'T>[][]

        member inline r.value = let (SparseArray4D v) = r in v

        /// Multiplies a 4D sparse array by a 2D array (LinearMatrix) using SECOND pair of indexes in 4D array.
        /// This is NOT a matrix multiplication.
        /// Returns a 4D sparse array.
        static member inline (*) (a : SparseArray4D<'T>, b : LinearMatrix<'T>) : SparseArray4D<'T> =
            let v =
                a.value
                |> Array.map (fun e -> e |> Array.map (fun x -> x * b))
                |> SparseArray4D

            v

        /// Multiplies a 4D sparse array by a 2D array (Matrix) using SECOND pair of indexes in 4D array.
        /// This is NOT a matrix multiplication.
        /// Returns a 4D sparse array.
        static member inline (*) (a : SparseArray4D<'T>, b : Matrix<'T>) : SparseArray4D<'T> =
            let v =
                a.value
                |> Array.map (fun e -> e |> Array.map (fun x -> x * b))
                |> SparseArray4D

            v

        member inline a.toSparseArray() =
            let n1 = a.value.Length
            let n2 = a.value[0].Length

            let value =
                [| for i in 0..(n1 - 1) -> [| for j in 0..(n2 - 1) -> SparseValue4D.createArray i j (a.value[i][j]) |] |]
                |> Array.concat
                |> Array.concat
                |> Array.sortBy (fun e -> e.i, e.j, e.i1, e.j1)

            value


    /// Performs a Cartesian multiplication of two 1D sparse arrays to obtain a 2D sparse array.
    let inline cartesianMultiply<'T when ^T: (static member ( * ) : ^T * ^T -> ^T) and ^T: (static member ( + ) : ^T * ^T -> ^T)> (a : SparseArray<'T>) (b : SparseArray<'T>) : SparseArray2D<'T> =
        let bValue = b.value
        a.value |> Array.map (fun e -> bValue |> Array.map (fun f -> { i = e.i; j = f.i; value2D = e.value1D * f.value1D })) |> Array.concat |> SparseArray2D<'T>.SparseArray2D


    /// Describes a function domain suitable for integral approximation.
    /// Equidistant grid is used to reduce the number of multiplications.
    type Domain =
        {
            midPoints : double[]
            step : double
            range : double
        }

        member d.noOfIntervals = d.midPoints.Length

        member d.integrateValues (v : SparseArray<double>) =
            let sum = v.value |> Array.map (fun e -> e.value1D) |> Array.sum
            sum * d.step

        static member create noOfIntervals minValue maxValue =
            let points = [| for i in 0..noOfIntervals -> minValue + (maxValue - minValue) * (double i) / (double noOfIntervals) |]

            {
                midPoints = [| for i in 0..noOfIntervals - 1 -> (points[i + 1] + points[i]) / 2.0 |]
                step =  (maxValue - minValue) / (double noOfIntervals)
                range = points[noOfIntervals] - points[0]
            }


    /// Data that describes a rectangle in ee * inf space.
    /// ee space is naturally limited to [-1, 1] unless we use a conformal transformation to extend it to [-Infinity, Infinity].
    /// inf (information) space is naturally limited at the lower bound (0). The upper bound can be rescaled to any number or even taken to Infinity.
    type Domain2D =
        {
            eeDomain : Domain
            infDomain : Domain
        }

        member d.integrateValues v =
            let sum = v |> Array.map (fun e -> e |> Array.sum) |> Array.sum
            sum * d.eeDomain.step * d.infDomain.step

        member d.integrateValues (a : SparseArray2D<double>) =
            let sum = a.value |> Array.map (fun e -> e.value2D) |> Array.sum
            sum * d.eeDomain.step * d.infDomain.step

        member d.integrateValues (a : SparseArray2D<double>, b : XY) =
            let bValue = b.value
            let sum = a.value |> Array.map (fun e -> e.value2D * bValue[e.i][e.j]) |> Array.sum
            sum * d.eeDomain.step * d.infDomain.step

        member d.integrateValues (a : SparseArray2D<double>, b : LinearMatrix<double>) =
            let bValue = b.getValue
            let sum = a.value |> Array.map (fun e -> e.value2D * (bValue e.i e.j)) |> Array.sum
            sum * d.eeDomain.step * d.infDomain.step

        member d.integrateValues (a : SparseArray4D<double>, b : LinearMatrix<double>) =
            a.value
            |> Array.map (fun v -> v |> Array.map (fun e -> d.integrateValues (e, b)))

        static member eeMinValue = -1.0
        static member eeMaxValue = 1.0
        static member infDefaultMinValue = 0.0

        static member create noOfIntervals l2 =
            {
                eeDomain = Domain.create noOfIntervals Domain2D.eeMinValue Domain2D.eeMaxValue
                infDomain = Domain.create noOfIntervals Domain2D.infDefaultMinValue l2
            }