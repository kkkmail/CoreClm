namespace FredholmSolver

open System
open FSharp.Collections

module Primitives =

    type Vector<'T when ^T: (static member ( * ) : ^T * ^T -> ^T) and ^T: (static member ( + ) : ^T * ^T -> ^T) and ^T: (static member ( - ) : ^T * ^T -> ^T)> =
        | Vector of 'T[]

        member inline r.value = let (Vector v) = r in v

        static member inline (*) (a : 'T, b : Vector<'T>) : Vector<'T> =
            let retVal = b.value |> Array.map (fun e -> a * e) |> Vector
            retVal

        static member inline (*) (a : Vector<'T>, b : 'T) : Vector<'T> =
            let retVal = a.value |> Array.map (fun e -> e * b) |> Vector
            retVal

        static member inline (+) (a : Vector<'T>, b : Vector<'T>) : Vector<'T> =
            let retVal = a.value |> Array.mapi (fun i e -> e + b.value[i]) |> Vector
            retVal

        static member inline (-) (a : Vector<'T>, b : Vector<'T>) : Vector<'T> =
            let retVal = a.value |> Array.mapi (fun i e -> e - b.value[i]) |> Vector
            retVal


    /// Rectangular representation of a matrix.
    type Matrix<'T when ^T: (static member ( * ) : ^T * ^T -> ^T) and ^T: (static member ( + ) : ^T * ^T -> ^T) and ^T: (static member ( - ) : ^T * ^T -> ^T)> =
        | Matrix of 'T[][]

        member inline r.value = let (Matrix v) = r in v

        // /// Matrix multiplication (not implemented yet as it is not needed).
        // static member inline ( ** ) (a : Matrix<'T>, b : Matrix<'T>) : Matrix<'T> =
        //     failwith "Matrix multiplication is not implemented yet."

        static member inline (*) (a : 'T, b : Matrix<'T>) : Matrix<'T> =
            let retVal = b.value |> Array.map (fun e -> e |> Array.map (fun v -> a * v)) |> Matrix
            retVal

        static member inline (*) (a : Matrix<'T>, b : 'T) : Matrix<'T> =
            let retVal = a.value |> Array.map (fun e -> e |> Array.map (fun v -> v * b)) |> Matrix
            retVal

        /// This is NOT a matrix multiplication but element by element multiplication.
        /// The index of a Vector matches the FIRST index of a Matrix.
        static member inline (*) (a : Vector<'T>, b : Matrix<'T>) : Matrix<'T> =
            let retVal = b.value |> Array.mapi (fun i e -> e |> Array.map (fun v -> a.value[i] * v)) |> Matrix
            retVal

        /// This is NOT a matrix multiplication but element by element multiplication.
        /// The index of a Vector matches the SECOND index of a Matrix.
        static member inline (*) (a : Matrix<'T>, b : Vector<'T>) : Matrix<'T> =
            let retVal = a.value |> Array.map (fun e -> e |> Array.mapi (fun j v -> v * b.value[j])) |> Matrix
            retVal

        /// This is NOT a matrix multiplication but element by element multiplication.
        static member inline (*) (a : Matrix<'T>, b : Matrix<'T>) : Matrix<'T> =
            let retVal = a.value |> Array.mapi (fun i e -> e |> Array.mapi (fun j v -> v * b.value[i][j])) |> Matrix
            retVal

        static member inline (+) (a : Matrix<'T>, b : Matrix<'T>) : Matrix<'T> =
            let retVal = a.value |> Array.mapi (fun i e -> e |> Array.mapi (fun j v -> v + b.value[i][j])) |> Matrix
            retVal

        static member inline (-) (a : Matrix<'T>, b : Matrix<'T>) : Matrix<'T> =
            let retVal = a.value |> Array.mapi (fun i e -> e |> Array.mapi (fun j v -> v - b.value[i][j])) |> Matrix
            retVal


    /// Linear representation of a matrix to use with FORTRAN DLSODE ODE solver.
    type LinearMatrix<'T when ^T: (static member ( * ) : ^T * ^T -> ^T) and ^T: (static member ( + ) : ^T * ^T -> ^T) and ^T: (static member ( - ) : ^T * ^T -> ^T)> =
        {
            start : int // Beginning of the matrix data in the array.
            d1 : int // Size of the fist dimension.
            d2 : int  // Size of the second dimension.
            x : 'T[] // Array of at least (start + d1 * d2) length.
        }

        member inline m.getValue i j  = m.x[m.start + i * m.d1 + j]

        static member inline create (x : 'T[][]) =
            let d1 = x.Length
            let d2 = x[0].Length

            {
                start = 0
                d1 = d1
                d2 = d2
                x = [| for a in x do for b in a do yield b |]
            }

        member inline m.d1Range = [| for i in 0..(m.d1 - 1) -> i |]
        member inline m.d2Range = [| for i in 0..(m.d2 - 1) -> i |]

        member inline m.toMatrix() = m.d1Range |> Array.map (fun i -> m.d2Range |> Array.map (fun j -> m.getValue i j )) |> Matrix

        /// This is NOT a matrix multiplication but element by element multiplication.
        /// The index of a Vector matches the FIRST index of a Matrix.
        static member inline (*) (a : Vector<'T>, b : LinearMatrix<'T>) : Matrix<'T> =
            let retVal = b.d1Range |> Array.map (fun i -> b.d2Range |> Array.map (fun j -> a.value[i] * (b.getValue i j))) |> Matrix
            retVal

        /// This is NOT a matrix multiplication but element by element multiplication.
        /// The index of a Vector matches the SECOND index of a Matrix.
        static member inline (*) (a : LinearMatrix<'T>, b : Vector<'T>) : Matrix<'T> =
            let retVal = a.d1Range |> Array.map (fun i -> a.d2Range |> Array.map (fun j -> (a.getValue i j) * b.value[j])) |> Matrix
            retVal

        /// This is NOT a matrix multiplication but element by element multiplication.
        static member inline (*) (a : Matrix<'T>, b : LinearMatrix<'T>) : Matrix<'T> =
            let retVal = a.value |> Array.mapi (fun i e -> e |> Array.mapi (fun j v -> v * (b.getValue i j))) |> Matrix
            retVal

        /// This is NOT a matrix multiplication but element by element multiplication.
        static member inline (*) (a : LinearMatrix<'T>, b : Matrix<'T>) : Matrix<'T> =
            let retVal = b.value |> Array.mapi (fun i e -> e |> Array.mapi (fun j v -> (a.getValue i j) * v)) |> Matrix
            retVal


    type Matrix<'T when ^T: (static member ( * ) : ^T * ^T -> ^T) and ^T: (static member ( + ) : ^T * ^T -> ^T) and ^T: (static member ( - ) : ^T * ^T -> ^T)>
        with
        member inline m.toLinearMatrix() = LinearMatrix<'T>.create m.value


    type LinearDataType =
        | ScalarData
        | VectorData of int
        | MatrixData of int * int


    type LinearDataElement =
        {
            start : int
            dataType : LinearDataType
        }

    type LinearDataInfo<'K when 'K : comparison> =
        {
            dataTypes : Map<'K, LinearDataElement>
            start : int
        }

        static member defaultValue : LinearDataInfo<'K> =
            {
                dataTypes = Map.empty<'K, LinearDataElement>
                start = 0
            }


    /// A collection of various data (of the same type) packed into an array to be used with FORTRAN DLSODE ODE solver.
    /// The type is generic to simplify tests so that integers can be used for exact comparison.
    /// Otherwise just double would do fine.
    type LinearData<'K, 'T when 'K : comparison and  ^T: (static member ( * ) : ^T * ^T -> ^T) and ^T: (static member ( + ) : ^T * ^T -> ^T) and ^T: (static member ( - ) : ^T * ^T -> ^T)> =
        {
            dataInfo : LinearDataInfo<'K>
            data : 'T[]
        }

        static member inline defaultValue : LinearData<'K, 'T> =
            {
                dataInfo = LinearDataInfo<'K>.defaultValue
                data = [||]
            }

        member inline d.Item
            with get i = d.dataInfo.dataTypes[i]

        member inline d1.append (k : 'K, d2 : 'T) : LinearData<'K, 'T> =
            if d1.dataInfo.dataTypes.ContainsKey k
            then failwith $"Cannot add the same key: '{k}' to the data collection."
            else
                {
                    d1 with
                        dataInfo =
                            {
                                d1.dataInfo with
                                    dataTypes = d1.dataInfo.dataTypes |> Map.add k { start = d1.dataInfo.start; dataType = ScalarData }
                                    start = d1.dataInfo.start + 1
                            }
                        data = Array.append d1.data [| d2 |]
                }

        member inline d1.append (k : 'K, d2 : Vector<'T>) : LinearData<'K, 'T> =
            if d1.dataInfo.dataTypes.ContainsKey k
            then failwith $"Cannot add the same key: '{k}' to the data collection."
            else
                {
                    d1 with
                        dataInfo =
                            {
                                d1.dataInfo with
                                    dataTypes = d1.dataInfo.dataTypes |> Map.add k { start = d1.dataInfo.start; dataType = VectorData d2.value.Length }
                                    start = d1.dataInfo.start + d2.value.Length
                            }
                        data = Array.append d1.data d2.value
                }

        member inline d1.append (k : 'K, d2 : Matrix<'T>) : LinearData<'K, 'T> =
            if d1.dataInfo.dataTypes.ContainsKey k
            then failwith $"Cannot add the same key: '{k}' to the data collection."
            else
                let n1 = d2.value.Length
                let n2 = d2.value[0].Length

                {
                    d1 with
                        dataInfo =
                            {
                                d1.dataInfo with
                                    dataTypes = d1.dataInfo.dataTypes |> Map.add k { start = d1.dataInfo.start; dataType = MatrixData (n1, n2) }
                                    start = d1.dataInfo.start + n1 * n2
                            }
                        data = Array.append d1.data (d2.value |> Array.concat)
                }

        static member inline create i d =
            {
                dataInfo = i
                data = d
            }


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


    /// See: https://github.com/dotnet/fsharp/issues/3302 for (*) operator.
    [<RequireQualifiedAccess>]
    type SparseArray2D<'T when ^T: (static member ( * ) : ^T * ^T -> ^T) and ^T: (static member ( + ) : ^T * ^T -> ^T) and ^T: (static member ( - ) : ^T * ^T -> ^T)> =
        | SparseArray2D of SparseValue2D<'T>[]

        member inline r.value = let (SparseArray2D v) = r in v

        /// Multiplies a 2D sparse array by a scalar value.
        /// Returns a 2D sparse array.
        static member inline (*) (a : SparseArray2D<'T>, b : 'T) : SparseArray2D<'T> =
            let v =
                a.value
                |> Array.map (fun e -> { e with value2D = e.value2D * b })
                |> SparseArray2D

            v

        /// Multiplies a 2D sparse array by a scalar value.
        /// Returns a 2D sparse array.
        static member inline (*) (a : 'T, b : SparseArray2D<'T>) : SparseArray2D<'T> =
            let v =
                b.value
                |> Array.map (fun e -> { e with value2D = a * e.value2D })
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

        /// Multiplies a 2D sparse array by a 2D array (Matrix) element by element.
        /// This is NOT a matrix multiplication.
        /// Returns a 2D sparse array.
        static member inline (*) (a : SparseArray2D<'T>, b : Matrix<'T>) : SparseArray2D<'T> =
            let v =
                a.value
                |> Array.map (fun e -> { e with value2D = e.value2D * b.value[e.i][e.j] })
                |> SparseArray2D

            v

        static member inline create z v =
            v
            |> Array.mapi (fun i e -> e |> Array.mapi (fun j v -> if v >= z then Some { i = i; j = j; value2D = v } else None))
            |> Array.concat
            |> Array.choose id
            |> SparseArray2D


    /// Representation of non-zero value in a sparse 4D array.
    type SparseValue4D<'T when ^T: (static member ( * ) : ^T * ^T -> ^T) and ^T: (static member ( + ) : ^T * ^T -> ^T) and ^T: (static member ( - ) : ^T * ^T -> ^T)> =
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


    type SparseValueArray4D<'T when ^T: (static member ( * ) : ^T * ^T -> ^T) and ^T: (static member ( + ) : ^T * ^T -> ^T) and ^T: (static member ( - ) : ^T * ^T -> ^T)> =
        | SparseValueArray4D of SparseValue4D<'T>[]

        member inline r.value = let (SparseValueArray4D v) = r in v


    /// A 4D representation of 4D sparse tensor where the first two indexes are full ([][] is used)
    /// and the last two are in a SparseArray2D.
    type SparseArray4D<'T when ^T: (static member ( * ) : ^T * ^T -> ^T) and ^T: (static member ( + ) : ^T * ^T -> ^T) and ^T: (static member ( - ) : ^T * ^T -> ^T)> =
        | SparseArray4D of SparseArray2D<'T>[][]

        member inline r.value = let (SparseArray4D v) = r in v

        // TODO kk:20230409 - Due to whatever reason this does not compile.
        // /// Multiplies a 4D sparse array by a scalar value.
        // /// Returns a 4D sparse array.
        // static member inline (*) (a : SparseArray4D<'T>, b : 'T) : SparseArray4D<'T> =
        //     let v =
        //         a.value
        //         |> Array.map (fun e -> e |> Array.map (fun x -> x * b))
        //         |> SparseArray4D
        //
        //     v

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

        member inline a.toSparseValueArray() : SparseValueArray4D<'T> =
            let n1 = a.value.Length
            let n2 = a.value[0].Length

            let value =
                [| for i in 0..(n1 - 1) -> [| for j in 0..(n2 - 1) -> SparseValue4D.createArray i j (a.value[i][j]) |] |]
                |> Array.concat
                |> Array.concat
                |> Array.sortBy (fun e -> e.i, e.j, e.i1, e.j1)
                |> SparseValueArray4D

            value


    /// Performs a Cartesian multiplication of two 1D sparse arrays to obtain a 2D sparse array.
    let inline cartesianMultiply<'T when ^T: (static member ( * ) : ^T * ^T -> ^T) and ^T: (static member ( + ) : ^T * ^T -> ^T) and ^T: (static member ( - ) : ^T * ^T -> ^T)> (a : SparseArray<'T>) (b : SparseArray<'T>) : SparseArray2D<'T> =
        let bValue = b.value
        a.value |> Array.map (fun e -> bValue |> Array.map (fun f -> { i = e.i; j = f.i; value2D = e.value1D * f.value1D })) |> Array.concat |> SparseArray2D<'T>.SparseArray2D


    /// Describes a function domain suitable for integral approximation.
    /// Equidistant grid is used to reduce the number of multiplications.
    type Domain =
        {
            midPoints : Vector<double>
            step : double
            range : double
        }

        member d.noOfIntervals = d.midPoints.value.Length

        member d.integrateValues (v : SparseArray<double>) =
            let sum = v.value |> Array.map (fun e -> e.value1D) |> Array.sum
            sum * d.step

        static member create noOfIntervals minValue maxValue =
            let points = [| for i in 0..noOfIntervals -> minValue + (maxValue - minValue) * (double i) / (double noOfIntervals) |]

            {
                midPoints = [| for i in 0..noOfIntervals - 1 -> (points[i + 1] + points[i]) / 2.0 |] |> Vector
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

        member private d.normalize v = v * d.eeDomain.step * d.infDomain.step
        member private d.integrateValues (a : double[][]) = a |> Array.map (fun e -> e |> Array.sum) |> Array.sum |> d.normalize
        member private d.integrateValues (a : SparseArray2D<double>) = a.value |> Array.map (fun e -> e.value2D) |> Array.sum |> d.normalize

        member d.integrateValues (a : Matrix<double>) =
            let sum = a.value |> Array.map (fun e -> e |> Array.sum) |> Array.sum |> d.normalize
            sum

        member d.integrateValues (a : LinearMatrix<double>) =
            let sum = a.d1Range |> Array.map (fun i -> a.d2Range |> Array.map (fun j -> a.getValue i j) |> Array.sum) |> Array.sum |> d.normalize
            sum

        member private d.integrateValues (a : SparseArray2D<double>, b : LinearMatrix<double>) =
            let bValue = b.getValue
            let sum = a.value |> Array.map (fun e -> e.value2D * (bValue e.i e.j)) |> Array.sum |> d.normalize
            sum

        member d.integrateValues (a : SparseArray4D<double>, b : LinearMatrix<double>) =
            a.value |> Array.map (fun v -> v |> Array.map (fun e -> d.integrateValues (e, b))) |> Matrix

        member d.integrateValues (a : SparseArray4D<double>) =
            a.value |> Array.map (fun v -> v |> Array.map d.integrateValues) |> Matrix

        member d.norm (a : LinearMatrix<double>) = d.integrateValues a

        member d.mean (a : LinearMatrix<double>) =
            let norm = d.norm a

            if norm > 0.0
            then
                let mx = (d.integrateValues (d.eeDomain.midPoints * a)) / norm
                let my = (d.integrateValues (a * d.infDomain.midPoints)) / norm
                (mx, my)
            else (0.0, 0.0)

        member d.stdDev (a : LinearMatrix<double>) =
            let norm = d.norm a

            if norm > 0.0
            then
                let mx, my = d.mean a
                let m2x = (d.integrateValues (d.eeDomain.midPoints * (d.eeDomain.midPoints * a))) / norm
                let m2y = (d.integrateValues ((a * d.infDomain.midPoints) * d.infDomain.midPoints)) / norm
                (Math.Sqrt (m2x - mx * mx), Math.Sqrt (m2y - my * my))
            else (0.0, 0.0)

        static member eeMinValue = -1.0
        static member eeMaxValue = 1.0
        static member infDefaultMinValue = 0.0

        static member create noOfIntervals l2 =
            let eeDomain = Domain.create noOfIntervals Domain2D.eeMinValue Domain2D.eeMaxValue
            let infDomain = Domain.create noOfIntervals Domain2D.infDefaultMinValue l2

            {
                eeDomain = eeDomain
                infDomain = infDomain
                // midPoints = cartesianMultiply eeDomain infDomain
            }
