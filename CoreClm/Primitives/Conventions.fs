namespace Primitives

/// TODO kk:20230420 - Conventions have not been formalized yet.
///
/// Use suffixes: Value, Array, Data, Func, Var, Params, Info, State and come up with naming conventions
/// to use them in F#. Explain why each suffix should be used in each case. Add more suffixes if found necessary.
///
/// In F#, it is important to have clear and consistent naming conventions to make the code more readable
/// and maintainable. Using suffixes can help to easily identify the purpose and type of variables, functions,
/// and other elements in your code. Here's a suggestion for a naming convention using the suffixes you provided,
/// along with a brief explanation of when to use each:
///
///  1. Value (Val): Use this suffix for immutable values or constants. This helps to indicate that the value
///     will not change throughout the program.
///     Example: let piVal = 3.14159265359
///
///  2. Array (Arr): Use this suffix for arrays to make it clear that you are working with a collection of values.
///     Example: let intArr = [|1; 2; 3|]
///
///  3. Data (Data): Use this suffix for records, discriminated unions, or other complex data types to clarify that
///     they represent a specific structure or object.
///     Example: type CustomerData = {Name: string; Age: int}
///
///  4. Func (Func): Use this suffix for functions or delegates to signify that the element represents a
///     callable piece of code.
///     Example: let addFunc x y = x + y
///
///  5. Variable (Var): Use this suffix for mutable variables or reference cells to highlight that their values
///     can change during program execution.
///     Example: let mutable counterVar = 0
///
///  6. Parameters (Params): Use this suffix for tuples or records that hold a collection of parameters,
///     typically for functions or configuration settings.
///     Example: let dbConnectionParams = {Server: "localhost"; Port: 5432}
///
///  7. Information (Info): Use this suffix for metadata or information that describes other elements,
///     like module or function documentation.
///     Example: let authorInfo = "John Doe"
///
///  8. State (State): Use this suffix for types or values that represent the state of an object or a system.
///     This is particularly useful in functional programming, where functions may receive and return state.
///     Example: type GameState = {PlayerPosition: int * int; Score: int}
///
/// In addition to these suffixes, you can consider the following additional suffixes for your F# naming convention:
///
///  9. List (List): Use this suffix for list values to make it clear that you are working with a collection
///     of values in a linked-list structure.
///     Example: let intList = [1; 2; 3]
///
/// 10. Option (Opt): Use this suffix for option types to indicate that a value might be present or absent (Some or None).
///     Example: let findCustomerOpt id = customers |> List.tryFind (fun c -> c.Id = id)
///
/// Remember that naming conventions can vary across different projects and teams. It's important to be consistent within your project and to follow any established guidelines in the codebase you are working on.

/// Conventions used in the code.
/// Not all code follows the conventions yet, but new code should.
/// The following suffixes are used:
///     [No suffix]
///     Value / Array / etc...
///     + Data
///     + Func
///     + Var
///     Params
///     Info
///     State
///
/// Unwrapping method should follow the convention that matches the suffixes. And suffix is omitted then
/// unwrapping method still should follow convention.
/// The following unwrapping method names should be used:
///     value
///     invoke
///     data
///     var
module Conventions =

    // /// Suffixes that should be used in the type names based on what the type does.
    // type Suffix =
    //     /// Use for some very general structures, which cah be used anywhere.
    //     /// See type Sample.
    //     | NoSuffix
    //
    //     /// Data suffix should be used to describe the following things and under the hood hold
    //     /// some collections [mostly doubles] which:
    //     | Value
    //
    //     /// Doesn't change over time (or we do know how they change over time).
    //     | Data
    //
    //     /// Func suffix should be used in single case DU, which hold functions.
    //     | Func
    //
    //     /// We need to calculate their change over time (even though the actual structures are immutable).
    //     | Var
    //
    //     ///
    //     | Params
    //
    //     ///
    //     | Info


    /// Use for some very general structures, which cah be used anywhere.
    /// A single case DU wrapper around any general simple immutable data that is NOT varied over time.
    /// The unwrapping method should be called value.
    /// It CAN be serialized into the database.
    /// It could have a default value, in which case a static method should be called defaultValue.
    type Sample =
        | Sample of double

        member r.value = let (Sample v) = r in v
        static member defaultValue = Sample 0.0


    /// A single case DU wrapper around any general simple low level immutable data, which can be used for both var and data.
    /// Use Var or Data if possible instead of Value.
    /// The unwrapping method should be called value.
    /// It CAN be serialized into the database.
    /// It could have a default value, in which case a static method should be called defaultValue.
    type SampleValue =
        | SampleValue of double

        member r.value = let (SampleValue v) = r in v
        static member defaultValue = SampleValue 0.0


    /// A single case DU wrapper around any function.
    /// The unwrapping method should be called "invoke" instead of "value".
    /// It CANNOT be serialized into the database.
    type SampleFunc =
        | SampleFunc of (int -> int)

        member r.invoke = let (SampleFunc v) = r in v


    /// A single or multiple case DU, which can map its cases into specific functions.
    /// The part of the name without Value suffix should match the name of the relevant Func wrapper (as above).
    /// The unwrapping method should be called as the type of the function wrapper that it unwraps into
    /// and it may take parameter if needed.
    type SampleFuncValue =
        | Func1
        | Func2 of int * int

        member f.sampleFunc _ =
            match f with
            | Func1 -> SampleFunc (fun _ -> 1)
            | Func2 (a, b) -> SampleFunc (fun i -> a * i + b)


    /// A single case DU wrapper around any general immutable data that IS varied over time.
    /// The unwrapping method should be called var.
    /// It CAN be serialized into the database.
    type SampleVar =
        | SampleVar of double

        member r.var = let (SampleVar v) = r in v


    /// Another example of a single case DU wrapper around any general immutable data that IS varied over time.
    /// The unwrapping method should be called var.
    /// It CAN be serialized into the database.
    type SampleArrayVar =
        | SampleArrayVar of double[]

        member r.var = let (SampleArrayVar v) = r in v


    /// A single case DU wrapper around any general immutable data that is NOT varied over time
    /// or its change over time is known.
    /// The unwrapping method should be called data.
    /// It CAN be serialized into the database.
    type SampleData =
        | SampleData of double

        member r.data = let (SampleData v) = r in v


    /// Another example of a single case DU wrapper around any general immutable data that is NOT varied over time
    /// or its change over time is known.
    /// The unwrapping method should be called data.
    /// It CAN be serialized into the database.
    type SampleArrayData =
        | SampleArrayData of double[]

        member r.data = let (SampleArrayData v) = r in v


    /// A complex (record) variable.
    /// The labels should generally be called the same as underlying types but with small caps first letter.
    type SampleComplexVar =
        {
            sampleVar : SampleVar
            sampleArrayVar : SampleArrayVar
        }


    /// A complex (record) data.
    /// The labels should generally be called the same as underlying types but with small caps first letter.
    type SampleComplexData =
        {
            sampleData : SampleData
            sampleArrayData : SampleArrayData
        }

    /// What to do if we need to pass various types structures at the same time (e.g., var and data?)?
    type ToDo =
        | ToDo
