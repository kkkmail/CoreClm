namespace Primitives

/// <summary>
/// Updated Naming Conventions in F#
/// </summary>
///
/// <remarks>
///
/// <para>Suffixes to Use:</para>
///
/// <list type="bullet">
/// <item>
/// <term>Param - </term>
/// <description>Used for tuples or records that hold a collection of parameters, typically for functions or configuration settings. Considered the primary source from which data is created or manipulated.</description>
/// <example>Example: <code>dbConnectionParam = {server = "localhost"; port = 5432}</code></example>
/// </item>
///
/// <item>
/// <term>Data - </term>
/// <description>Used for records, discriminated unions, or other complex data types. Data is considered secondary, usually derived or modified based on parameters.</description>
/// <example>Example: <code>customerData = {name = "John"; age = 30}</code></example>
/// </item>
///
/// <item>
/// <term>Info - </term>
/// <description>Used for metadata or information that describes other elements, like module or function documentation.</description>
/// <example>Example: <code>authorInfo = "Jane Doe"</code></example>
/// </item>
///
/// <item>
/// <term>State - </term>
/// <description>Used for types or values that represent the state of an object or a system.</description>
/// <example>Example: <code>gameState = {playerPosition = (2,3); score = 40}</code></example>
/// </item>
///
/// <item>
/// <term>FuncValue - </term>
/// <description>Used for Discriminated Unions which map to a function. Allows serialization of the DU while retaining the capability to map it back to a function.</description>
/// <example>Example: <code>type MapperFuncValue = AddOne | MultiplyByTwo</code></example>
/// </item>
///
/// <item>
/// <term>Delegate - </term>
/// <description>Replaces the current use of the Proxy suffix for passing a collection of functions or a record of functions.</description>
/// <example>Example: <code>type mathDelegate = {add : int -&gt; int -&gt; int; multiply : int -&gt; int -&gt; int}</code></example>
/// </item>
///
/// <item>
/// <term>Hybrid - </term>
/// <description>Used for exceptional cases where it is beneficial to pass both data and functions together as a single parameter.</description>
/// <example>Example: <code>type configHybrid = {data : customerData; action : int -&gt; int}</code></example>
/// </item>
///
/// <item>
/// <term>Proxy - </term>
/// <description>Used for third-party and/or communication connections where calculation is sent over machine boundaries.</description>
/// <example>Example: <code>type httpProxy = {address : string; port : int}</code></example>
/// </item>
///
/// <item>
/// <term>Opt - </term>
/// <description>Used in record labels to enhance readability, specifying that the field can be Some or None.</description>
/// <example>Example: <code>type customerData = {name : string; ageOpt : int option}</code></example>
/// </item>
/// </list>
///
/// <para>Additional Notes:</para>
/// <list type="bullet">
/// <item>
/// <description>For collections, plural forms are used rather than a suffix to indicate the collection type, e.g., forms : List&lt;int&gt;.</description>
/// </item>
///
/// <item>
/// <description>Functions carry an action in their name to indicate their purpose, e.g., add instead of addFunc.</description>
/// </item>
///
/// <item>
/// <description>Mutable variables are rare, so the Var suffix can be omitted.</description>
/// </item>
///
/// <item>
/// <description>If a function returns an Option type, indicate it in the function name rather than using a suffix, e.g., tryFindCustomer.</description>
/// </item>
/// </list>
///
/// </remarks>
module ConventionsV2 =
    /// Updated Naming Conventions in F#
    ///
    /// Suffixes to Use:
    ///
    /// - Param: Used for tuples or records that hold a collection of parameters, typically for functions or configuration settings. Considered the primary source from which data is created or manipulated.
    ///   Example: `dbConnectionParam = {server = "localhost"; port = 5432}`
    ///
    /// - Data: Used for records, discriminated unions, or other complex data types. Data is considered secondary, usually derived or modified based on parameters.
    ///   Example: `customerData = {name = "John"; age = 30}`
    ///
    /// - Info: Used for metadata or information that describes other elements, like module or function documentation.
    ///   Example: `authorInfo = "Jane Doe"`
    ///
    /// - State: Used for types or values that represent the state of an object or a system.
    ///   Example: `gameState = {playerPosition = (2,3); score = 40}`
    ///
    /// - FuncValue: Used for Discriminated Unions which map to a function. Allows serialization of the DU while retaining the capability to map it back to a function.
    ///   Example: `type MapperFuncValue = AddOne | MultiplyByTwo`
    ///
    /// - Delegate: Replaces the current use of the Proxy suffix for passing a collection of functions or a record of functions.
    ///   Example: `type mathDelegate = {add : int -> int; multiply : int -> int}`
    ///
    /// - Hybrid: Used for exceptional cases where it is beneficial to pass both data and functions together as a single parameter.
    ///   Example: `type configHybrid = {data : customerData; action : int -> int}`
    ///
    /// - Proxy: Used for third-party and/or communication connections where calculation is sent over machine boundaries.
    ///   Example: `type httpProxy = {address = string; port : int}`
    ///
    /// - Opt: Used in record labels to enhance readability, specifying that the field can be Some or None.
    ///   Example: `type customerData = {name : string; ageOpt : int option}`
    ///
    /// Additional Notes:
    ///
    /// - For collections, plural forms are used rather than a suffix to indicate the collection type, e.g., forms : List<int>.
    /// - Functions carry an action in their name to indicate their purpose, e.g., add instead of addFunc.
    /// - Mutable variables are rare, so the Var suffix can be omitted.
    /// - If a function returns an Option type, indicate it in the function name rather than using a suffix, e.g., tryFindCustomer.
    let x = 0


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
