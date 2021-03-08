namespace ClmSys

module ExitErrorCodes =

    [<Literal>]
    let CompletedSuccessfully = 0

    [<Literal>]
    let CriticalError = -1

    [<Literal>]
    let InvalidCommandLineArgs = -2

    [<Literal>]
    let UnknownException = -3

    [<Literal>]
    let DatabaseErrorOccurred = -4

    [<Literal>]
    let CannotFindSpecifiedFileException = -5

    [<Literal>]
    let BinaryDataUnavailable = -6
