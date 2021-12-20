namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt
open Clm.Distributions
open ClmSys.ContGenPrimitives
open Clm.ReactionRatesBase

module Defaults_004_005_000 =

    /// Activated catalytic reactions playground.
    ///
    /// The value of activationScarcity is set to 1 because we want to activate all "chosen" catalysts
    /// and they are already scarce enough.
    type DefaultDataParam =
        {
            activationScarcity : double
            activationMultiplier : double

            acCatSynthScarcity : double
            acCatSynthMultiplier : double
            acCatSynthSimilarity : double

            acCatDestrScarcity : double
            acCatDestrMultiplier : double
            acCatDestrSimilarity : double

            ligForward : double
            ligBackward : double

            acFwdCatLigScarcity : double
            acFwdCatLigMultiplier : double
            acFwdCatLigSimilarity : double

            acBkwCatLigScarcity : double
            acBkwCatLigMultiplier : double
            acBkwCatLigSimilarity : double

            sugarForward : double
            sugarBackward : double
            sugarScarcity : double
        }

        override p.ToString() = $"parameters: %0A{p}"

        static member zero =
            {
                activationScarcity = 0.0
                activationMultiplier = 1.0

                acCatSynthScarcity = 0.0
                acCatSynthMultiplier = 1.0
                acCatSynthSimilarity = 0.0

                acCatDestrScarcity = 0.0
                acCatDestrMultiplier = 1.0
                acCatDestrSimilarity = 0.0

                ligForward = 0.001
                ligBackward = 0.010

                acFwdCatLigScarcity = 0.0
                acFwdCatLigMultiplier = 1.0
                acFwdCatLigSimilarity = 0.0

                acBkwCatLigScarcity = 0.0
                acBkwCatLigMultiplier = 1.0
                acBkwCatLigSimilarity = 0.0

                sugarForward = 100.0
                sugarBackward = 0.001
                sugarScarcity = 0.001
            }

        static member zero01 =
            {
                DefaultDataParam.zero
                    with
                    sugarForward = 10.0
            }

        static member zero02 =
            {
                DefaultDataParam.zero
                    with
                    sugarScarcity = 0.002
            }

        static member zero03 =
            {
                DefaultDataParam.zero
                    with
                    sugarScarcity = 0.0005
            }

        /// Use: "ContGenAdm.exe add -i 4005000004 -n 10 -m 3 -y 10 -t 250000 -r 1 -g > -a.txt" for these values.
        static member codeGenValue_001 =
            {
                activationScarcity = 1.0
                activationMultiplier = 10_000.0

                acCatSynthScarcity = 0.001_000
                acCatSynthMultiplier = 100_000.0
                acCatSynthSimilarity = 0.1

                acCatDestrScarcity = 0.001_000
                acCatDestrMultiplier = 100_000.0
                acCatDestrSimilarity = 0.1

                ligForward = 0.001
                ligBackward = 0.010

                acFwdCatLigScarcity = 0.000_000_100
                acFwdCatLigMultiplier = 100_000.0
                acFwdCatLigSimilarity = 0.000_000_100

                acBkwCatLigScarcity = 0.000_000_100
                acBkwCatLigMultiplier = 100_000.0
                acBkwCatLigSimilarity = 0.000_000_100

                sugarForward = 100.0
                sugarBackward = 0.001
                sugarScarcity = 0.001
            }

        /// Use: "ContGenAdm.exe add -i 4005000006 -n 7 -m 3 -y 10 -t 250000 -r 1 -g > -a.txt" for these values.
        static member codeGenValue_002 =
            {
                activationScarcity = 1.0
                activationMultiplier = 10_000.0

                acCatSynthScarcity = 0.001_000
                acCatSynthMultiplier = 100_000.0
                acCatSynthSimilarity = 0.1

                acCatDestrScarcity = 0.001_000
                acCatDestrMultiplier = 100_000.0
                acCatDestrSimilarity = 0.1

                ligForward = 0.001
                ligBackward = 0.010

                acFwdCatLigScarcity = 0.000_001_000
                acFwdCatLigMultiplier = 100_000.0
                acFwdCatLigSimilarity = 0.000_000_100

                acBkwCatLigScarcity = 0.000_001_000
                acBkwCatLigMultiplier = 100_000.0
                acBkwCatLigSimilarity = 0.000_000_100

                sugarForward = 100.0
                sugarBackward = 0.001
                sugarScarcity = 0.001
            }


        /// Use: "ContGenAdm.exe add -i 4005000005 -n 20 -m 3 -y 10 -t 250000 -r 1 -g > -a.txt" for these values.
        static member defaultValue =
            {
                activationScarcity = 1.0
                activationMultiplier = 10_000.0

                acCatSynthScarcity = 0.000_050
                acCatSynthMultiplier = 100_000.0
                acCatSynthSimilarity = 0.2

                acCatDestrScarcity = 0.000_050
                acCatDestrMultiplier = 100_000.0
                acCatDestrSimilarity = 0.2

                ligForward = 0.001
                ligBackward = 0.010

                acFwdCatLigScarcity = 0.000_000_002
                acFwdCatLigMultiplier = 100_000.0
                acFwdCatLigSimilarity = 0.000_000_002

                acBkwCatLigScarcity = 0.000_000_002
                acBkwCatLigMultiplier = 100_000.0
                acBkwCatLigSimilarity = 0.000_000_002

                sugarForward = 100.0
                sugarBackward = 0.001
                sugarScarcity = 0.001
            }


    let data =
            let d = DefaultDataParam.defaultValue

            let d105 = { d with
                                acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                                acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                                acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                                acCatDestrSimilarity = d.acCatDestrSimilarity * 0.0 } // 105

            [
                DefaultDataParam.zero               // 0
                DefaultDataParam.zero01             // 1
                DefaultDataParam.zero02             // 2
                DefaultDataParam.zero03             // 3

                DefaultDataParam.codeGenValue_001   // 4

                d                                   // 5
                DefaultDataParam.codeGenValue_002   // 6

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 1.5 }   // 7
                { d with acCatDestrScarcity = d.acCatDestrScarcity * 1.5 }   // 8
                { d with acFwdCatLigScarcity = d.acFwdCatLigScarcity * 1.5 } // 9
                { d with acBkwCatLigScarcity = d.acBkwCatLigScarcity * 1.5 } // 10

                { d with acCatDestrScarcity = d.acCatDestrScarcity * 2.0 }   // 11
                { d with acCatDestrScarcity = d.acCatDestrScarcity * 2.5 }   // 12 +
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.8 }   // 13
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.6 }   // 14

                { d with
                         acCatDestrScarcity = d.acCatDestrScarcity * 2.0
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.8 }   // 15 +

                // ===============================================================

                { d with sugarForward = 0.0 }                                // 16

                // ===============================================================

                { d with
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.6
                         acCatDestrScarcity = d.acCatDestrScarcity * 2.0 }   // 17

                { d with
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.8
                         acCatDestrScarcity = d.acCatDestrScarcity * 2.5 }   // 18

                { d with
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.6
                         acCatDestrScarcity = d.acCatDestrScarcity * 2.5 }   // 19

                // ===============================================================

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.5 }   // 20
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.4 }   // 21
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0 }   // 22

                { d with
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.5
                         acCatDestrScarcity = d.acCatDestrScarcity * 3.0 }   // 23

                // ===============================================================

                { d with acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.8 } // 24
                { d with acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.8 } // 25

                { d with
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 1.5
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.8 } // 26

                { d with
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.5
                         acCatDestrScarcity = d.acCatDestrScarcity * 3.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 1.5
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.8 } // 27

                { d with acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.6 } // 28
                { d with acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.6 } // 29

                { d with
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 3.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 1.5
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.8 } // 30

                // ===============================================================

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.1 }   // 31
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.2 }   // 32
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.3 }   // 33
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.7 }   // 34
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.9 }   // 35
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 1.1 }   // 36
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 1.2 }   // 37
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 1.3 }   // 38
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 1.4 }   // 39
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 2.0 }   // 40
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 2.5 }   // 41
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 3.0 }   // 42

                // ===============================================================

                { d with acCatDestrScarcity = d.acCatDestrScarcity * 0.6 }   // 43
                { d with acCatDestrScarcity = d.acCatDestrScarcity * 0.8 }   // 44
                { d with acCatDestrScarcity = d.acCatDestrScarcity * 1.2 }   // 45
                { d with acCatDestrScarcity = d.acCatDestrScarcity * 3.0 }   // 46
                { d with acCatDestrScarcity = d.acCatDestrScarcity * 4.0 }   // 47
                { d with acCatDestrScarcity = d.acCatDestrScarcity * 5.0 }   // 48

                { d with acCatDestrScarcity = d.acCatDestrScarcity * 0.0 }   // 49
                { d with acCatDestrScarcity = d.acCatDestrScarcity * 0.1 }   // 50
                { d with acCatDestrScarcity = d.acCatDestrScarcity * 0.2 }   // 51
                { d with acCatDestrScarcity = d.acCatDestrScarcity * 0.3 }   // 52
                { d with acCatDestrScarcity = d.acCatDestrScarcity * 0.4 }   // 53
                { d with acCatDestrScarcity = d.acCatDestrScarcity * 0.5 }   // 54

                // ===============================================================

                { d with acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.2 } // 55
                { d with acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.4 } // 56
                { d with acFwdCatLigScarcity = d.acFwdCatLigScarcity * 1.2 } // 57
                { d with acFwdCatLigScarcity = d.acFwdCatLigScarcity * 2.0 } // 58
                { d with acFwdCatLigScarcity = d.acFwdCatLigScarcity * 2.5 } // 59
                { d with acFwdCatLigScarcity = d.acFwdCatLigScarcity * 3.0 } // 60

                // ===============================================================

                { d with acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.2 } // 61
                { d with acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.4 } // 62
                { d with acBkwCatLigScarcity = d.acBkwCatLigScarcity * 1.2 } // 63
                { d with acBkwCatLigScarcity = d.acBkwCatLigScarcity * 2.0 } // 64
                { d with acBkwCatLigScarcity = d.acBkwCatLigScarcity * 2.5 } // 65
                { d with acBkwCatLigScarcity = d.acBkwCatLigScarcity * 3.0 } // 66

                // ===============================================================

                // From: AcCat_FactorAnalysis__004.nb
                { d with
                         acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 1.74953
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.974201 } // 67

                // ===============================================================

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 1.5 }   // 68

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 2.0 }   // 69

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.6 } // 70

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.4 } // 71

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.8 } // 72

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.6 } // 73

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.8 } // 74

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.2 } // 75

                // ===============================================================

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.6
                         acCatDestrSimilarity = d.acCatDestrSimilarity * 0.5 }   // 76

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.6
                         acCatDestrSimilarity = d.acCatDestrSimilarity * 1.5 }   // 77

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.6
                         acFwdCatLigSimilarity = d.acFwdCatLigSimilarity * 0.5 } // 78

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.6
                         acFwdCatLigSimilarity = d.acFwdCatLigSimilarity * 1.5 } // 79

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.6
                         acBkwCatLigSimilarity = d.acBkwCatLigSimilarity * 0.5 } // 80

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.6
                         acBkwCatLigSimilarity = d.acBkwCatLigSimilarity * 1.5 } // 81

                // ===============================================================

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.6
                         acCatDestrSimilarity = d.acCatDestrSimilarity * 0.0 }   // 82

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.6
                         acFwdCatLigSimilarity = d.acFwdCatLigSimilarity * 0.0 } // 83

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.6
                         acBkwCatLigSimilarity = d.acBkwCatLigSimilarity * 0.0 } // 84

                // ===============================================================

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0 } // 85

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0 } // 86

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.2 } // 87

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.4}  // 88

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 0.0 }   // 89

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 0.2 }   // 90

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 0.4 }   // 91

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 0.6 }   // 92

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 0.8 }   // 93

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 2.5 }   // 94

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 3.0 }   // 95

                // ===============================================================
                // 3 pair of parameters (acCatSynth, acFwdCatLig, acBkwCatLig) set to 0.

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0 } // 96

                // All are zeros, which makes it effectively produce only sugar.
                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 0.0 }   // 97

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 0.5 }   // 98

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 1.5 }   // 99

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 2.0 }   // 100

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 2.5 }   // 101

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 3.0 }   // 102

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 4.0 }   // 103

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 5.0 }   // 104

                // ===============================================================

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrSimilarity = d.acCatDestrSimilarity * 0.0 } // 105

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrSimilarity = d.acCatDestrSimilarity * 0.5 } // 106

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrSimilarity = d.acCatDestrSimilarity * 1.5 } // 107

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrSimilarity = d.acCatDestrSimilarity * 2.0 } // 108

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrSimilarity = d.acCatDestrSimilarity * 2.5 } // 109

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrSimilarity = d.acCatDestrSimilarity * 3.0 } // 110

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrSimilarity = d.acCatDestrSimilarity * 3.5 } // 111

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrSimilarity = d.acCatDestrSimilarity * 4.0 } // 112

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrSimilarity = d.acCatDestrSimilarity * 4.5 } // 113

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrSimilarity = d.acCatDestrSimilarity * 5.0 } // 114

                // ===============================================================

                { d105 with sugarScarcity = d105.sugarScarcity * 0.2 }         // 115
                { d105 with sugarScarcity = d105.sugarScarcity * 0.5 }         // 116
                { d105 with sugarScarcity = d105.sugarScarcity * 2.0 }         // 117
                { d105 with sugarScarcity = d105.sugarScarcity * 5.0 }         // 118

                // ===============================================================
                // 3 pair of parameters (acCatSynth, acFwdCatLig, acBkwCatLig) set to 0.
                // See 96 - 104 above.

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 0.1 }   // 119

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 0.2 }   // 120

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 0.3 }   // 121

                { d with acCatSynthScarcity = d.acCatSynthScarcity * 0.0
                         acFwdCatLigScarcity = d.acFwdCatLigScarcity * 0.0
                         acBkwCatLigScarcity = d.acBkwCatLigScarcity * 0.0
                         acCatDestrScarcity = d.acCatDestrScarcity * 0.4 }   // 122

                // ===============================================================
            ]
            |> withRowNumberUniqueOrFail


    let getDefaultValue (n, e) =
        let clmDefaultValueId = (4_005_000_000L + n) |> ClmDefaultValueId
        printfn $"clmDefaultValueId = %A{clmDefaultValueId}, e = %A{e}"

        let description = e.ToString()
        let catRateGenType = ByEnantiomerPairs FixedVal
        let successNumberType = ThresholdBased

        let defaultRateParams =
            //===========================================================
            let wasteRecyclingParam = ReactionRateProviderParams.defaultWasteRecyclingParam 0.1
            //===========================================================
            let synthParam = ReactionRateProviderParams.defaultSynthRndParamImpl (Some 0.001, Some 0.001)

            let acCatSynthRndParam = (synthParam, (Some e.acCatSynthScarcity), e.acCatSynthMultiplier)

            let acCatSynthParam =
                ReactionRateProviderParams.defaultAcCatSynthSimParam acCatSynthRndParam (Some e.acCatSynthSimilarity) catRateGenType
            //===========================================================
            let destrParam = ReactionRateProviderParams.defaultDestrRndParamImpl (Some 0.001, None)

            let acCatDestrRndParam = (destrParam, (Some e.acCatDestrScarcity), e.acCatDestrMultiplier)

            let acCatDestrParam =
                ReactionRateProviderParams.defaultAcCatDestrSimParam acCatDestrRndParam (Some e.acCatDestrSimilarity) catRateGenType
            //===========================================================
            let ligParam = ReactionRateProviderParams.defaultLigRndParamImpl (e.ligForward, e.ligBackward)

            let acFwdCatLigParam =
                ReactionRateProviderParams.defaultAcFwdCatLigSimParam (ligParam, Some e.acFwdCatLigScarcity, e.acFwdCatLigMultiplier) (Some e.acFwdCatLigSimilarity) catRateGenType

            let acBkwCatLigParam =
                ReactionRateProviderParams.defaultAcBkwCatLigSimParam (ligParam, Some e.acBkwCatLigScarcity, e.acBkwCatLigMultiplier) (Some e.acBkwCatLigSimilarity) catRateGenType
            //===========================================================
            let sugParam = ReactionRateProviderParams.defaultSugarSynthRndParamImpl ((Some e.sugarForward, Some e.sugarBackward), Some e.sugarScarcity)
            //===========================================================
            let activationParam = ReactionRateProviderParams.defaultActivationParamImpl (Some e.activationScarcity, e.activationMultiplier)
            //===========================================================
            let rates =
                [
                    wasteRecyclingParam
                    synthParam |> SynthesisRateParam
                    destrParam |> DestructionRateParam
                    ligParam |> LigationRateParam

                    if e.sugarForward > 0.0 && e.sugarScarcity > 0.0
                    then
                        sugParam |> SugarSynthesisRateParam

                        if (e.activationScarcity > 0.0) then activationParam
                        if (e.activationScarcity > 0.0 && e.acCatSynthScarcity > 0.0) then acCatSynthParam
                        if (e.activationScarcity > 0.0 && e.acCatDestrScarcity > 0.0) then acCatDestrParam

                        if (e.activationScarcity > 0.0 && e.acFwdCatLigScarcity > 0.0) then acFwdCatLigParam
                        if (e.activationScarcity > 0.0 && e.acBkwCatLigScarcity > 0.0) then acBkwCatLigParam
                ]
            //===========================================================

            {
                rateParams = rates
                successNumberType = successNumberType
            }

        {
            clmDefaultValueId = clmDefaultValueId
            defaultRateParams = defaultRateParams
            description = Some description
        }

    let defaultValues =
        printfn "\n"

        data
        |> List.map getDefaultValue
        |> updateDescription "All activated reactions - cat lig with similarity + all sugars playground."
