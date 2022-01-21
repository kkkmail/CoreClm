(* ::Package:: *)

(* ============================================== *)

(* https://mathematica.stackexchange.com/questions/172302/find-all-the-subexpressions-with-a-given-head-in-an-expression *)
findAll = DeleteDuplicates@Cases[#, Blank[#2], Infinity] &;

(* ============================================== *)

(* https://mathematica.stackexchange.com/questions/92269/distributing-function-arguments-with-function-compositions-how-to-compute-f *)
Default[purify,2]=sym:Except[HoldPattern@Symbol[___],_Symbol]/;Not@MemberQ[Attributes[sym],Constant];
Default[purify,3]={-1};
purify[expr_,Shortest[patt_.,1],Shortest[levelspec_.,2],opts:OptionsPattern[Replace]]:=Evaluate@Replace[Unevaluated[expr],func:patt:>func[##],levelspec,opts]&

(* ============================================== *)

id[x_] := x;
SetAttributes[hasConstants, HoldFirst];
hasConstants[aLst_?ListQ, b_] := AnyTrue[FreeQ[#, b] & /@ aLst, id];
hasConstants[aLst_?ListQ, b_, c_] := AnyTrue[(FreeQ[#, b] && FreeQ[#, c]) & /@ aLst, id];

SetAttributes[hasNoVars, HoldFirst];
hasNoVars[a_?ListQ, b_] := AllTrue[FreeQ[#, b] & /@ a, id];
hasNoVars[a_?ListQ, b_, c_] := AllTrue[FreeQ[#, b] & /@ a, id] && AllTrue[FreeQ[#, c] & /@ a, id];

SetAttributes[notNormalized, HoldFirst];

notNormalized[aLst_?ListQ] := Module[{retVal},
  retVal = ! (aLst === Sort[Apply[List, Evaluate[Apply[Times, aLst]]]]);
  (*
  Print["notNormalized[a]::aLst = ",aLst];
  Print["notNormalized[a]::retVal = ", retVal];
  *)
  Return[retVal];
];

notNormalized[a__] := False;

SetAttributes[freeQ, HoldFirst];
freeQ[a_, b_, c_] := FreeQ[a, b] && FreeQ[a, c];

SetAttributes[partitionConstants, HoldFirst];
partitionConstants[a_?ListQ, b_] := GatherBy[a, FreeQ[#, b] &];
partitionConstants[a_?ListQ, b_, c_] := GatherBy[a, freeQ[#, b, c] &];

(* ============================================== *)

SetAttributes[integrate, HoldFirst];
integrate[0, b_] := 0;
integrate[0, b_, c_] := 0;
integrate[-a_, b_] := -integrate[a, b];
integrate[-a_, b_, c_] := -integrate[a, b, c];

integrate[Times[Plus[a__], d__], b_] := Apply[Plus, integrate[Evaluate[#*d], b] & /@ {a}];
integrate[Times[Plus[a__], d__], b_, c_] := Apply[Plus, integrate[Evaluate[#*d], b, c] & /@ {a}];

integrate[Plus[a__], b_] := Apply[Plus, integrate[Evaluate[#], b] & /@ {a}];
integrate[Plus[a__], b_, c_] := Apply[Plus, integrate[Evaluate[#], b, c] & /@ {a}];

integrate[a_*integrate[b_, c_], d_] := integrate[Evaluate[a*b], c, d];
integrate[integrate[a_, b_], c_] := integrate[a, b, c];

integrate[Times[aInp__], b_] :=
  Module[{retVal, p, v, i1, i2, f, aLst, a, vc, aVal},
    (*
    Print["integrate[a,b]::aInp = ", aInp];
    Print["integrate[a,b]::b = ", b];
    *)
    aVal = Evaluate[Times[aInp]];
    (* Print["integrate[a,b]::aVal = ", aVal]; *)

    If[aVal == 0, Return[0]];

    aLst = Sort[Apply[List, aVal]];
    (* Print["integrate[a,b]::aLst = ", aLst]; *)
    
    a = Apply[Times, aLst];
    (* Print["integrate[a,b]::a = ", a]; *)
    
    p = partitionConstants[aLst, b];
    (* Print["integrate[a,b]::p = ", p]; *)
    
    v = hasNoVars[aLst, b];
    (* Print["integrate[a,b]::v = ", v]; *)
    
    vc = hasConstants[aLst, b];
    (* Print["integrate[a,b]::vc = ", vc]; *)
    
    f = FreeQ[#, b] & /@ aLst;
    (* Print["integrate[a,b]::FreeQ[a,b] = ",f]; *)
    
    If[vc && ! v,
     (
      (* Print["Has constants and variables."]; *)
      i1 = If[f[[1]], 1, 2];
      i2 = 3 - i1;
      (*
      Print["integrate[a,b]::i1 = ", i1];
      Print["integrate[a,b]::i2 = ", i2];
      *)
      retVal = Apply[Times, p[[i1]]]*integrate[Evaluate[Apply[Times, p[[i2]]]], b];
      ),
     If[vc && v,
       (
        (* Print["Has only constants."]; *)
        retVal = Times[p[[1]]];
        ),
       If[! vc && ! v,
         (
          (* Print["Has only variables."]; *)
          retVal = integrate[Evaluate[Apply[Times, p[[1]]]], b];
          ),
         (
          Print["Has NOTHING. This should not happen."];
          retVal = Indeterminate;
          )
         ];
       ];
     ];
    
    (* Print["integrate[a,b]::retVal = ", retVal]; *)
    
    Return[retVal];
    ] /; (hasConstants[{aInp}, b] || notNormalized[{aInp}]);

integrate[Times[aInp__], b_, c_] := 
  Module[{retVal, p, v, i1, i2, f, aLst, a, vc, aVal},
    (*
    Print["integrate[a,b,c]::aInp = ", aInp];
    Print["integrate[a,b,c]::b = ", b];
    *)
    aVal = Evaluate[Times[aInp]];
    (* Print["integrate[a,b]::aVal = ", aVal]; *)

    If[aVal == 0, Return[0]];

    aLst = Sort[Apply[List, aVal]];
    (* Print["integrate[a,b,c]::aLst = ", aLst]; *)
    
    a = Apply[Times, aLst];
    (* Print["integrate[a,b,c]::a = ", a]; *)
    
    p = partitionConstants[aLst, b, c];
    (* Print["integrate[a,b,c]::p = ", p]; *)
    
    v = hasNoVars[aLst, b, c];
    (* Print["integrate[a,b,c]::v = ", v]; *)
    
    vc = hasConstants[aLst, b, c];
    (* Print["integrate[a,b,c]::vc = ", vc]; *)
    
    f = freeQ[#, b, c] & /@ aLst;
    (* Print["integrate[a,b,c]::freeQ[a,b,c] = ",f]; *)
    
    If[vc && ! v,
     (
      (* Print["Has constants and variables."]; *)
      i1 = If[f[[1]], 1, 2];
      i2 = 3 - i1;
      (*
      Print["integrate[a,b,c]::i1 = ", i1];
      Print["integrate[a,b,c]::i2 = ", i2];
      *)
      retVal = Apply[Times, p[[i1]]] * integrate[Evaluate[Apply[Times, p[[i2]]]], b, c];
      ),
     If[vc && v,
       (
        (* Print["Has only constants."]; *)
        retVal = Times[p[[1]]];
        ),
       If[! vc && ! v,
         (
          (* Print["Has only variables."]; *)
          retVal = integrate[Evaluate[Apply[Times, p[[1]]]], b, c];
          ),
         (
          Print["Has NOTHING. This should not happen."];
          retVal = Indeterminate;
          )
         ];
       ];
     ];
    
    (* Print["integrate[a,b,c]::retVal = ", retVal]; *)
    
    Return[retVal];
    ] /; (hasConstants[{aInp}, b, c] || notNormalized[{aInp}]);

(* ============================================== *)
(*
simplify[integrate[a_, b_]] := integrate[Evaluate[Simplify[a]], b];
simplify[integrate[a_, b_, c_]] := integrate[Evaluate[Simplify[a]], b, c];

simplify[integrate[a_, b_], assum_] := integrate[Evaluate[Simplify[a, assum]], b];
simplify[integrate[a_, b_, c_], assum_] := integrate[Evaluate[Simplify[a, assum]], b, c];
*)

simplify[a_, assum___] :=
    Module[{retVal, lst, len, ii, tmp},
      lst = Apply[List, a];
      (* Print["simplify::a = ", a]; *)
      tmp = Table[If[ii == 1, (Evaluate[Simplify[lst[[ii]], assum]]), (lst[[ii]])], {ii, 1, Length[lst]}];
      (* Print["simplify::tmp = ", tmp]; *)
      retVal = Apply[integrate, tmp];
      (* Print["simplify::retVal = ", retVal]; *)
      Return[retVal];
    ] /; ToString[Head[a]] == "integrate";

simplify[a_, assum___] := Apply[Plus, simplify[Evaluate[#], assum] & /@ Apply[List, a]] /; ToString[Head[a]] == "Plus";
simplify[a_, assum___] := Apply[Times, simplify[Evaluate[#], assum] & /@ Apply[List, a]] /; ToString[Head[a]] == "Times";
simplify[a_, assum___] := Simplify[a, assum] /; (ToString[Head[a]] != "integrate" && ToString[Head[a]] != "Plus" && ToString[Head[a]] != "Times");

(*
simplify[a_] := Simplify[a];
simplify[a_, assum_] := Simplify[a, assum];
*)
(* ============================================== *)

(* All series are first order and around 0. *)
(*
SetAttributes[series, HoldFirst];
series[-a_, x_] := -integrate[a, x];
series[-a_, x_, y_] := -series[a, x, y];

series[Times[Plus[a__], b__], x_] := Apply[Plus, series[Evaluate[#*b], x] & /@ {a}];
series[Times[Plus[a__], b__], x_, y_] := Apply[Plus, series[Evaluate[#*b], x, y] & /@ {a}];

series[Plus[a__], x_] := Apply[Plus, series[Evaluate[#], x] & /@ {a}];
series[Plus[a__], x_, y_] := Apply[Plus, series[Evaluate[#], x, y] & /@ {a}];

series[integrate[a_,b_],x_]:=integrate[series[a,x],b];
series[integrate[a_,b_,c_],x_]:=integrate[series[a,x],b,c];
series[integrate[a_,b_,c_,d_],x_]:=integrate[series[a,x],b,c,d];

(* series[Times[Plus[a__], d__], b_] := Apply[Plus, series[Evaluate[#*d], b] & /@ {a}]; *)
(* series[Times[a__],b_]:=Apply[Plus, series[Evaluate[#], b] & /@ {a}]; *)
series[Plus[a__],b_]:=Apply[Plus, series[Evaluate[#], b] & /@ {a}];

series[a_,x_]:=Normal[Series[a,{x,0,1}]];

series[a_, x_, y_] :=
    Module[{retVal, hx, hy},
      hx = findAll[a, x];
      hy = findAll[a, y];
      Print["series::x = ", x, "y = ", y];

      If[Length[hx] == 0 && Length[hy] == 0,
        (
          Print["series::Both x and y are symbols."];
        ),
        If[Length[hx] == 0 && Length[hy] > 0,
        (
          Print["series::x is symbol and y is function:", hy];
        ),
          If[Length[hx] > 0 && Length[hy] == 0],
          (

          ),
          (

          )]
      ];

      If[Length[hx] == 0, hx = {x}];
      If[Length[hy] == 0, hy = {y}];
      Print["series::hx = ", hx];
      Print["series::hy = ", hy];

      Return[retVal];
    ];
*)
(* ============================================== *)


