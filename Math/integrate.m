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

(* ============================================== *)

SetAttributes[derivative, HoldFirst];

derivative[a_, x_] :=
    Module[{retVal, lst, len, ii, tmp},
      lst = Apply[List, a];
      tmp = Table[If[ii == 1, (Evaluate[D[lst[[ii]], x]]), (lst[[ii]])], {ii, 1, Length[lst]}];
      retVal = Apply[integrate, tmp];
      Return[retVal];
    ] /; ToString[Head[a]] == "integrate";

derivative[a_, x_] := Apply[Plus, derivative[Evaluate[#], x] & /@ Apply[List, a]] /; ToString[Head[a]] == "Plus";

derivative[a_, x_] :=
    Module[{retVal, aLst, len, ii, jj, tmp, h},
      aLst = Apply[List, Evaluate[a]];
      len = Length[aLst];
      (* Print["diff::aLSt = ", aLst//MatrixForm]; *)
      h = Table[ToString[Head[aLst[[ii]]]], {ii, 1, len}];
      (* Print["diff::h = ", h//MatrixForm]; *)
      tmp = Table[Table[If[ii == jj, (derivative[Evaluate[aLst[[jj]]], x]), aLst[[jj]]], {jj, 1, len}], {ii, 1, len}];
      (* Print["diff::tmp = ", tmp // MatrixForm]; *)
      retVal = Sum[Apply[Times, tmp[[ii]]], {ii, 1, len}];
      (* Print["diff::retVal = ", retVal]; *)
      Return[retVal];
    ] /; ToString[Head[a]] == "Times";

derivative[a_, x_] := Module[{retVal},
  (*
  Print["diff[a,x]::a = ", a];
  Print["diff[a,x]::Head[a] = ", ToString[Head[a]]];
  Print["diff[a,x]::x = ", x];
  *)
  retVal = D[a, x];
  (* Print["diff[a,x]::retVal = ", retVal]; *)
  Return[retVal];
] /; (ToString[Head[a]] != "integrate" && ToString[Head[a]] != "Plus" && ToString[Head[a]] != "Times");

(* ============================================== *)

(* All series are first order and around 0. *)

SetAttributes[series, HoldFirst];
series[-a_, x__] := -series[a, x];
(* series[-a_, x_, y_] := -series[a, x, y]; *)

series[a_, x__] := Apply[Plus, series[Evaluate[#], x] & /@ Apply[List, a]] /; ToString[Head[a]] == "Plus";
(* series[a_, x_, y_] := Apply[Plus, series[Evaluate[#], x, y] & /@ Apply[List, a]] /; ToString[Head[a]] == "Plus"; *)

series[a_, x__] :=
    Module[{retVal, lst, tmp, zeroRule, p, ii, jj, xLst, xAll},
      Print["series[a, x] /; integrate :: x = ", { x }];
      Print["series[a, x] /; integrate :: a = ", a];
      xLst = { x };

      lst = Evaluate[Apply[List, a]];
      Print["series[a, x] /; integrate :: lst = ", lst // MatrixForm];

      xAll = (findAll[a, #]) & /@ xLst;
      Print["series[a, x] /; integrate :: xAll = ", xAll // MatrixForm];

      p = Apply[Join, Table[If[Length[xAll[[ii]]] == 0, ({ xLst[[ii]]}), (xAll[[ii]])], {ii, 1, Length[xLst]}]];
      zeroRule := (# :> 0) & /@ p;
      Print["series[a, x] /; integrate :: p = ", p];
      Print["series[a, x] /; integrate :: zeroRule = ", zeroRule];

      tmp = Table[If[ii == 1, ((lst[[1]] /. zeroRule) + Apply[Plus, Table[(derivative[lst[[1]], p[[jj]]] /.zeroRule) * p[[jj]], {jj, 1, Length[p]}]]), (lst[[ii]])], {ii, 1, Length[lst]}];
      Print["series[a, x] /; integrate :: tmp = ", tmp // MatrixForm];

      retVal = Apply[integrate, tmp];
      Print["series[a, x] /; integrate :: retVal = ", retVal];

      Return[retVal];
    ] /; ToString[Head[a]] == "integrate";

(*
series[a_, x_] :=
    Module[{retVal, lst, tmp, zeroRule, hx, p, ii, jj},
      Print["series[a, x] /; integrate :: x = ", x];
      Print["series[a, x] /; integrate :: a = ", a];

      lst = Evaluate[Apply[List, a]];
      Print["series[a, x] /; integrate :: lst = ", lst // MatrixForm];

      hx = findAll[a, x];

      Print["series[a, x] /; integrate :: hx = ", hx];

      zeroRule := If[Length[hx] == 0, ({ x :> 0 }), ((# :> 0) & /@ hx)];
      p = If[Length[hx] == 0, ({ x }), (hx)];
      Print["series[a, x] /; integrate :: zeroRule = ", zeroRule];
      Print["series[a, x] /; integrate :: p = ", p];

      tmp = Table[If[ii == 1, ((lst[[1]] /. zeroRule) + Apply[Plus, Table[(derivative[lst[[1]], p[[jj]]] /.zeroRule) * p[[jj]], {jj, 1, Length[p]}]]), (lst[[ii]])], {ii, 1, Length[lst]}];
      Print["series[a, x] /; integrate :: tmp = ", tmp // MatrixForm];

      retVal = Apply[integrate, tmp];
      Print["series[a, x] /; integrate :: retVal = ", retVal];

      Return[retVal];
    ] /; ToString[Head[a]] == "integrate";
*)
(*
series[a_, x_, y_] :=
    Module[{retVal, lst, tmp, zeroRule, hx, hy, p, ii, jj},
      Print["series[a, x, y] /; integrate :: x = ", x, ", y = ", y];
      Print["series[a, x, y] /; integrate :: a = ", a];

      lst = Evaluate[Apply[List, a]];
      Print["series[a, x, y] /; integrate :: lst = ", lst // MatrixForm];

      hx = findAll[a, x];
      hy = findAll[a, y];

      Print["series[a, x, y] /; integrate :: hx = ", hx];
      Print["series[a, x, y] /; integrate :: hy = ", hy];

      zeroRule := Join[If[Length[hx] == 0, ({ x :> 0 }), ((# :> 0) & /@ hx)], If[Length[hy] == 0, ({ y :> 0 }), ((# :> 0) & /@ hy)]];
      p = Join[If[Length[hx] == 0, ({ x }), (hx)], If[Length[hy] == 0, ({ y }), (hy)]];
      Print["series[a, x, y] /; integrate :: zeroRule = ", zeroRule];
      Print["series[a, x, y] /; integrate :: p = ", p];

      tmp = Table[If[ii == 1, ((lst[[1]] /. zeroRule) + Apply[Plus, Table[(derivative[lst[[1]], p[[jj]]] /.zeroRule) * p[[jj]], {jj, 1, Length[p]}]]), (lst[[ii]])], {ii, 1, Length[lst]}];
      Print["series[a, x, y] /; integrate :: tmp = ", tmp // MatrixForm];

      retVal = Apply[integrate, tmp];
      Print["series[a, x, y] /; integrate :: retVal = ", retVal];

      Return[retVal];
    ] /; ToString[Head[a]] == "integrate";
*)
(* ============================================== *)
