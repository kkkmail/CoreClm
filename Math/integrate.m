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

SetAttributes[freeQ, HoldFirst];
freeQ[a_, x__] := AllTrue[(FreeQ[a, #]) & /@ { x }, id];

SetAttributes[hasConstants, HoldFirst];
hasConstants[aLst_?ListQ, x__] := AnyTrue[(freeQ[#, x]) & /@ aLst, id];

SetAttributes[hasNoVars, HoldFirst];
hasNoVars[a_?ListQ, x__] := AllTrue[freeQ[#, x] & /@ a, id];

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

SetAttributes[partitionConstants, HoldFirst];
partitionConstants[a_?ListQ, x__] := GatherBy[a, freeQ[#, x] &];

(* ============================================== *)

SetAttributes[integrate, HoldFirst];
integrate[0, x__] := 0;
integrate[-a_, x__] := -integrate[a, x];
integrate[Times[Plus[a__], b__], x__] := Apply[Plus, integrate[Evaluate[# * b], x] & /@ { a }];
integrate[Plus[a__], x__] := Apply[Plus, integrate[Evaluate[#], x] & /@ { a }];
integrate[a_ * integrate[b_, x__], y__] := Apply[integrate, Join[{ Evaluate[a * b] }, Sort[Join[{ x }, { y }]]]];
integrate[integrate[a_, x__], y__] := Apply[integrate, Join[{ Evaluate[a] }, Sort[Join[{ x }, { y }]]]];

integrate[Times[aInp__], x__] :=
  Module[{retVal, p, v, i1, i2, f, aLst, a, vc, aVal},
    aVal = Evaluate[Times[aInp]];
    If[aVal == 0, Return[0]];
    aLst = Sort[Apply[List, aVal]];
    a = Apply[Times, aLst];
    p = partitionConstants[aLst, x];
    v = hasNoVars[aLst, x];
    vc = hasConstants[aLst, x];
    f = freeQ[#, x] & /@ aLst;

    (*
    Print["integrate[a,b,c]::aInp = ", aInp];
    Print["integrate[a,b,c]::x = ", { x }];
    Print["integrate[a,b]::aVal = ", aVal];
    Print["integrate[a,b,c]::aLst = ", aLst];
    Print["integrate[a,b,c]::a = ", a];
    Print["integrate[a,b,c]::p = ", p];
    Print["integrate[a,b,c]::v = ", v];
    Print["integrate[a,b,c]::vc = ", vc];
    Print["integrate[a,b,c]::freeQ[a,b,c] = ",f];
    *)
    
    If[vc && ! v,
     (
      (* Print["Has constants and variables."]; *)
      i1 = If[f[[1]], 1, 2];
      i2 = 3 - i1;
      (*
      Print["integrate[a,b,c]::i1 = ", i1];
      Print["integrate[a,b,c]::i2 = ", i2];
      *)
      retVal = Apply[Times, p[[i1]]] * integrate[Evaluate[Apply[Times, p[[i2]]]], x];
      ),
     If[vc && v,
       (
        (* Print["Has only constants."]; *)
        retVal = Times[p[[1]]];
        ),
       If[! vc && ! v,
         (
          (* Print["Has only variables."]; *)
          retVal = integrate[Evaluate[Apply[Times, p[[1]]]], x];
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
    ] /; (hasConstants[{aInp}, x] || notNormalized[{aInp}]);

(* ============================================== *)

SetAttributes[simplify, HoldFirst];

simplify[a_, assum___] :=
    Module[{retVal, lst, len, ii, tmp},
      lst = Apply[List, a];
      tmp = Table[If[ii == 1, (Evaluate[Simplify[lst[[ii]], assum]]), (lst[[ii]])], {ii, 1, Length[lst]}];
      retVal = Apply[integrate, tmp];

      (*
      Print["simplify /; integrate :: a = ", a];
      Print["simplify /; integrate :: tmp = ", tmp];
      Print["simplify /; integrate :: retVal = ", retVal];
      *)

      Return[retVal];
    ] /; ToString[Head[a]] == "integrate";

simplify[a_, assum___] :=
    Module[{retVal},
      retVal = Simplify[Expand[Apply[Plus, simplify[Evaluate[#], assum] & /@ Apply[List, a]]]];

      (*
      Print["simplify /; Plus :: a = ", a];
      Print["simplify /; Plus :: retVal = ", retVal];
      *)

      Return[retVal];
    ] /; ToString[Head[a]] == "Plus";

simplify[a_, assum___] :=
    Module[{retVal},
      retVal = Simplify[Expand[Apply[Times, simplify[Evaluate[#], assum] & /@ Apply[List, a]]]];

      (*
      Print["simplify /; Times :: a = ", a];
      Print["simplify /; Times :: retVal = ", retVal];
      *)

      Return[retVal];
    ] /; ToString[Head[a]] == "Times";

simplify[a_, assum___] := Simplify[Expand[a], assum] /; (ToString[Head[a]] != "integrate" && ToString[Head[a]] != "Plus" && ToString[Head[a]] != "Times");

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
      h = Table[ToString[Head[aLst[[ii]]]], {ii, 1, len}];
      tmp = Table[Table[If[ii == jj, (derivative[Evaluate[aLst[[jj]]], x]), aLst[[jj]]], {jj, 1, len}], {ii, 1, len}];
      retVal = Sum[Apply[Times, tmp[[ii]]], {ii, 1, len}];

      (*
      Print["diff::aLSt = ", aLst//MatrixForm];
      Print["diff::h = ", h//MatrixForm];
      Print["diff::tmp = ", tmp // MatrixForm];
      Print["diff::retVal = ", retVal];
      *)

      Return[retVal];
    ] /; ToString[Head[a]] == "Times";

derivative[a_, x_] := Module[{retVal},
  retVal = D[a, x];

  (*
  Print["diff[a,x]::a = ", a];
  Print["diff[a,x]::Head[a] = ", ToString[Head[a]]];
  Print["diff[a,x]::x = ", x];
  Print["diff[a,x]::retVal = ", retVal];
  *)

  Return[retVal];
] /; (ToString[Head[a]] != "integrate" && ToString[Head[a]] != "Plus" && ToString[Head[a]] != "Times");

(* ============================================== *)

(* All series are first order and around 0. *)

SetAttributes[series, HoldFirst];
(* series[-a_, x__] := -series[a, x]; *)

series[a_, x__] :=
    Module[{retVal},
      retVal = Apply[Plus, series[Evaluate[#], x] & /@ Apply[List, a]];

      (*
      Print["series[a, x] /; Plus :: x = ", { x }];
      Print["series[a, x] /; Plus :: a = ", a];
      Print["series[a, x] /; Plus :: a = ", retVal];
      Print["==="];
      *)

      Return[retVal];
    ] /; ToString[Head[a]] == "Plus";

series[a_, x__] :=
    Module[{retVal, lst, tmp, zeroRule, p, ii, jj, xLst, xAll},
      xLst = { x };
      lst = Evaluate[Apply[List, a]];
      xAll = (findAll[{ a }, #]) & /@ xLst;
      p = Apply[Join, Table[If[Length[xAll[[ii]]] == 0, ({ xLst[[ii]]}), (xAll[[ii]])], {ii, 1, Length[xLst]}]];
      zeroRule := (# :> 0) & /@ p;
      tmp = Evaluate[Table[If[ii == 1, ((lst[[1]] /. zeroRule) + Apply[Plus, Table[(derivative[lst[[1]], p[[jj]]] /.zeroRule) * p[[jj]], {jj, 1, Length[p]}]]), (lst[[ii]])], {ii, 1, Length[lst]}]];
      retVal = Apply[integrate, tmp];

      (*
      Print["series[a, x] /; integrate :: x = ", { x }];
      Print["series[a, x] /; integrate :: a = ", a];
      Print["series[a, x] /; integrate :: lst = ", lst // MatrixForm];
      Print["series[a, x] /; integrate :: xAll = ", xAll // MatrixForm];
      Print["series[a, x] /; integrate :: p = ", p // MatrixForm];
      Print["series[a, x] /; integrate :: zeroRule = ", zeroRule // MatrixForm];
      Print["series[a, x] /; integrate :: tmp = ", tmp // MatrixForm];
      Print["series[a, x] /; integrate :: retVal = ", retVal];
      Print["==="];
      *)

      Return[retVal];
    ] /; ToString[Head[a]] == "integrate";

series[a_, x__] :=
    Module[{retVal, aLst, len, ii, jj, tmp, xLst, xAll, p, zeroRule},
      aLst = Apply[List, a];
      len = Length[aLst];
      xLst = { x };
      xAll = (findAll[{ a }, #]) & /@ xLst;
      p = Apply[Join, Table[If[Length[xAll[[ii]]] == 0, ({ xLst[[ii]]}), (xAll[[ii]])], {ii, 1, Length[xLst]}]];
      zeroRule := (# :> 0) & /@ p;
      (* Here we are over counting the zero-th term (Length[aLst] - 1) times. We need to subtract that. *)
      tmp = Table[Table[If[ii == jj, (series[Evaluate[aLst[[jj]]], x]), (Evaluate[aLst[[jj]] /. zeroRule])], {jj, 1, len}], {ii, 1, len}];
      retVal = Evaluate[Sum[Apply[Times, tmp[[ii]]], {ii, 1, len}] - ((Length[aLst] - 1) * (a) /. zeroRule)];

      (*
      Print["series[a, x] /; Times :: aLSt = ", aLst // MatrixForm];
      Print["series[a, x] /; Times :: xLst = ", xLst // MatrixForm];
      Print["series[a, x] /; Times :: xAll = ", xAll // MatrixForm];
      Print["series[a, x] /; Times :: p = ", p // MatrixForm];
      Print["series[a, x] /; Times :: zeroRule = ", zeroRule // MatrixForm];
      Print["series[a, x] /; Times :: tmp = ", tmp // MatrixForm];
      Print["series[a, x] /; Times::retVal = ", retVal];
      Print["==="];
      *)

      Return[simplify[retVal]];
    ] /; ToString[Head[a]] == "Times";

series[a_, x__] :=
    Module[{retVal, p, zeroRule, xLst, xAll, ii, jj},
      xLst = { x };
      xAll = (findAll[{ a }, #]) & /@ xLst;
      p = Apply[Join, Table[If[Length[xAll[[ii]]] == 0, ({ xLst[[ii]]}), (xAll[[ii]])], {ii, 1, Length[xLst]}]];
      zeroRule := (# :> 0) & /@ p;
      retVal = Evaluate[(a /. zeroRule) + Apply[Plus, Table[(derivative[a, p[[jj]]] /.zeroRule) * p[[jj]], {jj, 1, Length[p]}]]];

      (*
      Print["series[a, x] :: a = ", a];
      Print["series[a, x] :: x = ", { x }];
      Print["series[a, x] :: xAll = ", xAll // MatrixForm];
      Print["series[a, x] :: p = ", p];
      Print["series[a, x] :: zeroRule = ", zeroRule];
      Print["series[a, x] :: retVal = ", retVal];
      Print["==="];
      *)

      Return[simplify[retVal]];
    ] /; (ToString[Head[a]] != "integrate" && ToString[Head[a]] != "Plus" && ToString[Head[a]] != "Times");

(* Returns true if v is a valid series variable, e.g.: { x, 0 } *)
seriesVariable[v_?ListQ] := Length[v] == 2 && !NumericQ[v[[1]]];
seriesVariable[v_] := False /; (!ListQ[v]);

seriesAllVariables[x__] := AllTrue[(seriesVariable[#]) & /@ { x }, id];

(* TODO kk:20220124 - make series above support higher order expansions and then rename this into series. *)
(* See: https://mathematica.stackexchange.com/questions/15023/multivariable-taylor-expansion-does-not-work-as-expected *)
series2[a_, order_Integer?NonNegative, x__] :=
    Module[{retVal, xLst, t, xRules, newA},
      xLst = { x };
      xRules = (#[[1]] -> (#[[1]] - #[[2]]) * t + #[[2]]) & /@ xLst;
      newA = a /. xRules;
      retVal = Normal[Series[newA, {t, 0, order}]] /. { t -> 1 };
      Return[retVal];
    ] /; seriesAllVariables[x];

(* ============================================== *)

(* Rules *)
varRules := { A->(\[CapitalDelta]+\[Theta]),a->(-\[CapitalDelta]+\[Theta]), u[x_] :> u00*(u0[x] + \[Delta]*u1[x]), n[x_] :> L  (1 + \[CapitalDelta]n[x])/2, m[x_] :> L  (1 - \[CapitalDelta]n[x])/2 };
zeroRules := { \[Delta]u[x_] :> 0, \[Delta] :> 0, \[CapitalDelta] :> 0 };
\[CapitalDelta]nFunc[x_] := (2 L x)/(1 + Sqrt[1 + 4 x^2]);

(* ============================================== *)
(* Changes integrate into Integrate from -Infinity to Infinity *)
toIntegrate[i_, assum___] := Apply[Plus, toIntegrate[Evaluate[#], assum] & /@ Apply[List, i]] /; ToString[Head[i]] == "Plus";
toIntegrate[i_, assum___] := Apply[Times, toIntegrate[Evaluate[#], assum] & /@ Apply[List, i]] /; ToString[Head[i]] == "Times";

toIntegrate[i_, assum___] :=
    Module[{retVal, lst, param},
      lst = Apply[List, i];
      param = Join[{ lst[[1]] }, ( { #, -Infinity, Infinity } ) & /@ Drop[lst, 1]];
      If[Length[{ assum }] > 0, param = Join[param, { Assumptions -> assum }]];
      (* Print["toIntegrate :: param = ", param]; *)
      retVal = Apply[Integrate, param];
      Return[retVal];
    ] /; ToString[Head[i]] == "integrate";

toIntegrate[i_, assum___] := i /; (ToString[Head[i]] != "integrate" && ToString[Head[i]] != "Plus" && ToString[Head[i]] != "Times");

(* ============================================== *)
(* Changes integrate into Integrate from -1 to 1 *)
toIntegrate2[i_, assum___] := Apply[Plus, toIntegrate2[Evaluate[#], assum] & /@ Apply[List, i]] /; ToString[Head[i]] == "Plus";
toIntegrate2[i_, assum___] := Apply[Times, toIntegrate2[Evaluate[#], assum] & /@ Apply[List, i]] /; ToString[Head[i]] == "Times";

toIntegrate2[i_, assum___] :=
    Module[{retVal, lst, param},
      lst = Apply[List, i];
      param = Join[{ lst[[1]] }, ( { #, -1, 1 } ) & /@ Drop[lst, 1]];
      If[Length[{ assum }] > 0, param = Join[param, { Assumptions -> assum }]];
      (* Print["toIntegrate :: param = ", param]; *)
      retVal = Apply[Integrate, param];
      Return[retVal];
    ] /; ToString[Head[i]] == "integrate";

toIntegrate2[i_, assum___] := i /; (ToString[Head[i]] != "integrate2" && ToString[Head[i]] != "Plus" && ToString[Head[i]] != "Times");

(* ============================================== *)
(* Sqrt simplification *)

(* sqrtNotNormalized[x_] *)
sqrtFwdRule[sqrtFunc_] := { Sqrt[x_] :>  sqrtFunc[x] }
sqrtInvRule[sqrtFunc_] := { sqrtFunc[x_] :> Sqrt[x] }
(* applySqrt[sqrtFunc_, x__] := Apply[Times, (sqrtFunc[#]) & /@  Apply[List, x]] /; ToString[Head[x]] == "Times"; *)
sqrt[x__] := Apply[Times, (sqrt[#]) & /@  Apply[List, x]] /; ToString[Head[x]] == "Times";
simplifySqrt[expr_] := ToExpression[ToString[InputForm[expr] /. sqrtFwdRule[sqrt]]] /. sqrtInvRule[sqrt];

(* ============================================== *)
(* https://mathematica.stackexchange.com/questions/257742/eigenvalue-problem-for-fredholm-integral *)

fredholmSolver[noOfPoints_?IntegerQ, domain : {_, _}, integrand_] :=
    Module[{len, val, vec, nodes, weights, midgrid, mm, vec1, s,vecNew},
        {nodes, weights} = Most[NIntegrate`GaussRuleData[noOfPoints, MachinePrecision]];
        midgrid = Rescale[nodes, {0, 1}, domain];
        len = Length[midgrid];
        (* Print["midgrid = ", midgrid // MatrixForm]; *)
        (* Print["integrand[0,0] = ", integrand[0.0, 0.0]]; *)
        mm = Table[integrand[midgrid[[i]], midgrid[[j]]] * weights[[i]], {i, len}, {j, len}];
        (* Print["mm = ", mm // MatrixForm]; *)
        {val, vec} = Eigensystem[mm];
        (* Print["val = ", val // MatrixForm]; *)
        (* Print["vec = ", vec // MatrixForm]; *)
        vec1 = vec[[1]];
        s = Sum[vec1[[ii]], {ii, Floor[(len / 2)] + 1, len}];
        (* Print["s = ", s]; *)
        (* Print["vec1 = ", vec1]; *)

        vecNew = Table[
            (
                vec1 = vec[[ii]];
                s = Sum[Re[vec1[[jj]]], {jj, Floor[(len/2)] + 1, len}];
                If[s >= 0, vec1, -vec1]
            ), {ii, len}];

        Return[{val, vecNew}];
    ];

(* ============================================== *)
