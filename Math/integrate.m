(* ::Package:: *)

(* ============================================== *)
(* Some standard Plot options *)
SetOptions[Plot, BaseStyle -> FontSize -> 18];
legendFontSize = 16;
tickFontSize = 20;

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

fredholmPrecision = 200;

getMidGridAndWeights[noOfPoints_?IntegerQ, domain : {_, _}] :=
      Module[{nodes, weights, midGrid},
          {nodes, weights} = Most[NIntegrate`GaussRuleData[noOfPoints, fredholmPrecision]];
          midGrid = Rescale[nodes, {0, 1}, domain];
          Return[SetPrecision[{midGrid, weights}, fredholmPrecision]];
      ];

getMidGridAndWeights1[noOfPoints_?IntegerQ, domain : {_, _}] :=
    Module[{weights, midGrid, step, start},
        step = SetPrecision[(domain[[2]] - domain[[1]]) / noOfPoints, fredholmPrecision];
        start = SetPrecision[domain[[1]] + step / 2, fredholmPrecision];
        midGrid = SetPrecision[Table[start + step * ii, {ii, 0, noOfPoints - 1}], fredholmPrecision];
        weights = SetPrecision[Table[step, {ii, 1, noOfPoints}], fredholmPrecision];
        Return[{midGrid, weights}];
    ];

fredholmSolver[noOfPoints_?IntegerQ, domain : {_, _}, integrand_] :=
    Module[{len, val, vec, weights, midGrid, mm, vec1, s1,s2,vecNew},
        {midGrid, weights} = getMidGridAndWeights[noOfPoints, domain];
        len = Length[midGrid];
        (* Print["midGrid = ", midGrid // MatrixForm]; *)
        (* Print["integrand[0,0] = ", integrand[0.0, 0.0]]; *)

        (* Print["midGrid[[1]] = ", FullForm[midGrid[[1]]]]; *)
        (* Print["weights[[1]] = ", FullForm[weights[[1]]]]; *)

        mm = Table[SetPrecision[integrand[SetPrecision[midGrid[[i]], fredholmPrecision], SetPrecision[midGrid[[j]], fredholmPrecision]] * SetPrecision[weights[[i]], fredholmPrecision], fredholmPrecision], {i, len}, {j, len}];
        (* Print["mm = ", mm // MatrixForm]; *)

        (* Print["mm[[1, 1]] = ", mm[[1, 1]]]; *)
        {val, vec} = Eigensystem[mm];
        (* Print["val = ", val // MatrixForm]; *)
        (* Print["vec = ", vec // MatrixForm]; *)
        vec1 = vec[[1]];
        s = Sum[vec1[[ii]], {ii, Floor[(len / 2)] + 1, len}];
        (* Print["s = ", s]; *)

        vecNew = Table[
            (
                vec1 = vec[[ii]];
                s1 = Sum[Re[vec1[[jj]]], {jj, 1, Floor[(len/2)]}];
                s2 = Sum[Re[vec1[[jj]]], {jj, Floor[(len/2)] + 1, len}];
                If[Abs[s1] > Abs[s2], If[s1 >= 0, vec1, -vec1], If[s2 >= 0, vec1, -vec1]]
            ), {ii, len}];

        Return[{val, vecNew, mm}];
    ];

(* Gets mu and sigma out of the first two eigenvectors *)
(*
getMuSigma[noOfPoints_?IntegerQ, domain : {_, _}, vec_] :=
    Module[{e1, e2, mu1, mu2, s1, s2, mu, s, weights, midgrid, len, ii, norm1, norm2, m1, m2, p1, p2, mp1, mp2},
        (* Print[sep]; *)
        {midgrid, weights} = getMidGridAndWeights[noOfPoints, domain];
        len = Length[midgrid];

        (* Print["len = ", len, ", midgrid = ",midgrid, ", weights = ",
        weights]; *)

        e1 = Re[vec[[1]]];
        e2 = Re[vec[[2]]];

        m1 = Sum[e1[[ii]] * weights[[ii]], {ii, 1, Floor[(len / 2)]}];
        m2 = Sum[e2[[ii]] * weights[[ii]], {ii, 1, Floor[(len / 2)]}];
        p1 = Sum[e1[[ii]] * weights[[ii]], {ii, Floor[(len / 2)] + 1, len}];
        p2 = Sum[e2[[ii]] * weights[[ii]], {ii, Floor[(len / 2)] + 1, len}];
        mp1 = Sqrt[Abs[m1 * p1]];
        mp2 = Sqrt[Abs[m2 * p2]];

        (* Print["m1 = ", m1, ", p1 = ", p1, ", m2 = ", m2, ", p2 = ", p2, ", mp1 = ", mp1, ", mp2 = ", mp2]; *)
        (* Print[ListLinePlot[{Table[{midgrid[[ii]],Re[e1[[ii]]]},{ii,1, len}],Table[{midgrid[[ii]],Re[e2[[ii]]]},{ii,1,len}]}, Frame\[Rule]True, GridLines\[Rule]Automatic, PlotRange\[Rule]All]]; *)

        norm1 = Sum[e1[[ii]] * weights[[ii]], {ii, 1, Floor[(len / 2)]}];
        norm2 = Sum[e2[[ii]] * weights[[ii]], {ii, Floor[(len / 2)] + 1, len}];
        mu1 = Sum[midgrid[[ii]] * e1[[ii]] * weights[[ii]], {ii, 1, Floor[(len / 2)]}] / norm1;
        mu2 = Sum[midgrid[[ii]] * e2[[ii]] * weights[[ii]], {ii, Floor[(len / 2)] + 1, len}] / norm2;

        s1 = Sqrt[(Sum[midgrid[[ii]]^2 * e1[[ii]]*weights[[ii]], {ii, 1, Floor[(len / 2)]}] / norm1) - mu1^2];
        s2 = Sqrt[(Sum[midgrid[[ii]]^2 * e2[[ii]]*weights[[ii]], {ii, Floor[(len / 2)] + 1, len}] / norm2) - mu2^2];

        (* Print["getMuSigma - L1(weighted): norm1 = ", norm1, ", norm2 = ", norm2]; *)
        (* Print["    mu1 = ", mu1, ", mu2 = ", mu2, ", s1 = ", s1, ", s2 = ", s2]; *)

        {mu, s} = If[mp1 < mp2, {mu1, s1}, {mu2, s2}];
        (* Print["mu = ", mu, ", s = ", s]; *)
        Return[{mu, s}];
    ];
*)

getMuSigma[noOfPoints_?IntegerQ, domain : {_, _}, vec_] :=
  Module[{e1, e2, mu1m, mu2m, s1m, s2m, mu1p, mu2p, s1p, s2p, mu, s, weights, midgrid, len, ii, norm1m, norm2m, norm1p, norm2p, m1, m2, p1, p2, m1p1, m2p2, m1p2, m2p1},
   (* Print[sep]; *)
   {midgrid, weights} = SetPrecision[getMidGridAndWeights[noOfPoints, domain], MachinePrecision];
   len = Length[midgrid];

   (* Print["len = ",len,", midgrid = ",midgrid,", weights = ",
   weights]; *)

   e1 = Re[vec[[1]]];
   e2 = Re[vec[[2]]];

   (*
   Print["e1 = ", Chop[SetPrecision[e1, MachinePrecision],10^-3]];
   Print[sep];
   Print["e2 = ", Chop[SetPrecision[e2, MachinePrecision],10^-3]];
   Print[sep];
   *)

   m1 = Abs[SetPrecision[Sum[e1[[ii]]*weights[[ii]], {ii, 1, Floor[(len/2)]}],MachinePrecision]];
   m2 = Abs[SetPrecision[Sum[e2[[ii]]*weights[[ii]], {ii, 1, Floor[(len/2)]}],MachinePrecision]];
   p1 = Abs[SetPrecision[Sum[e1[[ii]]*weights[[ii]], {ii, Floor[(len/2)] + 1, len}],MachinePrecision]];
   p2 = Abs[SetPrecision[Sum[e2[[ii]]*weights[[ii]], {ii, Floor[(len/2)] + 1, len}],MachinePrecision]];

   m1p1 = Sqrt[Abs[m1*p1]];
   m2p2 = Sqrt[Abs[m2*p2]];
   m1p2 = Sqrt[Abs[m1*p2]];
   m2p1 = Sqrt[Abs[m2*p1]];

   (* Print["m1 = ",m1,", p1 = ",p1,", m2 = ",m2,", p2 = ",p2, ", m1p1 = ",m1p1,", m2p2 = ",m2p2,", m1p2 = ",m1p2,", m2p1 = ", m2p1]; *)
   (*Print[ListLinePlot[{Table[{midgrid[[ii]],Re[e1[[ii]]]},{ii,1,len}],Table[{midgrid[[ii]],Re[e2[[ii]]]},{ii,1,len}]},Frame\[Rule]True,GridLines\[Rule]Automatic,PlotRange\[Rule]All]];*)

   norm1m = Sum[e1[[ii]]*weights[[ii]], {ii, 1, Floor[(len/2)]}];
   norm2p = Sum[e2[[ii]]*weights[[ii]], {ii, Floor[(len/2)] + 1, len}];
   norm1p = Sum[e1[[ii]]*weights[[ii]], {ii, Floor[(len/2)] + 1, len}];
   norm2m = Sum[e2[[ii]]*weights[[ii]], {ii, 1, Floor[(len/2)]}];

   mu1m = Sum[midgrid[[ii]]*e1[[ii]]*weights[[ii]], {ii, 1, Floor[(len/2)]}]/norm1m;
   mu2p = Sum[midgrid[[ii]]*e2[[ii]]*weights[[ii]], {ii, Floor[(len/2)] + 1, len}]/norm2p;
   mu1p = Sum[midgrid[[ii]]*e1[[ii]]*weights[[ii]], {ii, Floor[(len/2)] + 1, len}]/norm1p;
   mu2m = Sum[midgrid[[ii]]*e2[[ii]]*weights[[ii]], {ii, 1, Floor[(len/2)]}]/norm2m;

   s1m = Sqrt[(Sum[midgrid[[ii]]^2*e1[[ii]]*weights[[ii]], {ii, 1, Floor[(len/2)]}]/norm1m) - mu1m^2];
   s2p = Sqrt[(Sum[midgrid[[ii]]^2*e2[[ii]]*weights[[ii]], {ii, Floor[(len/2)] + 1, len}]/norm2p) - mu2p^2];
   s1p = Sqrt[(Sum[midgrid[[ii]]^2*e1[[ii]]*weights[[ii]], {ii, Floor[(len/2)] + 1, len}]/norm1p) - mu1p^2];
   s2m = Sqrt[(Sum[midgrid[[ii]]^2*e2[[ii]]*weights[[ii]], {ii, 1, Floor[(len/2)]}]/norm2m) - mu2m^2];

   (*
   Print["getMuSigma - L1(weighted): norm1m = ",norm1m,", norm2p = ", norm2p", norm1p = ",norm1p", norm2m = ",norm2m];
   Print["    mu1m = ",mu1m,", mu2p = ",mu2p,", mu1p = ",mu1p, ", mu2m = ",mu2m];
   Print["    s1m = ",s1m,", s2p = ",s2p,", s1p = ",s1p,", s2m = ", s2m];
   *)

   {mu, s} = If[m2p1 > (m1p1 + m2p2 + m1p2), {mu1p, s1p}, If[m1p2 > (m1p1 + m2p2 + m2p1), {mu1m, s1m}, If[m1p1 < m2p2, {mu1m, s1m}, {mu2p, s2p}]]];
   Print["mu = ", mu, ", s = ", s];
   Return[{mu, s}];
   ];

getMuSigmaFromData[d_] :=
  Module[{val, vec, mu1, sigma, k, noOfPoints, mORa, e, useQuadratic, g, mu, s},
   {val, vec, {mu1, sigma}, k, {noOfPoints, mORa, e, useQuadratic, g}} = d;
   {mu, s} = getMuSigma[noOfPoints, domain, vec];
   Return[{mu, s}];
   ];


(* Fixes a pair of two eigenvectors by symmetrizing and renormalizing them. *)
fixVec[vec1_, vec2_, weights_] :=
    Module[{noOfPoints, e1, e2, ep, em, epi, emi, e1n, e2n, n1, n2, e1nn, e2nn, e1New, e2New, mu, sigma, e0n, e0nm1, nnn, l1Norm, ii, jj},
        noOfPoints = Length[vec1];
        e1 = Re[vec1];
        e2 = Re[vec2];
        ep = e1 + e2;
        em = e1 - e2;
        epi = Reverse[ep];
        emi = Reverse[em];
        e1n = ep + epi;
        e2n = em - emi;
        n1 = Sqrt[Sum[e1n[[ii]]^2 * weights[[ii]], {ii, 1, noOfPoints}]];
        n2 = Sqrt[Sum[e2n[[ii]]^2 * weights[[ii]], {ii, 1, noOfPoints}]];
        e1nn = e1n / n1;
        e2nn = e2n / n2;
        e1New = (e1nn + e2nn) / 2;
        e2New = (e1nn - e2nn) / 2;
        Return[{e1New, e2New}];
    ];

(* ============================================== *)

gFactor = 0;

delta[x_, y_, e_] := e / ((ArcTan[(1 - y) / e] + ArcTan[(1 + y) / e])*((x - y)^2 + e^2));
delta1[x_, y_, e_] := 2 * Exp[-(x - y)^2 / e^2]/(e * Sqrt[Pi] * (Erf[(1 - y) / e] + Erf[(1 + y) / e]));

(* https://en.wikipedia.org/wiki/Stirling%27s_approximation *)
stirling[nn_] := Sqrt[2 * Pi * nn] * (nn / E)^nn * E^(1 / (12 * nn + 1 / 2))

binomial[nn_, kk_] := Gamma[nn + 1] / (Gamma[kk + 1] * Gamma[nn - kk + 1])
entropy[x_, nn_, mm_] := Log[mm^nn*binomial[nn, nn * (x + 1) / 2]]/nn;
rateMultiplier[x_, nn_, mm_, g_] := (1 + g * x) * entropy[0, nn, mm] / entropy[x, nn, mm];
rateMultiplier[x_, nn_, mm_] := rateMultiplier[x, nn, mm, gFactor];
rateMultiplierQuadratic[x_, a_, g_] := (1 + g * x) * (1 + a * x^2);

u00[e_, x_] := Exp[-x^2/e^2]/(e *Sqrt[Pi]* Erf[1/e]);

(* ============================================== *)

runFredholmSolver[noOfPoints_?IntegerQ, mORa_?NumericQ, e_?NumericQ, useQuadratic_?BooleanQ, g_?NumericQ] :=
  Module[{val, vec, k, kFunc, midGrid, weights, e1, e2, mu, sigma, l1Norm, ii, jj, retVal1},
   Print["runFredholmSolver::Starting - noOfPoints, = ", noOfPoints, ", mORa = ", mORa, ", e = ", N[e], ", g = ", N[g], ", useQuadratic = ", useQuadratic];

   kFunc[x_, y_] := Module[{xp, yp, ep, mp, retVal, np, ap, gp},
     xp = SetPrecision[x, fredholmPrecision];
     yp = SetPrecision[y, fredholmPrecision];
     ep = SetPrecision[e, fredholmPrecision];
     mp = SetPrecision[mORa, fredholmPrecision];
     np = SetPrecision[noOfMolecules, fredholmPrecision];
     ap = SetPrecision[mORa, fredholmPrecision];
     gp = SetPrecision[g, fredholmPrecision];

     retVal = SetPrecision[If[useQuadratic, (rateMultiplierQuadratic[yp, ap, gp]), (rateMultiplier[yp, np, mp, gp])] * delta1[xp, yp, ep],fredholmPrecision];

     Return[retVal];
     ];

   {val, vec, k} = SetPrecision[fredholmSolver[noOfPoints, domain, kFunc], MachinePrecision];
   {midGrid, weights} = SetPrecision[getMidGridAndWeights[noOfPoints, domain], MachinePrecision];
   {mu, sigma} = getMuSigma[noOfPoints, domain, vec];
   retVal1 = {val, vec, {mu, sigma}, k, {noOfPoints, mORa, e, useQuadratic, g}};
   Print["    runFredholmSolver::Completed - noOfPoints, = ", noOfPoints, ", mORa = ", mORa, ", e = ", N[e], ", g = ", N[g], ", useQuadratic = ", useQuadratic];
   Return[retVal1];
   ];

(* ============================================== *)

printResults[data_] :=
  Module[{val, vec, mu, sigma, k, noOfPoints, mORa, e, useQuadratic, midGrid, weights, g, e1, e2, l1Norm, ii, jj},
   {val, vec, {mu, sigma}, k, {noOfPoints, mORa, e, useQuadratic, g}} = data;
   Print[sep];
   Print["noOfPoints, = ", noOfPoints, ", mORa = ", mORa, ", e = ", N[e], ", g = ", N[g], ", useQuadratic = ", useQuadratic];
   {midGrid, weights} = getMidGridAndWeights[noOfPoints, domain];

   Print["val[[1]] - val[[2]] = ", (val[[1]] - val[[2]])];
   Print["val[[3]] - val[[4]] = ", (val[[3]] - val[[4]])];
   Print["val[[5]] - val[[6]] = ", (val[[5]] - val[[6]])];
   Print["val[[7]] - val[[8]] = ", (val[[7]] - val[[8]])];
   Print["val[[9]] - val[[10]] = ", (val[[9]] - val[[10]])];
   Print["val[[11]] - val[[12]] = ", (val[[11]] - val[[12]])];
   Print["val[[13]] - val[[14]] = ", (val[[13]] - val[[14]])];
   Print["val[[15]] - val[[16]] = ", (val[[15]] - val[[16]])];
   Print["val[[17]] - val[[18]] = ", (val[[17]] - val[[18]])];
   Print["val[[19]] - val[[20]] = ", (val[[19]] - val[[20]])];

   (*
   Print["midGrid and weights"];
   Print[ListLinePlot[midGrid, PlotRange -> All, Frame -> True, GridLines -> Automatic, ImageSize -> Large, PlotStyle -> Thickness[0.005], DataRange -> domain]];
   Print[ListLinePlot[weights, PlotRange -> All, Frame -> True, GridLines -> Automatic, ImageSize -> Large, PlotStyle -> Thickness[0.005], DataRange -> domain]];
   *)

   e1 = Re[vec[[1]]];
   e2 = Re[vec[[2]]];
   l1Norm = Table[Abs[Sum[Re[vec[[ii]][[jj]]]*weights[[jj]], {jj, 1, noOfPoints}]], {ii, 1, noOfPoints}];

   Print["L1 norms"];
   Print[ListLinePlot[Take[l1Norm, Min[maxNorms, noOfPoints]],
     PlotRange -> All, Frame -> True, GridLines -> Automatic,
     ImageSize -> Large, PlotStyle -> Thickness[0.005]]];

   Print[ListLinePlot[l1Norm, PlotRange -> All, Frame -> True,
     GridLines -> Automatic, ImageSize -> Large,
     PlotStyle -> Thickness[0.005]]];

   Print["Eigenvalues"];
   Print[ListLinePlot[Re[Take[val, Min[maxEigenValues, noOfPoints]]],
     PlotRange -> All, Frame -> True, GridLines -> Automatic,
     PlotRange -> {0, Automatic},
     FrameLabel -> {Style["i", FontSize -> tickFontSize],
       Style[Subscript["\[Lambda]", "i"], FontSize -> tickFontSize]},
     ImageSize -> Large, PlotStyle -> Thickness[0.005],
     FrameTicksStyle ->
      Directive @@ {tickFontSize, FontColor -> Black},
     LabelStyle -> Directive[Bold, Black]]];

   Print[ListLinePlot[Re[val], PlotRange -> All, Frame -> True,
     GridLines -> Automatic, PlotRange -> {0, Automatic},
     FrameLabel -> {Style["i", FontSize -> tickFontSize],
       Style[Subscript["\[Lambda]", "i"], FontSize -> tickFontSize]},
     ImageSize -> Large, PlotStyle -> Thickness[0.005],
     FrameTicksStyle ->
      Directive @@ {tickFontSize, FontColor -> Black},
     LabelStyle -> Directive[Bold, Black]]];

   Print["Eigenvectors"];
   (* u1 and u2 *)
   Print[ListLinePlot[{Table[{midGrid[[ii]], e1[[ii]]}, {ii, 1,
        noOfPoints}],
      Table[{midGrid[[ii]], e2[[ii]]}, {ii, 1, noOfPoints}]},
     PlotRange -> All, Frame -> True, GridLines -> Automatic,
     DataRange -> domain,
     FrameLabel -> {Style["\[Eta]", FontSize -> tickFontSize],
       Style["u", FontSize -> tickFontSize]}, ImageSize -> Large,
     PlotStyle -> Thickness[0.005],
     FrameTicksStyle ->
      Directive @@ {tickFontSize, FontColor -> Black},
     LabelStyle -> Directive[Bold, Black],
     PlotLegends ->
      Placed[{Style[Subscript["u", "1"], FontSize -> legendFontSize],
        Style[Subscript["u", "2"], FontSize -> legendFontSize]}, {0.7,
         0.7}]]];

   (* u1 *)
   Print[ListLinePlot[
     Table[{midGrid[[ii]], e1[[ii]]}, {ii, 1, noOfPoints}],
     PlotRange -> All, Frame -> True, GridLines -> Automatic,
     DataRange -> domain,
     FrameLabel -> {Style["\[Eta]", FontSize -> tickFontSize],
       Style[Subscript["u", "1"], FontSize -> tickFontSize]},
     ImageSize -> Large, PlotStyle -> Thickness[0.005],
     FrameTicksStyle ->
      Directive @@ {tickFontSize, FontColor -> Black},
     LabelStyle -> Directive[Bold, Black]]];

   (* u2 *)
   Print[ListLinePlot[
     Table[{midGrid[[ii]], e2[[ii]]}, {ii, 1, noOfPoints}],
     PlotRange -> All, Frame -> True, GridLines -> Automatic,
     DataRange -> domain,
     FrameLabel -> {Style["\[Eta]", FontSize -> tickFontSize],
       Style[Subscript["u", "2"], FontSize -> tickFontSize]},
     ImageSize -> Large, PlotStyle -> Thickness[0.005],
     FrameTicksStyle ->
      Directive @@ {tickFontSize, FontColor -> Black},
     LabelStyle -> Directive[Bold, Black]]];

   Return[];
   ];

(* ============================================== *)

renormalize[v_, w_] := Module[{retVal, norm, len, ii, e},
   len = Length[v];
   e = Re[v];
   norm = Sqrt[Sum[e[[ii]]^2*w[[ii]], {ii, 1, len}]];
   retVal = e/norm;
   Return[retVal];
   ];

(* ============================================== *)
