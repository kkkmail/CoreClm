sep = "=========================================";

plot3DChart[x_, y_, z_, zName_, descr_] := Module[{xLen, yLen, data},
    xLen = Length[x];
    yLen = Length[y];
    data = Flatten[Table[{x[[ii]], y[[jj]], z[[ii, jj]]}, {ii, 1, xLen}, {jj, 1, yLen}], 1];
    Print[descr];
    Print[ListPlot3D[data, PlotTheme -> {"Classic", "ClassicLights"}, AxesLabel -> {"\[Eta]", "\[Zeta]", zName}, PlotRange -> All, LabelStyle -> {FontSize -> 16, Bold, Black}, ImageSize -> Large]];
    Print[sep];
    Print[""];
];

plotChart[d_, colIdx_, yName_, divisor_] := Module[{xVal, xLen, data},
    Print[yName];
    xVal =  Transpose[d][[1]];
    xLen = Floor[ Length[xVal] / divisor];
    data=Table[{xVal[[ii]],d[[ii,colIdx]]},{ii,1,xLen}];
    (* data = Table[{xVal[[ii]], d[[ii, colIdx]]}, {ii, Length[xVal] - xLen + 1, Length[xVal]}]; *)
    Print[ListPlot[data, FrameLabel -> {{yName, None}, {"t", None}}, PlotRange -> All, Frame -> True, GridLines -> Automatic, Joined -> True, LabelStyle -> {FontSize -> 16, Bold, Black}, ImageSize -> Large, PlotStyle -> {Thickness[0.005]}]];
    Print[sep];
    Print[""];
];

plotSliceU[d_] := Module[{t, u},
    t = d[[1]];
    Print["t = ", t];
    u = d[[2]];
    (* plot3DChart[etaData, zetaData, Chop[u], "u(t = " <> ToString[t] <> ")", "t = " <> ToString[t]]; *)
    plot3DChart[etaData, zetaData, Chop[u], "u", "t = " <> ToString[t]];
];

plotUT[ut_] := Module[{len},
    len = Length[ut];
    Table[plotSliceU[ut[[ii]]], {ii, 1, len}];
];

plotAll[divisor_] := Module[{xLen, yLen, kaDivGamma},
    Print["descr = ", descr];
    Print[sep];

    xLen = Length[etaData];
    yLen = Length[zetaData];
    kaDivGamma = Table[ka[[ii, jj]] / gamma[[ii, jj]], {ii, 1, xLen}, {jj, 1, yLen}];

    plot3DChart[etaData, zetaData, ka, Subscript["k", "a"], "k0 = " <> ToString[k0]];
    plot3DChart[etaData, zetaData, gamma, "\[Gamma]", "gamma0 = " <> ToString[gamma0]];
    plot3DChart[etaData, zetaData, kaDivGamma, Subscript["k", "a"] / "\[Gamma]", "k0 = " <> ToString[k0] <> ", gamma0 = " <> ToString[gamma0]];
    plot3DChart[etaData, zetaData, uData, "u", "u (as is)"];
    plot3DChart[etaData, zetaData, Chop[uData], "u", "u (chopped)"];

    plotChart[chartData, 2, Subscript["\[Mu]", "\[Eta]"], divisor];
    plotChart[chartData, 3, Subscript["\[Sigma]", "\[Eta]"], divisor];
    plotChart[chartData, 4, Subscript["\[Mu]", "\[Zeta]"], divisor];
    plotChart[chartData, 5, Subscript["\[Sigma]", "\[Zeta]"], divisor];
    plotChart[chartData, 6, "I", divisor];
    plotChart[chartData, 7, Subscript["u", "total"], divisor];
    plotChart[chartData, 8, "food", divisor];
    plotChart[chartData, 9, "waste", divisor];
    Print[sep];
    Print[sep];

    plotUT[uDataT];
];
