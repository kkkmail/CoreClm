sep = "=========================================";

plot3DChart[x_, y_, z_, zName_] := Module[{xLen, yLen, data},
    xLen = Length[x];
    yLen = Length[y];
    data = Flatten[Table[{x[[ii]], y[[jj]], z[[ii, jj]]}, {ii, 1, xLen}, {jj, 1, yLen}], 1];
    Print[ListPlot3D[data, PlotTheme -> {"Classic", "ClassicLights"}, AxesLabel -> {"\[Eta]", "\[Zeta]", zName}, PlotRange -> All]];
    Print[sep];
    Print[""];
];

plotChart[d_, colIdx_, yName_, divisor_] := Module[{xVal, xLen, data},
    Print[yName];
    xVal =  Transpose[d][[1]];
    xLen = Floor[ Length[xVal] / divisor];
    data=Table[{xVal[[ii]],d[[ii,colIdx]]},{ii,1,xLen}];
    (* data = Table[{xVal[[ii]], d[[ii, colIdx]]}, {ii, Length[xVal] - xLen + 1, Length[xVal]}]; *)
    Print[ListPlot[data, AxesLabel -> {"y", yName}, PlotRange -> All, Frame -> True, GridLines -> Automatic, Joined -> True]];
    Print[sep];
    Print[""];
];

plotSliceU[d_] := Module[{t, u},
    t = d[[1]];
    Print["t = ", t];
    u = d[[2]];
    (* plot3DChart[etaData, zetaData, Chop[u], "u(t = " <> ToString[t] <> ")"]; *)
    plot3DChart[etaData, zetaData, Chop[u], "u"];
];

plotUT[ut_] := Module[{len},
    len = Length[ut];
    Table[plotSliceU[ut[[ii]]], {ii, 1, len}];
];

plotAll[divisor_] := Module[{},
    Print["descr = ", descr];
    Print[sep];

    plot3DChart[etaData, zetaData, ka, Subscript["k", "a"]];
    plot3DChart[etaData, zetaData, gamma, "\[Gamma]"];
    plot3DChart[etaData, zetaData, uData, "u"];
    plot3DChart[etaData, zetaData, Chop[uData], "u"];

    plotChart[chartData, 2, "eeMean", divisor];
    plotChart[chartData, 3, "eeStdDev", divisor];
    plotChart[chartData, 4, "infMean", divisor];
    plotChart[chartData, 5, "infStdDev", divisor];
    plotChart[chartData, 6, "invariant", divisor];
    plotChart[chartData, 7, "total", divisor];
    plotChart[chartData, 8, "food", divisor];
    plotChart[chartData, 9, "waste", divisor];
    Print[sep];
    Print[sep];

    plotUT[uDataT];
];
