(* ::Package:: *)

(* Constants and constant-like data functions *)

ffmpegFolder := "C:\\FFMpeg\\bin";
workingFolder := "C:\\EeInf";
animationExtension := ".mp4";
xPaddingValue := 70.05;
yPaddingValue := 20.45;
yScale := 0.01;
getListPlotOptions3D[resolution_]:= { ImageSize -> resolution, PlotTheme -> {"Classic", "ClassicLights"}, AxesLabel -> {"\[Eta]", "\[Zeta]", "u"}, PlotRange -> All, LabelStyle -> {FontSize -> 16, Bold, Black} };

(* ========================================= *)

extractScriptName[descr_String] := Module[{matches, retVal},
    matches = StringCases[descr, "name = Some \"" ~~ Shortest[name__] ~~ "\"" :> name];
    matches = If[Length[matches] > 0, matches, StringCases[descr, "name = \"" ~~ Shortest[name__] ~~ "\"" :> name]];
    retVal = If[Length[matches] > 0, First[matches], Indeterminate];
    Return[retVal];
];

(* ========================================= *)

(* Location of .m frame files. *)
dataFolder := "C:\\EeInf\\Data";

(* Location of .png frame files. *)
framesFolder := workingFolder <> "\\Frames";

ffmpegExecutable := FileNameJoin[{ffmpegFolder, "ffmpeg"}];

startTime = AbsoluteTime[];
startMemory = MemoryInUse[];

(* ========================================= *)

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

exportListPlot3D[x_, y_, z_, zName_, suffix_] :=
  Module[{xLen, yLen, data, scriptName, fullOutputFileName},
    xLen = Length[x];
    yLen = Length[y];
    data = Flatten[Table[{x[[ii]], y[[jj]], z[[ii, jj]]}, {ii, 1, xLen}, {jj, 1, yLen}], 1];
    scriptName = extractScriptName[descr];
    Print["scriptName: ", scriptName];
    fullOutputFileName = FileNameJoin[{ framesFolder, scriptName <> "__" <> suffix <> ".png" }];
    Print["Exporting: ", fullOutputFileName, ". Memory used: ", Round[(MemoryInUse[] - startMemory)/10^9, 0.001], "GB, time taken: ", Round[AbsoluteTime[] - startTime], " seconds."];
    Export[fullOutputFileName, ListPlot3D[data, PlotTheme -> {"Classic", "ClassicLights"}, AxesLabel -> {"\[Eta]", "\[Zeta]", zName}, PlotRange -> All, LabelStyle -> {FontSize -> 16, Bold, Black}, ImageSize -> Large], "PNG"];
    Return[0];
];

getData[d_, colIdx_, divisor_] := Module[{xVal, xLen, data},
    xVal =  Transpose[d][[1]];
    xLen = Floor[ Length[xVal] / divisor];
    data = Table[{xVal[[ii]],d[[ii,colIdx]]},{ii,1,xLen}];
    Return[data];
];

plotChart[d_, colIdx_, yName_, divisor_] := Module[{data},
    Print[yName];
    data = getData[d, colIdx, divisor];
    (* data = Table[{xVal[[ii]], d[[ii, colIdx]]}, {ii, Length[xVal] - xLen + 1, Length[xVal]}]; *)
    Print[ListPlot[data, FrameLabel -> {{yName, None}, {"t", None}}, PlotRange -> All, Frame -> True, GridLines -> Automatic, Joined -> True, LabelStyle -> {FontSize -> 16, Bold, Black}, ImageSize -> Large, PlotStyle -> {Thickness[0.005]}]];
    Print[sep];
    Print[""];
];

getLineStyle[lineNumber_, totalLines_] := Module[{colors, baseStyle},
    colors = {Black, Red, Green, Blue}; (* Very distinct colors *)
    baseStyle = If[totalLines > 4 && lineNumber > 4, Dashed, Solid]; (* Determine base style based on line number and total lines *)

    (* Ensure color cycles through the list without causing Mod[0,0] *)
    (* Mod with Length[colors] ensures cycling through colors without error *)
    {colors[[Mod[lineNumber - 1, Length[colors]] + 1]], baseStyle}
];

plotCombinedChart[d_, colIdx_, yName_, divisor_, shift_:{}, plotRange_:All] := Module[
    {len, data, plotStyles, legendOptions, legendPlacement},

    Print[yName];
    len = Length[d];
    data = Table[getData[d[[ii]], colIdx, divisor], {ii, 1, len}];

    plotStyles = Table[getLineStyle[ii, len], {ii, 1, len}];

    (* Determine legend placement based on shift parameter *)
    legendPlacement = Switch[Length[shift],
        0, Right, (* Legend outside the chart by default *)
        1, {shift[[1]], 0.75}, (* If one parameter is provided, use it as x shift *)
        2, shift, (* If two parameters are provided, use them as x and y shifts *)
        _, {0.75, 0.75} (* Default to {0.75, 0.75} if the list has an unexpected length *)
    ];

    legendOptions = If[Length[shift] > 0,
        Placed[LineLegend[Automatic, Table[ToString[i], {i, 1, Length[data]}], LegendMarkerSize -> {15, 3}], legendPlacement],
        LineLegend[Automatic, Table[ToString[i], {i, 1, Length[data]}], LegendMarkerSize -> {15, 3}]
    ];

    (* Generate and print the plot with the configured legend options *)
    Print[ListPlot[data,
      PlotLegends -> legendOptions,
      FrameLabel -> {{yName, None}, {"t", None}},
      PlotRange -> plotRange, Frame -> True, GridLines -> Automatic,
      Joined -> True, LabelStyle -> {FontSize -> 16, Bold, Black},
      ImageSize -> Large,
      PlotStyle -> Table[{plotStyles[[ii, 1]], plotStyles[[ii, 2]], Thickness[0.005]}, {ii, 1, len}]
    ]];
    Print[sep];
    Print[""];
];

(* Set yScale before usage. *)
plotCombinedChartScaled[d_, colIdx_, yName_, divisor_, shift_:{}] := Module[
    {len, data, plotStyles, legendOptions, legendPlacement},

    Print[yName];
    len = Length[d];
    data = Table[getData[d[[ii]], colIdx, divisor], {ii, 1, len}];

    plotStyles = Table[getLineStyle[ii, len], {ii, 1, len}];

    (* Determine legend placement based on shift parameter *)
    legendPlacement = Switch[Length[shift],
        0, Right, (* Legend outside the chart by default *)
        1, {shift[[1]], 0.75}, (* If one parameter is provided, use it as x shift *)
        2, shift, (* If two parameters are provided, use them as x and y shifts *)
        _, {0.75, 0.75} (* Default to {0.75, 0.75} if the list has an unexpected length *)
    ];

    legendOptions = If[Length[shift] > 0,
        Placed[LineLegend[Automatic, Table[ToString[i], {i, 1, Length[data]}], LegendMarkerSize -> {15, 3}], legendPlacement],
        LineLegend[Automatic, Table[ToString[i], {i, 1, Length[data]}], LegendMarkerSize -> {15, 3}]
    ];

    (* Generate and print the plot with the configured legend options *)
    Print[ListPlot[data,
      PlotLegends -> legendOptions,
      FrameLabel -> {{yName, None}, {"t", None}},
      PlotRange -> {Automatic, yScale}, Frame -> True, GridLines -> Automatic,
      Joined -> True, LabelStyle -> {FontSize -> 16, Bold, Black},
      ImageSize -> Large,
      PlotStyle -> Table[{plotStyles[[ii, 1]], plotStyles[[ii, 2]], Thickness[0.005]}, {ii, 1, len}]
    ]];
    Print[sep];
    Print[""];
];

exportListPlot[d_, colIdx_, yName_, divisor_, suffix_] :=
  Module[{xVal, xLen, data, scriptName, fullOutputFileName},
    xVal =  Transpose[d][[1]];
    xLen = Floor[ Length[xVal] / divisor];
    data = Table[{xVal[[ii]],d[[ii,colIdx]]},{ii,1,xLen}];
    scriptName = extractScriptName[descr];
    Print["scriptName: ", scriptName];
    fullOutputFileName = FileNameJoin[{ framesFolder, scriptName <> "__" <> suffix <> ".png" }];
    Print["Exporting: ", fullOutputFileName, ". Memory used: ", Round[(MemoryInUse[] - startMemory)/10^9, 0.001], "GB, time taken: ", Round[AbsoluteTime[] - startTime], " seconds."];
    Export[fullOutputFileName, ListPlot[data, FrameLabel -> {{yName, None}, {"t", None}}, PlotRange -> All, Frame -> True, GridLines -> Automatic, Joined -> True, LabelStyle -> {FontSize -> 16, Bold, Black}, ImageSize -> Large, PlotStyle -> {Thickness[0.005]}], "PNG"];
    Return[0];
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

exportSliceU[d_] := Module[{t, u, suffix},
    t = d[[1]];
    Print["t = ", t];
    u = d[[2]];
    suffix = "u__t_" <> StringPadLeft[ToString[t], 6, "_"];
    exportListPlot3D[etaData, zetaData, Chop[u], "u", suffix];
];

exportUT[ut_] := Module[{len},
    len = Length[ut];
    Table[exportSliceU[ut[[ii]]], {ii, 1, len}];
];

plotAll[divisor_] := Module[{xLen, yLen, kaDivGamma},
    Print["descr = ", descr];
    Print[sep];

    xLen = Length[etaData];
    yLen = Length[zetaData];
    kaDivGamma = Table[ka[[ii, jj]] / gamma[[ii, jj]], {ii, 1, xLen}, {jj, 1, yLen}];

    plot3DChart[etaData, zetaData, ka, Subscript["k", "a"], "k0 = " <> ToString[k0]];
    exportListPlot3D[etaData, zetaData, ka, Subscript["k", "a"], "ka"];

    plot3DChart[etaData, zetaData, gamma, "\[Gamma]", "gamma0 = " <> ToString[gamma0]];
    exportListPlot3D[etaData, zetaData, gamma, "\[Gamma]", "gamma"];

    plot3DChart[etaData, zetaData, kaDivGamma, Subscript["k", "a"] / "\[Gamma]", "k0 = " <> ToString[k0] <> ", gamma0 = " <> ToString[gamma0]];

    plot3DChart[etaData, zetaData, uData, "u", "u (as is)"];
    exportListPlot3D[etaData, zetaData, uData, "u", "u"];

    plot3DChart[etaData, zetaData, Chop[uData], "u", "u (chopped)"];

    plotChart[chartData, 2, Subscript["\[Mu]", "\[Eta]"], divisor];
    exportListPlot[chartData, 2, Subscript["\[Mu]", "\[Eta]"], divisor, "mu_eta"];

    plotChart[chartData, 3, Subscript["\[Sigma]", "\[Eta]"], divisor];
    exportListPlot[chartData, 3, Subscript["\[Sigma]", "\[Eta]"], divisor, "sigma_eta"];

    plotChart[chartData, 4, Subscript["\[Mu]", "\[Zeta]"], divisor];
    exportListPlot[chartData, 4, Subscript["\[Mu]", "\[Zeta]"], divisor, "mu_zeta"];

    plotChart[chartData, 5, Subscript["\[Sigma]", "\[Zeta]"], divisor];
    exportListPlot[chartData, 5, Subscript["\[Sigma]", "\[Zeta]"], divisor, "sigma_zeta"];

    plotChart[chartData, 6, "I", divisor];

    plotChart[chartData, 7, Subscript["u", "total"], divisor, ""];
    exportListPlot[chartData, 7, Subscript["u", "total"], divisor, "u_total"];

    plotChart[chartData, 8, "food", divisor];
    exportListPlot[chartData, 8, "food", divisor, "food"];

    plotChart[chartData, 9, "waste", divisor];
    exportListPlot[chartData, 9, "waste", divisor, "waste"];

    Print[sep];
    Print[sep];

    (* plotUT[uDataT]; *)
    exportUT[uDataT];
];

(* ==================================================== *)

createFolder[folderName_?StringQ]:= If[Not[DirectoryQ[folderName]], CreateDirectory[folderName]];

getImagePadding[g_] :=
  Module[{options, imagePadding, retVal},
   options = AbsoluteOptions[g, ImagePadding];
   imagePadding = ImagePadding /. options;
   retVal = If[imagePadding === All || imagePadding === Automatic || imagePadding === ImagePadding, {0, 0}, {First[First[imagePadding]], First[Last[imagePadding]]}];
   Return[retVal];
];

getFilePadding[fullDataFileName_, resolution_] :=
  Module[{retVal, frameData, chart, options},
   Print["Loading file: ", fullDataFileName, ". Memory used: ", Round[(MemoryInUse[] - startMemory)/10^9, 0.001], "GB, time taken: ", Round[AbsoluteTime[] - startTime], " seconds."];
   frameData = Import[fullDataFileName];
   options = getListPlotOptions3D[resolution];
   chart = ListPlot3D[frameData, Sequence @@ options];
   retVal = getImagePadding[chart];
   Return[retVal];
   ];

getExportFileName[fullDataFileName_] := Module[{fileNameWithExtension, fullFramesFileName},
    fileNameWithExtension = FileNameTake[fullDataFileName];
    fullFramesFileName = FileNameJoin[{framesFolder, StringReplace[fileNameWithExtension, ".m" -> ".png"]}];
    Return[fullFramesFileName];
];

exportPngFile[fullDataFileName_, resolution_, xPadding_, yPadding_] :=
  Module[{frameData, fullFramesFileName, options},
   fullFramesFileName = getExportFileName[fullDataFileName];
   options = Join[{ ImagePadding -> {{xPadding, 0}, {yPadding, 0}} }, getListPlotOptions3D[resolution]];
   Print["Exporting: ", fullDataFileName, " into : ", fullFramesFileName, ". Memory used: ", Round[(MemoryInUse[] - startMemory)/10^9, 0.001], "GB, time taken: ", Round[AbsoluteTime[] - startTime], " seconds."];
   frameData = Import[fullDataFileName];
   Export[fullFramesFileName, ListPlot3D[frameData, Sequence @@ options], "PNG"];
   Return[0];
];

exportPngFile[fullDataFileName_, resolution_] := exportPngFile[fullDataFileName, resolution, xPaddingValue, yPaddingValue];

createAnimation[filePrefix_, resolution_, duration_] :=
  Module[{files, frames, animation, frameData, endTime, endMemory, memoryUsed, timeTaken, paddings, xPadding, yPadding, noOfFiles, frameRate, ffmpegCommand, result, concatFileName, filesList, fileName, outputFile},
   Print["Starting..."];
   createFolder[dataFolder];
   createFolder[framesFolder];

   files = FileNames[FileNameJoin[{dataFolder, filePrefix <> "*.m"}]];
   (* files=Take[Sort[files],20]; *)
   files = Sort[files];
   noOfFiles = Length[files];
   frameRate = N[noOfFiles/duration];
   Print["Duration; ", duration, ", frame rate: ", frameRate, "."];

   Print["Loading ", noOfFiles, " files..."];
   paddings = Table[getFilePadding[fileName, resolution], {fileName, files}];
   xPadding = Max[Table[paddings[[i, 1]], {i, 1, noOfFiles}]];
   yPadding = Max[Table[paddings[[i, 2]], {i, 1, noOfFiles}]];
   Print["xPadding = ", xPadding, ", yPadding = ", yPadding];

   Print["Exporting ", noOfFiles, " files..."];
   Table[exportPngFile[fileName, resolution, xPadding, yPadding], {fileName, files}];
   Print["Exported ", noOfFiles, " frames."];

   concatFileName = FileNameJoin[{workingFolder, filePrefix <> "concat.txt"}];
   filesList = Sort[FileNames[FileNameJoin[{framesFolder, filePrefix <> "*.png"}]]];
   Print["filesList = ", filesList]
   Export[concatFileName, "ffconcat version 1.0\n" <> StringJoin["file " <> StringReplace[#, "\\" -> "\\\\"] <> "\n" & /@ filesList], "Text"];
   outputFile = FileNameJoin[{workingFolder, filePrefix <> "animation" <> animationExtension}];
   ffmpegCommand = StringJoin[ffmpegExecutable, " -y -f concat -safe 0 -r ", ToString[frameRate], " -i ", concatFileName, " -framerate ", ToString[frameRate], " ", outputFile];

   Print["Running ffmpeg with command: ", ffmpegCommand];
   result = RunProcess[{"cmd", "/c", ffmpegCommand}];
   Print["result = ", result];

   endTime = AbsoluteTime[];
   endMemory = MemoryInUse[];
   timeTaken = Round[endTime - startTime];
   memoryUsed = Round[(endMemory - startMemory)/10^9, 0.001];
   Print["Time taken: ", timeTaken, " seconds."];
   Print["Memory used: ", memoryUsed, " GB."];
   Print["xPadding = ", xPadding, ", yPadding = ", yPadding];
   Print["Exported animation."];
];

getFiles[filePrefix_] := Module[{files},
    files = FileNames[FileNameJoin[{dataFolder, filePrefix <> "*.m"}]];
    files = Sort[files];
    Return[files];
];

(* Same as above but does not calculate padding first. *)
(* This is faster but could result in some jerkiness in the video. *)
(* This also does not rewrite PNG files. So, if you restart it, then it will not process already existing files. *)
createAnimationQuick[filePrefix_, resolution_, duration_] :=
  Module[{files, frames, animation, frameData, endTime, endMemory, memoryUsed, timeTaken, paddings, xPadding, yPadding, noOfFiles, frameRate, ffmpegCommand, result, concatFileName, filesList, fileName, outputFile},
   Print["Starting for file prefix \"", filePrefix, "\" ..."];
   createFolder[dataFolder];
   createFolder[framesFolder];

   files = getFiles[filePrefix];
   noOfFiles = Length[files];
   frameRate = N[noOfFiles/duration];
   Print["Duration; ", duration, ", frame rate: ", frameRate, "."];

   Print["Exporting ", noOfFiles, " files..."];
   Table[If[FileExistsQ[getExportFileName[fileName]], Print["File: ", fileName, " was already exported."], exportPngFile[fileName, resolution]], {fileName, files}];
   Print["Exported ", noOfFiles, " frames."];

   concatFileName = FileNameJoin[{workingFolder, filePrefix <> "concat.txt"}];
   filesList = Sort[FileNames[FileNameJoin[{framesFolder, filePrefix <> "*.png"}]]];
   Print["filesList = ", filesList]
   Export[concatFileName, "ffconcat version 1.0\n" <> StringJoin["file " <> StringReplace[#, "\\" -> "\\\\"] <> "\n" & /@ filesList], "Text"];
   outputFile = FileNameJoin[{workingFolder, filePrefix <> "animation" <> animationExtension}];
   ffmpegCommand = StringJoin[ffmpegExecutable, " -y -f concat -safe 0 -r ", ToString[frameRate], " -i ", concatFileName, " -framerate ", ToString[frameRate], " ", outputFile];

   Print["Running ffmpeg with command: ", ffmpegCommand];
   result = RunProcess[{"cmd", "/c", ffmpegCommand}];
   Print["result = ", result];

   endTime = AbsoluteTime[];
   endMemory = MemoryInUse[];
   timeTaken = Round[endTime - startTime];
   memoryUsed = Round[(endMemory - startMemory)/10^9, 0.001];
   Print["Time taken: ", timeTaken, " seconds."];
   Print["Memory used: ", memoryUsed, " GB."];
   Print["Exported animation: \"", outputFile, "\"."];
];

createAllAnimationsQuick[] := Module[{files, fileNames, prefixes, uniquePrefixes, uniquePrefixesWithNoOfFiles},
    Print["Processing files in: \"", dataFolder, "\" folder."];
    files = FileNames[FileNameJoin[{dataFolder, "*.m"}]];
    fileNames = FileNameTake /@ files;
    Print["Found: ", Length[fileNames], " files."];

    prefixes = StringJoin[Riffle[Take[StringSplit[#, {"__"}], 2], "__"]] & /@ fileNames;
    uniquePrefixes = Sort[DeleteDuplicates[prefixes]];
    uniquePrefixesWithNoOfFiles = Table[{prefix, Length[getFiles[prefix]]}, {prefix, uniquePrefixes}];
    Print["Found: ", Length[uniquePrefixes], " prefixes."];
    Print[uniquePrefixesWithNoOfFiles // MatrixForm];

    Do[createAnimationQuick[prefix <> "__", "Large", 50], {prefix, uniquePrefixes}];
];
