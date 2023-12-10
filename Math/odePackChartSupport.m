(* Constants and constant-like data functions *)

ffmpegFolder = "C:\\FFMpeg\\bin";
workingFolder = "C:\\EeInf";
animationExtension = ".avi";
getListPlotOptions3D[resolution_]:= { ImageSize -> resolution, PlotTheme -> {"Classic", "ClassicLights"}, AxesLabel -> {"\[Eta]", "\[Zeta]", "u"}, PlotRange -> All, LabelStyle -> {FontSize -> 16, Bold, Black} };

(* ========================================= *)
(* Location of .m frame files. *)
dataFolder := workingFolder <> "\\Data"

(* Location of .png frame files. *)
framesFolder := workingFolder <> "\\Frames"

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

(* ==================================================== *)

createFolder[folderName_?StringQ]:= If[Not[DirectoryQ[folderName]], CreateDirectory[folderName]];

getImagePadding[g_] :=
  Module[{options, imagePadding, retVal},
   options = AbsoluteOptions[g, ImagePadding];
   imagePadding = ImagePadding /. options;
   retVal = If[imagePadding === Automatic || imagePadding === ImagePadding, {0, 0}, {First[First[imagePadding]], First[Last[imagePadding]]}];
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

exportPngFile[fullDataFileName_, resolution_, xPadding_, yPadding_] :=
  Module[{frameData, fileNameWithExtension, fullFramesFileName, options},
   fileNameWithExtension = FileNameTake[fullDataFileName];
   fullFramesFileName = FileNameJoin[{framesFolder, StringReplace[fileNameWithExtension, ".m" -> ".png"]}];
   options = Join[{ ImagePadding -> {{xPadding, 0}, {yPadding, 0}} }, getListPlotOptions3D[resolution]];
   Print["Exporting: ", fullDataFileName, " into : ", fullFramesFileName, ". Memory used: ", Round[(MemoryInUse[] - startMemory)/10^9, 0.001], "GB, time taken: ", Round[AbsoluteTime[] - startTime], " seconds."];
   frameData = Import[fullDataFileName];
   Export[fullFramesFileName, ListPlot3D[frameData, Sequence @@ options], "PNG"];
   Return[0];
];

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

