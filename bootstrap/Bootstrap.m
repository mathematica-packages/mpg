Print[
"Mpg Bootstrapper 0.1
********************"
];

(* If user sets $MpgDirectory before running the bootstrapper, the
   bootstrapper will honor their setting, and preserve it over time. *)
If[!ValueQ[$MpgDirectory],$MpgDirectory = FileNameJoin[{$UserBaseDirectory,"Mpg"}]];

(* Check for already installed Mpg *)
Print["Checking for existing Mpg installation."];

Catch[

If[Quiet[Needs["Mpg`"]] =!= $Failed, Print["Yippee! You already have Mpg."]; Throw[""]];

(* Download if not *)
Print["Downloading latest Mpg package."];

(* TODO: For Mathematica 9 and up, should use the built in URLSave.
         Meanwhile, JLink on all versions for back compat. *)
Module[{getURL,
        safeDirectoryReplace,
        tempZip,
        zipFiles,
        mpgPackageUrl = "https://github.com/mathematica-packages/mpg/zipball/stable",
        archiveDirectory},
  Needs["JLink`"];
  getURL[url_String, directory_String] :=
   JLink`JavaBlock[
    Module[{u, conn, filenameHeader, outFileName, stream, numRead,
      outFile, buf}, JLink`InstallJava[];
     u = JLink`JavaNew["java.net.URL", url];
     conn = u@openConnection[];
     If[conn === $Failed, Return[$Failed]];
     filenameHeader = conn@getHeaderField["Content-Disposition"];
     If[! FileExistsQ[directory], CreateDirectory[directory]];
     outFileName =
      FileNameJoin[{directory,
        StringCases[filenameHeader, "filename=" ~~ p : __ :> p][[1]]}];
     stream = conn@getInputStream[];
     If[stream === $Failed, Return[$Failed]];
     outFile = OpenWrite[outFileName, BinaryFormat -> True];
     buf = JLink`JavaNew["[B",
       5000];(*5000 is an arbitrary buffer size.*)
     While[(numRead = stream@read[buf]) > 0,
      BinaryWrite[outFile, Take[Mod[JLink`Val[buf], 256], numRead]]];
     stream@close[];
     Close[outFile]   (*Close returns the filename.*)]];

  safeDirectoryReplace[from_,to_] :=
    Module[{tempSuffix = ".tmp" <> ToString[RandomInteger[100000000]]},
    If[!FileExistsQ[from], Message[safeDirectoryReplace::missing],
     If[FileExistsQ[to], RenameDirectory[to, to <> tempSuffix];
      CopyDirectory[from, to]; DeleteDirectory[to <> tempSuffix,DeleteContents->True]; to,
      CopyDirectory[from, to]]]];

  tempZip = getURL[mpgPackageUrl,
    FileNameJoin[{$MpgDirectory, "Cache"}]];

  If[tempZip === $Failed,Print["Failed to fetch latest Mpg package from "<> mpgPackageUrl <>"."]];

  (* This only works in Mathematica 8.0 and up. Need to consider
     our options for downlevel here... *)
  archiveDirectory = ExtractArchive[tempZip,FileNameJoin[{$MpgDirectory,"Cache"}]][[1]];
  metaData = Import[FileNameJoin[{archiveDirectory,"MpgMeta.m"}]];
  safeDirectoryReplace[FileNameJoin[{archiveDirectory,"Mpg"}],FileNameJoin[{$MpgDirectory,"Active","Mpg"}]];
  DeleteDirectory[archiveDirectory,DeleteContents->True];
];

(* Put path settings in init.m *)
Print["Updating Kernel/init.m with Mpg locations."];

Module[{mpgDirString,
        mpgLoaderString,
        safeFileReplace,
        writeTempFile,
        breakExpressions,
        initFileName,
        initLines,
        initExpressions,
        tempName},

  If[$MpgDirectory===FileNameJoin[{$UserBaseDirectory,"Mpg"}],
    mpgDirString="FileNameJoin[{$UserBaseDirectory,\"Mpg\"}]",
    mpgDirString="\""<>StringReplace[$MpgDirectory,"\\"->"\\\\"]<>"\""];
  mpgLoaderString = "Module[{mpgLoader},
(* This module is managed by Mpg.
   Edits may get silently overwritten. *)
"<>"$MpgDirectory = " <> mpgDirString <> ";
AppendTo[$Path,FileNameJoin[{$MpgDirectory,\"Active\"}]];
]
";
  safeFileReplace[from_, to_] :=
   Module[{tempSuffix = ".tmp" <> ToString[RandomInteger[100000000]]},
    If[! FileExistsQ[from], Message[safeFileReplace::missing],
     If[FileExistsQ[to], RenameFile[to, to <> tempSuffix];
      RenameFile[from, to]; DeleteFile[to <> tempSuffix]; to,
      RenameFile[from, to]]]];

  writeTempFile[dir_,str_] :=
   Module[{tempName = FileNameJoin[{dir,"tmp" <> ToString[RandomInteger[100000000]]}],tempStream},
     tempStream = OpenWrite[tempName];
     WriteString[tempStream,str];
     Close[tempStream];
     tempName];

  breakExpressions[lines_, exprs_: {}, expr_: ""] /; Length[lines] == 0 :=
   If[expr == "", Flatten[exprs], Flatten[{exprs, expr}]];

  breakExpressions[lines0_, exprs_: {}, expr0_: ""] :=
    With[{expr = expr0 <> First[lines0] <> "\n", lines = Rest[lines0]},
     If[MatchQ[
     Quiet[
       Check[ToExpression[expr, InputForm, Hold],
        Null,
        {Syntax::com, ToExpression::sntx, ToExpression::sntxi}],
      {Syntax::com, ToExpression::sntx, ToExpression::sntxi}],
     Null],
     breakExpressions[lines, exprs, expr],
     breakExpressions[lines, {exprs, expr}, ""]]];

  initFileName = FileNameJoin[{$UserBaseDirectory, "Kernel", "init.m"}];

  initLines = ReadList[initFileName, String,
   NullRecords -> True];

  initExpressions = breakExpressions[initLines];

  initExpressions = Select[initExpressions,
   !MatchQ[ToExpression[#, InputForm, Hold],
    Hold[Module[{mpgLoader}, ___]]] &];

  tempName = writeTempFile[DirectoryName[initFileName],StringJoin[initExpressions,mpgLoaderString]];

  safeFileReplace[tempName,initFileName];

];

];

Print["
Mpg successfully bootstrapped.
Run Mpg`Help[] for more info."];
