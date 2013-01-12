(* Check for already installed MPG *)

(* Download if not *)

(* Put path settings in init.m *)

Module[{breakExpressions,safeFileReplace,writeTempFile,initFileName,initLines,initExpressions,tempName},
  mpgLoaderString = "Module[{mpgLoader},
(* This module is managed by MPG--edits get silently overwritten. *)
Print[\"MPG hooks installed.\"]]
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
       {ToExpression::sntxi, ToExpression::sntx}],
       {ToExpression::sntx, ToExpression::sntxi}],
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

]