(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA, see http://wlplugin.halirutan.de/ *)

(* :Title: MathematicaInformationTools *)
(* :Context: MathematicaInformationTools` *)
(* :Author: Patrick Scheibe *)
(* :Date: 2018-09-30 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2018 Patrick Scheibe *)
(* :Keywords: *)
(* :Discussion: *)

Package["IntelliJTools`"]
PackageImport["JLink`"]

JLink`LoadJavaClass["de.halirutan.wlintellij.Utils"];

$publicUnicodeArea = Join[Range[16^^E000 - 1], Range[16^^F8FF + 1, 16^^FFFF]];

ClearAll[
  $packageMacros,
  $contexts,
  $builtInNames,
  $existingSymbolVersions,
  $allNames,
  $additionalSymbols
];

getContextNames[context_String] := Block[{$ContextPath = {context}},
  StringJoin[context, #]& /@ Names[RegularExpression[context <> "\$?[A-Z]\\w*"]]
];
getStrippedContextNames[context_String] := Block[{$ContextPath = {context}},
  Names[RegularExpression[context <> "\$?[A-Z]\\w*"]]
];

$packageMacros = {
  "Package",
  "PackageExport",
  "PackageScope",
  "PackageImport"
};

$builtInNamedCharacters = {"\\[Degree]", "\\[Pi]", "\\[Infinity]", "\\[SystemsModelDelay]",
  "\\[SpanFromLeft]", "\\[SpanFromAbove]", "\\[SpanFromBoth]",
  "\\[ExponentialE]", "\\[ImaginaryI]", "\\[ImaginaryJ]",
  "\\[FormalA]", "\\[FormalB]", "\\[FormalC]", "\\[FormalD]",
  "\\[FormalE]", "\\[FormalF]", "\\[FormalG]", "\\[FormalH]",
  "\\[FormalI]", "\\[FormalJ]", "\\[FormalK]", "\\[FormalL]",
  "\\[FormalM]", "\\[FormalN]", "\\[FormalO]", "\\[FormalP]",
  "\\[FormalQ]", "\\[FormalR]", "\\[FormalS]", "\\[FormalT]",
  "\\[FormalU]", "\\[FormalV]", "\\[FormalW]", "\\[FormalX]",
  "\\[FormalY]", "\\[FormalZ]", "\\[FormalCapitalA]",
  "\\[FormalCapitalB]", "\\[FormalCapitalC]", "\\[FormalCapitalD]",
  "\\[FormalCapitalE]", "\\[FormalCapitalF]", "\\[FormalCapitalG]",
  "\\[FormalCapitalH]", "\\[FormalCapitalI]", "\\[FormalCapitalJ]",
  "\\[FormalCapitalK]", "\\[FormalCapitalL]", "\\[FormalCapitalM]",
  "\\[FormalCapitalN]", "\\[FormalCapitalO]", "\\[FormalCapitalP]",
  "\\[FormalCapitalQ]", "\\[FormalCapitalR]", "\\[FormalCapitalS]",
  "\\[FormalCapitalT]", "\\[FormalCapitalU]", "\\[FormalCapitalV]",
  "\\[FormalCapitalW]", "\\[FormalCapitalX]", "\\[FormalCapitalY]",
  "\\[FormalCapitalZ]", "\\[FormalCapitalAlpha]",
  "\\[FormalCapitalBeta]", "\\[FormalCapitalGamma]",
  "\\[FormalCapitalDelta]", "\\[FormalCapitalEpsilon]",
  "\\[FormalCapitalZeta]", "\\[FormalCapitalEta]",
  "\\[FormalCapitalTheta]", "\\[FormalCapitalIota]",
  "\\[FormalCapitalKappa]", "\\[FormalCapitalLambda]",
  "\\[FormalCapitalMu]", "\\[FormalCapitalNu]", "\\[FormalCapitalXi]",
  "\\[FormalCapitalOmicron]", "\\[FormalCapitalPi]",
  "\\[FormalCapitalRho]", "\\[FormalCapitalSigma]",
  "\\[FormalCapitalTau]", "\\[FormalCapitalUpsilon]",
  "\\[FormalCapitalPhi]", "\\[FormalCapitalChi]",
  "\\[FormalCapitalPsi]", "\\[FormalCapitalOmega]", "\\[FormalAlpha]",
  "\\[FormalBeta]", "\\[FormalGamma]", "\\[FormalDelta]",
  "\\[FormalCurlyEpsilon]", "\\[FormalZeta]", "\\[FormalEta]",
  "\\[FormalTheta]", "\\[FormalIota]", "\\[FormalKappa]",
  "\\[FormalLambda]", "\\[FormalMu]", "\\[FormalNu]", "\\[FormalXi]", "\
\\[FormalOmicron]", "\\[FormalPi]", "\\[FormalRho]",
  "\\[FormalFinalSigma]", "\\[FormalSigma]", "\\[FormalTau]",
  "\\[FormalUpsilon]", "\\[FormalCurlyPhi]", "\\[FormalChi]",
  "\\[FormalPsi]", "\\[FormalOmega]"};

$additionalSymbols = {
  "FEPrivate`AddSpecialArgCompletion"
};

$contexts = Sort[
  Join[
    Select[Contexts[],
      StringFreeQ[#, { "`Private`", "`PackagePrivate`", StartOfString ~~ _?LowerCaseQ ~~ ___, StartOfString ~~ "$" ~~ ___ }] &],
    {"System`Private`"}
  ]
];
$currentDir = DirectoryName@System`Private`$InputFileName;
PackageExport["$builtInNames"]
$builtInNames := Sort[Join[getStrippedContextNames["System`"], $packageMacros, $builtInNamedCharacters]];
$allNames := Sort[Join[Flatten[getContextNames /@ $contexts], $additionalSymbols ]];

$existingSymbolVersions := Association @@ Get[FileNameJoin[{$currentDir, "versionedSymbols.m"}]];

PackageExport["$versionedNames"]
$versionedNames = Sort[
  Flatten[
    getContextNames /@ {"System`", "Developer`", "Internal`"}
  ]
];

(* For good code completion we need an ordering of all possible completions. This is done with the *)
(* function frequency list that comes with Mathematica nowadays. I just assign numbers according to the *)
(* place in this list. The higher the number, the more important and the more like is the completion result. *)
PackageExport["$functionFrequency"]
$functionFrequency = With[{file = First[FileNames["all_top_level.m", {$InstallationDirectory}, Infinity]]},
  Dispatch[Append[
    MapIndexed[Rule["System`" <> #1, First[#2]]&, Reverse[Get[file]]],
    _ -> "0"
  ]]
];

(*namedCharacterQ[str_String] :=*)
(*    StringMatchQ[ToString@FullForm[str], "\"\\[" ~~ __ ~~ "]\""];*)

(* Call patterns, attributes and options of functions are available too and don't need to be extracted manually *)
PackageExport["$functionInformation"]
$functionInformation = With[{file = First[FileNames["SystemFiles/Kernel/TextResources/English/FunctionInformation.m", {$InstallationDirectory}, Infinity]]},
  DeleteCases[
    Rule @@@ Get[file],
    {_String?namedCharacterQ, __},
    Infinity
  ]
];

getContextNames[context_String] := Block[{$ContextPath = {context}},
  StringJoin[context, #]& /@ Names[RegularExpression[context <> "\$?[A-Z]\\w*"]]
];

ClearAll[isFunction, getOptions, getAttributes];
isFunction[symbol_String] := Not[TrueQ[Quiet@ToExpression[symbol, InputForm, ValueQ]]];
getOptions[symbol_String /; isFunction[symbol]] := Quiet@Keys[ToExpression[symbol, InputForm, Options]];
getOptions[__] := {};
getAttributes[symbol_String /; isFunction[symbol]] := With[
  {
    result = Quiet@ToExpression[symbol, InputForm, Attributes]
  },
  result /; Head[result === List]
];
getAttributes[__] := {};
getImportance[symbol_String] := ToExpression[(symbol /. $functionFrequency)];

cleanProperty[arg_List] := arg;
cleanProperty[__] := {};

PackageExport["getInformation"]
getInformation[context_, {name_, patt_, opts_, highlighting_, unknown_, _}] := Association[
  "context" -> context,
  "name" -> name,
  "functionQ" -> isFunction[context <> name] || Length[cleanProperty[patt]] > 0 || Length[cleanProperty[opts]] > 0,
  "options" -> cleanProperty@opts,
  "attributes" -> ToString /@ getAttributes[context <> name],
  "callPattern" -> ToString /@ cleanProperty@patt,
  (*"Highlighting" -> (cleanProperty[highlighting /. Infinity -> -1]),*)
  "importance" -> getImportance[context <> name]
];
getInformation[context_, {name_}] := getInformation[context, {name, {}, {}, {}, {}, None}];
getInformation[context_, {name_, patt_}] := getInformation[context, {name, patt, {}, {}, {}, None}];
getInformation[context_, {name_, patt_, opts_ }] := getInformation[context, {name, patt, opts, {}, {}, None}];
getInformation[context_, {name_, patt_, opts_, highlighting_}] := getInformation[context, {name, patt, opts, highlighting, {}, None}];
getInformation[context_, {name_, patt_, opts_, highlighting_, _}] := getInformation[context, {name, patt, opts, highlighting, {}, None}];

PackageExport["SymbolInformation"]
SymbolInformation[] := Association @@ Flatten[
  {
    Table[
      With[{context = First[entry]},
        (context <> #[[1]] -> getInformation[context, #])& /@ entry[[2]]
      ], {entry, $functionInformation}
    ],
    (# -> getInformation["", {#, {_}}])& /@ $packageMacros
  }
];

PackageExport["SaveSymbolInformation"]
SaveSymbolInformation[outputPath_String /; DirectoryQ[outputPath]] := Export[
  FileNameJoin[{outputPath, "SymbolInformation.json"}],
  SymbolInformation[],
  "JSON"
];

PackageExport["SaveContexts"]
SaveContexts[outputPath_String /; DirectoryQ[outputPath]] := Export[
  FileNameJoin[{outputPath, "Contexts.json"}],
  $contexts,
  "JSON"
];

PackageExport["SaveSymbolVersions"]
SaveSymbolVersions[outputPath_String /; DirectoryQ[outputPath]] := Export[
  FileNameJoin[{outputPath, "SymbolVersions.json"}],
  $existingSymbolVersions,
  "JSON"
];

PackageExport["SaveSystemSymbols"]
SaveSystemSymbols[outputPath_String /; DirectoryQ[outputPath]] := Export[
  FileNameJoin[{outputPath, "SystemSymbolNames.json"}],
  $builtInNames,
  "JSON"
];

PackageExport["SaveContextSymbols"]
SaveContextSymbols[outputPath_String /; DirectoryQ[outputPath]] := Export[
  FileNameJoin[{outputPath, "ContextSymbolNames.json"}],
  $allNames,
  "JSON"
];

PackageExport["SaveCompleteMathematicaInformation"]
SaveCompleteMathematicaInformation[outputPath_String /; DirectoryQ[outputPath]] := Module[{},
  SaveSymbolInformation[outputPath];
  SaveContexts[outputPath];
  SaveSymbolVersions[outputPath];
  SaveSystemSymbols[outputPath];
  SaveContextSymbols[outputPath];
];

(* ::Section:: *)
(* Named Characters *)

PackageExport["namedCharacterQ"]
namedCharacterQ[c_Integer] := With[
  {
    str = ToString[FromCharacterCode[c], InputForm, CharacterEncoding -> "PrintableASCII"]
  },
  StringMatchQ[str, "\"\\[" ~~ __ ~~ "]\""]
];
namedCharacterQ[name_String] := namedCharacterQ[First@ToCharacterCode[name]];

PackageExport["identifierPartQ"]
identifierPartQ[code_Integer] := Quiet@Check[ Symbol["a" <> FromCharacterCode[code] <> "b"]; True, False];

PackageExport["codePointToCharacterName"]
codePointToCharacterName::inv = "Code point `` is not a valid named character.";
codePointToCharacterName[code_Integer] := With[
  {
    str = ToString[FromCharacterCode[code], InputForm, CharacterEncoding -> "PrintableASCII"]
  },
  If[TrueQ@StringMatchQ[str, "\"\\[" ~~ __ ~~ "]\""],
    StringReplace[str, "\"\\[" ~~ name__ ~~ "]\"" :> name],
    Message[codePointToCharacterName::inv, code];
    $Failed
  ]
];

PackageExport["codePointToNamedCharacterString"]
codePointToNamedCharacterString[code_Integer] := Module[
  {
    name = codePointToCharacterName[code]
  },
  "\\[" <> name <> "]"
      /; name =!= $Failed
];

PackageExport["namedCharacterPropertiesEntry"]
namedCharacterPropertiesEntry[c_Integer] := With[
  {
    str = ToString[FromCharacterCode[c], InputForm, CharacterEncoding -> "PrintableASCII"]
  },
  {
    StringReplace[str, "\"\\[" ~~ name__ ~~ "]\"" :> name],
    "\\u" <> IntegerString[c, 16, 4],
    FromCharacterCode[c]
  }
];
namedCharacterPropertiesEntry[l : {_Integer ..}] := With[
  {
    sorted = SortBy[namedCharacterPropertiesEntry /@ l, First]
  },
  StringRiffle[#1 <> "=" <> #2 & @@@ sorted, "\n"]
];

PackageExport["createNamedCharactersPropertiesFile"]
createNamedCharactersPropertiesFile[path_, file_ : "NamedCharacters.properties"] := Module[
  {
    (* Unicode range until the first private region. Should be enough *)
    range = Select[$publicUnicodeArea, namedCharacterQ[#] && identifierPartQ[#] &],
    entries
  },
  entries = namedCharacterPropertiesEntry[range];
  Export[FileNameJoin[{path, file}], entries, "String"]
];

PackageExport["createUnicodeToNamedCharacterConversionFile"]
createUnicodeToNamedCharacterConversionFile[path_] := Module[
  {
    range = Select[Range[32, 16^^FFFF], namedCharacterQ[#]&],
    entries
  },
  entries = IntegerString[#] -> namedCharacterPropertiesEntry[#][[1]] & /@ range;
  Export[FileNameJoin[{path, "UnicodeToNamedCharacter.json"}], entries, "JSON"]
];

PackageExport["createNamedCharacterLexerTokens"]
createNamedCharacterLexerTokens[pluginProjectPath_String /; DirectoryQ[pluginProjectPath]] := Module[
  {
    range = Select[Range[16^^FFFF], namedCharacterQ[#]&],
    chars,
    lexerDir = "src/de/halirutan/mathematica/lang/lexer",
    outDir
  },
  outDir = FileNameJoin[{pluginProjectPath, lexerDir}];
  If[!DirectoryQ[outDir],
    Return[$Failed]
  ];
  chars = Function[l, codePointToCharacterName /@ l ] /@ GroupBy[range, identifierPartQ];
  Export[
    FileNameJoin[{outDir, "NamedCharacterIdentifiers.txt"}],
    "NamedCharacterIdentifiers = " <> Utils`createReducedRegex[chars[True]],
    "String"
  ];
  Export[
    FileNameJoin[{outDir, "NamedCharacterOperators.txt"}],
    "NamedCharacterOperators = " <> Utils`createReducedRegex[chars[False]],
    "String"
  ];
];


