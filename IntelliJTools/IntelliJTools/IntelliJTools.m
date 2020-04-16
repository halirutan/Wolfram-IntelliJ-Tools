(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA, see http://wlplugin.halirutan.de/ *)

(* :Title: MathematicaInformationTools *)
(* :Context: MathematicaInformationTools` *)
(* :Author: Patrick Scheibe *)
(* :Date: 2018-09-30 *)

(* :Copyright: (c) 2018-2020 Patrick Scheibe *)
(* :Keywords: *)
(* :Discussion: *)

Package["IntelliJTools`"]
PackageImport["JLink`"]
JLink`InstallJava[];

(* ::Section:: *)
(* Initialization of symbol and context lists *)

JLink`LoadJavaClass["de.halirutan.wlintellij.Utils"];
$directory = DirectoryName@System`Private`$InputFileName;
$publicUnicodeArea = Join[Range[16^^E000 - 1], Range[16^^F8FF + 1, 16^^FFFF]];

ClearAll[
  $packageMacros,
  $contexts,
  $builtInNames,
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
  "\\[FormalLambda]", "\\[FormalMu]", "\\[FormalNu]", "\\[FormalXi]",
  "\\[FormalOmicron]", "\\[FormalPi]", "\\[FormalRho]",
  "\\[FormalFinalSigma]", "\\[FormalSigma]", "\\[FormalTau]",
  "\\[FormalUpsilon]", "\\[FormalCurlyPhi]", "\\[FormalChi]",
  "\\[FormalPsi]", "\\[FormalOmega]"};

$additionalSymbols = {
  "FEPrivate`AddSpecialArgCompletion"
};

$contexts = Sort[
  Join[
    Select[DeleteCases[Contexts[], "Global`"],
      StringFreeQ[#, { "`Private`", "`PackagePrivate`", StartOfString ~~ _?LowerCaseQ ~~ ___, StartOfString ~~ "$" ~~ ___ }] &],
    {"System`Private`"}
  ]
];
$currentDir = DirectoryName@System`Private`$InputFileName;

(* Note, this is done during package loading so that $allNames is fixed before we do crazy shit with the system. *)
(* Especially, loading the auto-loadable symbols will introduce new System symbols (bug in Mma) and other stuff. *)
PackageExport["$builtInNames"]
$builtInNames = Sort[Join[getStrippedContextNames["System`"], $packageMacros, $builtInNamedCharacters]];
$allNames = Sort[Join[Flatten[getContextNames /@ $contexts], $additionalSymbols ]];

(* ::Section:: *)
(* Methods for extracting symbol information *)

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

(* Some symbols are not loaded by default and therefore checking, e.g. if they are a function will fail. *)
(* Here, we try to test for that and load symbols that are not loaded by default. *)
ClearAll[autoloadQ, carefullyLoad];

Attributes[autoloadQ] = {HoldAll};
autoloadQ[sym_Symbol] := With[
  {
    ov = Quiet[OwnValues[sym], {General::readp}]
  },
  ov =!= $Failed && ov =!= {} && Extract[ov, {1, 2, 0}, Hold] === Hold[Package`ActivateLoad]
];
autoloadQ[str_String] := ToExpression[str, InputForm, autoloadQ];

Attributes[carefullyLoad] = {HoldAll};
carefullyLoad[sym_Symbol] /; autoloadQ[sym] := With[
  {
    ov = Quiet[OwnValues[sym], {General::readp}]
  },
  With[
    {
      loader = Extract[ov, {1, 2}, Hold]
    },
    (* Below, we replace to symbol to be loaded with something else to prevent that it will be evaluated in the *)
    (* process. Therefore, "Times" could be anything that doesn't do harm. To give an example: System`$MobilePhone is *)
    (* an auto-loaded symbol which means the OwnValues will contain the code to load all necessary parts. We do not know *)
    (* if it's a function or a variable yet, but if we "touch" the symbol $MobilePhone or evaluate the auto-loading code *)
    (* it will also evaluate $MobilePhone on the way which accesses the internet and takes a long time. *)
    (* We want to load the definitions for $MobilePhone without evaluating it. Therefore, we place a dummy symbol in the *)
    (* auto-loading code which will load all necessary packages but only evaluate "Times". *)
    First@ReplacePart[loader, {1, 1} -> Times]
  ]
];
carefullyLoad[str_String] := ToExpression[str, InputForm, carefullyLoad];

ClearAll[isFunction, getOptions, getAttributes];
isFunction[symbol_String /; autoloadQ[symbol]] := (carefullyLoad[symbol]; isFunction[symbol]);
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

(* Takes the symbol lists for all available Wolfram Language versions and *)
combineSymbolVersion[] := Module[
  {
    files,
    versions
  },
  files = FileNames["*.txt", {FileNameJoin[{$directory, "Resources"}]}];
  versions = Get[#] -> ToExpression@StringReplace[FileBaseName[#], beg__ ~~ "-symbols" :> beg]& /@ files;
  Merge[AssociationThread /@ versions, Identity]
];

PackageExport["SaveSymbolVersions"]
SaveSymbolVersions[outputPath_String /; DirectoryQ[outputPath]] := Export[
  FileNameJoin[{outputPath, "SymbolVersions.json"}],
  combineSymbolVersion[],
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


(* ::Section:: *)
(* Creating html usage messages *)

(*	Here we replace Mathematica box expressions with HTML constructs. If we lack of some things we just use a
	a string representation like with UnderscriptBox *)
$boxRules = {
  StyleBox[f_, "TI"] :> {"<em>", f, "</em>"},
  StyleBox[f_, ___] :> {f},
  RowBox[l_] :> {l},
  SubscriptBox[a_, b_] :> {a, "<sub>", b, "</sub>"},
  SuperscriptBox[a_, b_] :> {a, "<sup>", b, "</sup>"},
  RadicalBox[x_, n_] :> {x, "<sup>1/", n, "</sup>"},
  FractionBox[a_, b_] :> {"(", a, ")/(", b, ")"},
  SqrtBox[a_] :> {"&radic;(", a, ")"},
  CheckboxBox[a_, ___] :> {"<u>", a, "</u>"},
  OverscriptBox[a_, b_] :> {"Overscript[", a, b, "]"},
  OpenerBox[a__] :> {"Opener[", a, "]"},
  RadioButtonBox[a__] :> {"RadioButton[", a, "]"},
  UnderscriptBox[a_, b_] :> {"Underscript[", a, b, "]"},
  UnderoverscriptBox[a_, b_, c_] :> {"Underoverscript[", a, b, c,
    "]"},
  SubsuperscriptBox[a_, b_, c_] :> {a, "<sub><small>", b,
    "</small></sub><sup><small>", c, "</small></sup>"}
};

(* The situation is weird. On Linux there are some symbols that are not displayed correctly, like &#10869; while *)
(* on other systems this works fine. I will only fix very few since it should work in general *)
$specialHtmlCharacterRules = {
  "<>" -> "&lt;&gt;",
  "&#62754;" -> "&rarr;",
  "&#61715;" -> "&lt;&#x7c;",
  "&#61716;" -> "&#x7c;&gt;",
  "&#10740;" -> ":>",
  "&#10869;" -> "=="
};

$referenceURL = "http://reference.wolfram.com/language/";
$searchURL = "http://reference.wolfram.com/search/?q=";


(*	Repeatedly replacing box expressions until nothing is left, then we join everything into a big String *)
convertBoxExpressionToHTML[boxExpr_] := StringJoin[ToString /@ Flatten[ReleaseHold[MakeExpression[boxExpr] //. $boxRules]]];

(* 	We need to take care to not evaluate symbols like Black (which is ev to RGBColor[0,0,0]) before we extract the
	usage message.
*)
hasUsage[str_] := With[{usg = ToExpression[str, InputForm, Function[s, MessageName[s, "usage"], HoldFirst]]},
  Head[usg] =!= MessageName
];

getUsage[str_] := With[{usg = ToExpression[str, InputForm, Function[s, MessageName[s, "usage"], HoldFirst]]},
  If[Head[usg] =!= MessageName,
    usg, ""
  ]
];

extractUsage[str_] := With[{usg = Function[expr, expr::usage, HoldAll] @@ MakeExpression[str]},
  If[Head[usg] === String, usg, ""]];

extractUsage[str_String, context_String] :=
    With[{usg =
        Function[expr, expr::usage, HoldAll] @@
            MakeExpression[context <> str, StandardForm]},
      If[Head[usg] === String, usg, ""]];

replaceNestedStyleString[str_] := StringReplace[
  str,
  {Shortest["\\\"\\!\\(\\*StyleBox[\\\"" ~~ name__ ~~ "\\\"" ~~ __ ~~ "\\_" ~~ n_ ~~ "\\)\\\""] :>
      "&quot;<em>" ~~ name ~~ "<sub>" ~~ n ~~ "</sub></em>&quot;",
    Shortest["\\\"\\!\\(\\*StyleBox[\\\"" ~~ name__ ~~ "\\\"" ~~ __ ~~ "\\)\\\""] :>
        "&quot;<em>" ~~ name ~~ "</em>&quot;"
  }];

namedCharacterQ[str_String] :=
    StringMatchQ[ToString@FullForm[str], "\"\\[" ~~ __ ~~ "]\""];

fixNamedCharacterLink[str_] := {StringReplace[
  ToString@FullForm[str], {"\"" :> "", "\\" -> "\\\\"}],
  StringReplace[
    ToString@FullForm[str], {"\"" :> "", "\\[" ~~ c__ ~~ "]" :> c}]};

createLinkName[s_] := If[StringMatchQ[ToString@FullForm[s], "\"\\[" ~~ __ ~~ "]\""],
  {
    StringReplace[ToString@FullForm[s], {"\"" :> "", "\\" -> "\\\\"}],
    StringReplace[ToString@FullForm[s], {"\"" :> "", "\\[" ~~ c__ ~~ "]" :> "character/" ~~ c}]
  },
  {s, s}
];

replaceNestedStyleString[str_] := StringReplace[str,
  Shortest["\\\"\\!\\(\\*StyleBox[\\\"" ~~ name__ ~~ "\\\"" ~~ __ ~~
      "\\_" ~~ n_ ~~ "\\)\\\""] :>
      "\\\"<em>" ~~ name ~~ "<sub>" ~~ n ~~ "</sub></em>\\\""];


createOnlineLink[symbol_String, context_String, checkUrl_] := Module[
  {charPart = "", middle, symbolPart, linkName, finalLink,
    root = $referenceURL},
  If[namedCharacterQ[symbol],
    {linkName, symbolPart} = fixNamedCharacterLink[symbol];
    charPart = "character/",
    {linkName, symbolPart} = {symbol, symbol}
  ];

  If[
    context === "System`",
    middle = "",
    middle = StringDrop[context, -1] <> "/"
  ];
  finalLink =
      root <> middle <> "ref/" <> charPart <> symbolPart <> ".html";
  If[TrueQ[checkUrl] && URLFetch[finalLink, "StatusCode"] === 404,
    finalLink =
        $searchURL <> linkName
  ];


  {symbolPart,
    "<a href=\"" <> finalLink <> "\">" <> linkName <> "</a>"}
];


createOptionString[s_] := With[{opts = Function[expr, Options[Unevaluated[expr]], HoldAll] @@ MakeExpression[s]},
  If[opts === {},
    "<p><b>Symbol has no options.</b></p>",
    "<p><b>Options: </b>" <> StringJoin@Riffle[ToString[First[#]] & /@ opts, ", "] <> "</p>"
  ]
];

convertUsageStringToHTML[usg_] := Module[{},
  Quiet@Check[
    StringSplit[
      StringReplace[StringReplace[
        StringReplace[
          usg, {Shortest["\!\(\*" ~~ content__ ~~ "\)"] :>
            StringReplace[
              convertBoxExpressionToHTML[StringReplace[replaceNestedStyleString[content], "\n" :> ""]],
              "<>" -> "&lt;&gt;"] }], {

          "\[Null]" :> "",
          a_?(StringMatchQ[ToString@FullForm[#], "\"\\[" ~~ __ ~~ "]\""] &) :> StringReplace[ToString[a, MathMLForm], {WhitespaceCharacter :> ""}]}
      ], $specialHtmlCharacterRules], "\n"],
    ""
  ]
];

Options[CreateHTMLUsageString] = {
  "CheckURL" -> False
};

PackageExport["CreateHTMLUsageString"]
CreateHTMLUsageString[s_String, context_String, OptionsPattern[]] := Module[{
  usg = extractUsage[s, context],
  attr = With[{full = context <> s}, Attributes[full]],
  link, name, html},

  {name, link} = createOnlineLink[s, context, OptionValue["CheckURL"]];
  html = Quiet[Check[convertUsageStringToHTML[usg], usg]];
  {name, StringJoin[
    "<h3>", link, "</h3>",
    If[usg =!= "",
      "<ul><li>" <> convertUsageStringToHTML[usg] <> "</ul>",
      ""
    ],
    "<p><b>Attributes:</b>",
    StringJoin[ToString /@ Riffle[attr, ", "]],
    "</p>",
    createOptionString[s]
  ]}
];

$versionedNames := Sort[
  Flatten[
    getContextNames /@ {"System`", "Developer`", "Internal`"}
  ]
];


PackageExport["CreateAllHtmlUsageMessages"]
CreateAllHtmlUsageMessages[path_String] := Module[
  {
    outPath,
    context,
    symbol
  },
  Do[
    context = Context[Evaluate[name]];
    symbol = StringDelete[#, Context[#]] &[name];
    outPath = FileNameJoin[{path, StringDrop[context, -1]}];
    If[! DirectoryQ[outPath],
      CreateDirectory[outPath]
    ];

    With[
      {
        file = FileNameJoin[{outPath, symbol <> ".html"}]
      },
      If[
        Not[TrueQ@FileExistsQ[file]],
        With[{res = CreateHTMLUsageString[symbol, context]},
          Export[file, res[[2]], "Text"];
        ]
      ]
    ], {name, $versionedNames}];
];

(* ::Section:: *)
(* Create Dictionary *)

(* ::Text:: *)
(* We ship the Wolfram Language Plugin with a custom dictionary. This prevents any of the Mathematica functions
 being highlighted by the spell-check. The dictionary is quite small since IntelliJ works with Camel-Case.
 Therefore, to ensure that, e.g. FeatureExtractorFunction is a valid word, we need to verify that Feature, Extractor,
 and Function are valid words. This leaves only a small number of words we need to ship in the dictionary. *)

(* ::Text:: *)
(* Note: The dictionary needs to be cleaned after creation since there are some typos like "Tranparency" *)

isWord[word_] := UpperCaseQ[StringPart[word, 1]] && StringFreeQ[word, "$"] && StringLength[word] > 2;
processWord[word_] := DeleteDuplicates@Flatten[
  StringCases[word, _?UpperCaseQ ~~ __?LowerCaseQ]
];

getNames[context_String] := Module[{names = Names[context <> "*"]},
  names =
      Select[StringReplace[names, context ~~ name__ :> name], isWord];
  processWord[names]
];

PackageExport["SaveDictionary"]
SaveDictionary[outputPath_String /; DirectoryQ[outputPath]] := Module[
  {
    names
  },
  names = Select[DeleteDuplicates@Sort[Flatten[getNames /@ Contexts[]]], Not[DictionaryWordQ[#]] &];
  Export[FileNameJoin[{outputPath, "WLDictionary.dic"}], names, "List"]
]