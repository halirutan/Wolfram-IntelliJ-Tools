(* Mathematica Source File *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)
(* :Author: patrick *)
(* :Date: 2019-05-25 *)

Export[
  FileNameJoin[
    {
      DirectoryName@System`Private`$InputFileName,
      ToString[$VersionNumber] <> "-symbols.txt"
    }
  ],
  Sort[
    Flatten[
      Function[context,
        Block[{$ContextPath = {context}},
          StringJoin[context, #] & /@
              Names[RegularExpression[context <> "\$?[A-Z]\\w*"]
              ]
        ]
      ] /@ {"System`", "Developer`", "Internal`"}]],
  "Package"
]