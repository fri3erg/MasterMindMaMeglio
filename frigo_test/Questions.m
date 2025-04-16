(* ::Package:: *)

(* ::Package:: *)

(* :Title: Trivia Mastermind - Question Module *)
(* :Context: Questions` *)
(* :Author: Gruppo 10 - I Ludopatici *)
(* :Summary: Package for handling trivia questions in Mastermind game *)
(* :Package Version: 0.3 *)
(* :History: last modified 11/4/2025 *)
(* :Copyright: \[Copyright] 2025 Gruppo 10 - Trivia Mastermind *)
(* :License: MIT License *)

BeginPackage["Questions`"];

MostraDomandaTrivia::usage = "MostraDomandaTrivia[seed] displays a trivia question window using the specified seed";
CaricaDomandeDaCSV::usage = "CaricaDomandeDaCSV[path] loads questions from CSV file";

Begin["`Private`"]

(* Default empty question database *)
domandeTrivia = {};
resultList={};
questionCounter = 0;

CaricaDomandeDaCSV[path_String] := Module[
  {csvText, data, headers, rows, dataset},

  csvText = Quiet@Check[
    Import[path, "Text"],
    Print["\:274c Failed to import CSV text."];
    Return[$Failed]
  ];

  Print["\:2705 CSV text imported successfully."];

  data = Quiet@Check[
    ImportString[csvText, "CSV"],
    Print["\:274c Failed to parse CSV content."];
    Return[$Failed]
  ];

  Print["\:2705 CSV parsed successfully. Rows loaded: ", Length[data]];

  If[!ListQ[data] || Length[data] < 2,
    Print["\:274c CSV structure invalid: should have at least a header and one row."];
    Return[$Failed];
  ];

  headers = data[[1]];
  rows = data[[2 ;;]];

  dataset = Dataset[AssociationThread[headers, #] & /@ rows];
  domandeTrivia = dataset;

];
MostraDomandaTrivia[seed_Integer] := Module[
  {questionWindow, result = $Failed, dialogOpen = True},
  
  If[Length[domandeTrivia] == 0,
    Print["\:274c No questions loaded. Use CaricaDomandeDaCSV first."];
    Return[$Failed]
  ];
  
  {currentQuestion, validOptions, correctIndex} = ShuffleQuestion[seed];
  
  questionWindow = CreateDialog[
    Column[{
      Pane[
        Style[currentQuestion["Question"], 16, Bold, TextAlignment -> Center],
        ImageSize -> {500, 80},
        Scrollbars -> False,
        Alignment -> Center
      ],
      Spacer[20],
      Grid[
        Partition[
          MapIndexed[
            Function[{text, idx},
              DynamicModule[{clicked = False, isCorrect = Null, pos = First[idx]},
                Button[
                  text,
                  isCorrect = (pos == correctIndex);
                  clicked = True;
                  result = isCorrect;
                  If[isCorrect, 
                    questionCounter++;
                    {currentQuestion, validOptions, correctIndex} = ShuffleQuestion[seed + questionCounter];
                  ];
                  (* Add delay before closing *)
                  RunScheduledTask[
                    NotebookClose[questionWindow];
                    dialogOpen = False;
                    RemoveScheduledTask[$ScheduledTask];
                  , {1.0}],
                  Background -> Dynamic[If[clicked, If[isCorrect, Green, Red], White]],
                  ImageSize -> {200, 40}, 
                  BaseStyle -> {
                    FontColor -> Black, 
                    FontWeight -> Bold,
                    FontFamily -> "Arial",
                    FontSize -> 14
                  },
                  Appearance -> {
                    "Default" -> {
                      FrameMargins -> 10,
                      FrameStyle -> Directive[Thickness[0.015], GrayLevel[0.5]] (* Thicker border *)
                    },
                    "Hover" -> {
                      Background -> Lighter[Gray, 0.95],
                      FrameStyle -> Directive[Thickness[0.008], GrayLevel[0.3]] (* Even thicker on hover *)
                    },
                    "Pressed" -> {
                      Background -> If[clicked, If[isCorrect, Green, Red], Lighter[Gray, 0.9]],
                      FrameStyle -> Directive[Thickness[0.008], GrayLevel[0]] (* Thick black border when pressed *)
                    }
                  },
                  FrameMargins -> 12 (* Increased internal padding *)
                ]
              ]
            ],
            validOptions
          ],
          UpTo[Ceiling[Length[validOptions]/2]]
        ],
        Spacings -> {1, 1}, Alignment -> Center
      ]
    }, Alignment -> Center],
    
    WindowTitle -> "Trivia Mastermind Hint",
    WindowSize -> {600, Automatic},
    Modal -> True,
    WindowElements -> {},
    WindowFrame -> "ModalDialog",
    Background -> White
  ];
  
  While[dialogOpen, Pause[0.1]];
  result
];
ShuffleQuestion[seed_Integer] := Module[
  {qIndex, shuffledOptions, correctIndexRaw, currentQuestion, optionKeys, correctText, newCorrectIndex},
	
SeedRandom[seed];
  qIndex = Mod[seed, Length[domandeTrivia], 1];
  
  currentQuestion = Normal[domandeTrivia[[qIndex]]];

  correctIndexRaw = Lookup[currentQuestion, "Correct Index", Missing["NotAvailable"]];
  correctIndexRaw = If[NumberQ[correctIndexRaw], Round[correctIndexRaw], 1];

  optionKeys = {"Option A", "Option B", "Option C", "Option D"};
  shuffledOptions = DeleteCases[Lookup[currentQuestion, optionKeys, ""], _Missing | "" | Null];

  correctText = Lookup[currentQuestion, optionKeys[[correctIndexRaw]], ""];

  (* shuffle options *)
  shuffledOptions = RandomSample[shuffledOptions];
  newCorrectIndex = FirstPosition[shuffledOptions, correctText][[1]];

  Print["\:2705 Shuffled options: ", shuffledOptions];

  {currentQuestion, shuffledOptions, newCorrectIndex}
];

RevealHint[] := Module[
  {},
  AppendTo[resultList,"guessed"]
  Print[resultList]
  ];
  

End[];
EndPackage[];
