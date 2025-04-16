(* ::Package:: *)

(* ::Package:: *)

(* :Title: Trivia Mastermind - Question Module *)
(* :Context: Questions` *)
(* :Author: Gruppo 10 - I Ludopatici *)
(* :Summary: Package for handling trivia questions in Mastermind game *)
(* :Package Version: 0.3 *)
(* :History: last modified 16/4/2025 *)
(* :Copyright: \[Copyright] 2025 Gruppo 10 - Trivia Mastermind *)
(* :License: MIT License *)

BeginPackage["Questions`"];

DisplayTriviaQuestion::usage = "Displays a trivia question dialog with options. Returns True if correct answer is selected, False otherwise. Parameters: seed (Integer), questionsDataset (Dataset)";
LoadQuestionsFromCSV::usage = "Loads questions from CSV file into a Dataset. Parameter: path (String)";
InitializeQuestionInterface::usage = "Creates the main question interface window. Parameter: seed (Integer)";
ProvideHintFeedback::usage = "Provides feedback based on answer correctness. Parameter: guessedCorrectly (True|False)";
PrepareQuestionData::usage = "Selects and prepares question data. Returns {question, options, correctIndex}. Parameters: seed (Integer), questionsDataset (Dataset)";

(* Global variables that need to be shared across functions *)

Begin["`Private`"]
timeToWait = 1.0;
paneSize={500,80};
imageSize= {200, 40};
windowSize= {600, Automatic};




(*
  Loads trivia questions from a CSV file into a dataset
  @param path: String path to the CSV file
  @return: Dataset containing the questions or $Failed if error occurs
*)
LoadQuestionsFromCSV[path_String] := Module[
  {csvText, data, headers, rows, dataset},
  
  (* Attempt to import the CSV file *)
  csvText = Quiet@Check[
    Import[path, "Text"],
    Print["\:274c Failed to import CSV text."];
    Return[$Failed]
  ];

  Print["\:2705 CSV text imported successfully."];

  (* Parse the CSV content *)
  data = Quiet@Check[
    ImportString[csvText, "CSV"],
    Print["\:274c Failed to parse CSV content."];
    Return[$Failed]
  ];

  (* Convert to dataset format *)
  headers = data[[1]];
  rows = data[[2 ;;]];
  dataset = Dataset[AssociationThread[headers, #] & /@ rows];
  
  Print["\:2705 Questions loaded: ", Length[dataset]];
  dataset
];




(*
  Displays a trivia question dialog with multiple choice options
  @param seed: Integer used to select and shuffle the question
  @param questionsDataset: Dataset containing all trivia questions
  @return: True if correct answer selected, False otherwise
*)
DisplayTriviaQuestion[seed_Integer, questionsDataset_] := Module[
  {questionWindow, result = $Failed, dialogOpen = True, 
   currentQuestion, questionOptions, correctAnswerIndex},
  
  (* Get shuffled question and options *)
  {currentQuestion, questionOptions, correctAnswerIndex} = PrepareQuestionData[seed, questionsDataset];
  
  (* Create the question dialog *)
  questionWindow = CreateDialog[
    Column[{
      Pane[
        Style[currentQuestion["Question"], 16, Bold, TextAlignment -> Center],
        ImageSize -> {500,80},
        Scrollbars -> False,
        Alignment -> Center
      ],
      Spacer[20],
      Grid[
        Partition[
          MapIndexed[
            Function[{optionText, optionIndex},
              DynamicModule[{clicked = False, isCorrect = Null, position = First[optionIndex]},
                Button[
                  optionText,
                  isCorrect = (position == correctAnswerIndex);
                  clicked = True;
                  result = isCorrect;
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
                  FrameMargins -> 12
                ]
              ]
            ],
            questionOptions
          ],
          UpTo[Ceiling[Length[questionOptions]/2]]
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
  
  (* Wait for dialog to close *)
  While[dialogOpen, Pause[0.1]];
  result
];




(*
  Prepares question data by selecting and shuffling options
  @param seed: Integer used for random selection
  @param questionsDataset: Dataset containing all questions
  @return: {question, options, correctIndex}
*)
PrepareQuestionData[seed_Integer, questionsDataset_] := Module[
  {questionIndex, options, rawCorrectIndex, selectedQuestion, optionKeys, correctIndex},
  
  SeedRandom[seed];
  (* Select question based on seed *)
  questionIndex = Mod[seed, Length[questionsDataset], 1];
  selectedQuestion = Normal[questionsDataset[[questionIndex]]];

  (* Get correct answer index *)
  rawCorrectIndex = Lookup[selectedQuestion, "Correct Index", Missing["NotAvailable"]];
  rawCorrectIndex = If[NumberQ[rawCorrectIndex], Round[rawCorrectIndex], 1];

  (* Extract all available options *)
  optionKeys = {"Option A", "Option B", "Option C", "Option D"};
  options = DeleteCases[Lookup[selectedQuestion, optionKeys, ""], _Missing | "" | Null];

  {selectedQuestion, options, rawCorrectIndex}
];




(*
  Provides feedback based on whether user guessed correctly
  @param guessedCorrectly: Boolean indicating if answer was correct
*)
ProvideHintFeedback[guessedCorrectly:(True | False)] := Module[
  {},
  If[guessedCorrectly,
    Print["Correct answer! Here's your hint..."],
    Print["Incorrect answer. Better luck next time!"]
  ]
];





(*
  Initializes the trivia question interface
  @param seed: Integer seed for question selection
*)
InitializeQuestionInterface[seed_Integer] := 
  CreateDialog[DynamicModule[
    {questionCounter = 0, result = Null, loadedQuestions = {}},
    loadedQuestions = LoadQuestionsFromCSV["science-technology.csv"];
    Column[{
Button[
    Style["GET A HINT", FontWeight -> Bold, FontFamily -> "Arial", FontSize -> 14], 
    ProvideHintFeedback[DisplayTriviaQuestion[seed + questionCounter, loadedQuestions]];
    questionCounter++,
    Method -> "Queued",
    ImageSize -> {200, 40}, 
    BaseStyle -> {
        FontColor -> Black, 
        FontFamily -> "Arial",
        FontSize -> 14
    },
    FrameMargins -> 12,
    Alignment -> Center
]}]
  ],
  WindowTitle -> "Mastermind Trivia Hints",
  WindowSize -> {300, 150}
  ];





End[];
EndPackage[];


(* ::Input:: *)
(**)
