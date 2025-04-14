(* ::Package:: *)

(* :Title:Trivia Mastermind*)
(* :Context:PacchettoProgetto`*)
(* :Author:Gruppo 10 - I Ludopatici*)
(* :Summary:Package per "Trivia Mastermind", progetto di MC Unibo anno 24/25*)
(* :Package Version:0.2*)
(* :History:last modified 11/4/2025*)
(* :Copyright:\[Copyright] 2025 Gruppo 10 - Trivia Mastermind*)
(* :License:MIT License*)
(* :Discussion:Funzionalit\[AGrave] obbligatorie:
	- Seed da chiedere all\[CloseCurlyQuote]utente per (ri)generare un esercizio
	- Genera Esercizio
	- Verifica Risultato
	- Mostra Soluzione
	- Pulisci
*)

BeginPackage["PacchettoProgetto`"];
(*ClearAll["PacchettoProgetto`*"];*)

(* USAGES DI FUNZIONI CHIAMATE ESPLICITAMENTE NEL NOTEBOOK *)
(* ES. f::usage= "text"; *)
avviaSchermataDiGioco::usage="aaaaaa";


Begin["`Private`"];
(* Ricorda di documentare ogni riga di codice: funzionalit\[AGrave],
variabili di input, variabili di lavoro, variabili di output, spiegazione dei singoli passaggi *)


(* aaaaaa *)
avviaSchermataDiGioco[] := Module[
  {
    screenSize, screenWidth, screenHeight,
    fontScale, content, mainWindow, titleFontScale, subTitleFontScale
  },
  
  (* Ottieni dimensioni dello schermo *)
  screenSize = Quiet @ Check[
    FrontEndExecute @ FrontEnd`Value[FE`getScreenSize[]],
    {1920, 1080}
  ];
  
  If[MatchQ[screenSize, {_Integer, _Integer}],
    {screenWidth, screenHeight} = screenSize,
    {screenWidth, screenHeight} = {1920, 1080}
  ];
  
  titleFontScale = Min[screenWidth, screenHeight] / 15;
  
  content = Column[
    {
      Spacer[30],
      Style["MASTERMIND MA MEGLIO",
        FontSize -> titleFontScale,
        FontWeight -> Bold,
        FontColor -> Black,
        FontFamily -> "IBM Plex Mono",
        TextAlignment -> Center
      ],
      
      Spacer[5],
      
      Style["NOME MEMBRI GRUPPO",
        FontSize -> titleFontScale/4,
        FontWeight -> Normal,
        FontColor -> Gray,
        FontFamily -> "IBM Plex Mono",
        TextAlignment -> Center
      ], 
      
      Spacer[50],
      Row[{
		 ClickPane[
		  Framed[
		    Grid[{
		      {
		        Graphics[Text[Style["\|01f331", FontSize -> 24]], ImageSize -> 24],
		        Style["SEED GAME", Black, FontFamily -> "Arial", FontSize -> 24, Bold]
		      }
		    },
		    Alignment -> {Center, Center}, Spacings -> {1.5, 0}
		    ],
		    
		    Background -> LightGray,
		    FrameStyle -> None,
		    RoundingRadius -> 10,
		    FrameMargins -> {{15, 15}, {5, 5}},
		    ImageSize -> Automatic
		  ],
		  
		  Function[Print["Pulsante cliccato!"]]
		],
		Spacer[40],
		 ClickPane[
		  Framed[
		    Grid[{
		      {
		         Graphics[Text[Style["\|01f3b2", Red, FontSize -> 24]], ImageSize -> 24],
		        Style["RANDOM GAME", Black, FontFamily -> "Arial", FontSize -> 24, Bold]
		      }
		    },
		    Alignment -> {Center, Center}, Spacings -> {1.5, 0}
		    ],
		    
		    Background -> LightGray,
		    FrameStyle -> None,
		    RoundingRadius -> 10,
		    FrameMargins -> {{15, 15}, {5, 5}},
		    ImageSize -> Automatic
		  ],
		  
		  Function[Print["Pulsante cliccato!"]]
		],
		Spacer[40],
		 ClickPane[
		  Framed[
		    Grid[{
		      {
				Graphics[Text[Style["\|01f6e0\:fe0f", FontSize -> 24]], ImageSize -> 24],
		        Style["CUSTOM GAME", Black, FontFamily -> "Arial", FontSize -> 24, Bold]
		      }
		    },
		    Alignment -> {Center, Center}, Spacings -> {1.5, 0}
		    ],
		    
		    Background -> LightGray,
		    FrameStyle -> None,
		    RoundingRadius -> 10,
		    FrameMargins -> {{15, 15}, {5, 5}},
		    ImageSize -> Automatic
		  ],
		  
		  Function[Print["Pulsante cliccato!"]]
		]
      }],
      Spacer[50],
      
		ClickPane[
		  Framed[
		    Style["ESCI", White, FontFamily -> "Arial", FontSize -> 24, Bold],
		    Background -> Red,
		    FrameStyle -> None,
		    RoundingRadius -> 10,
		    FrameMargins -> {{15, 15}, {5, 5}},
		    ImageSize -> Automatic
		  ],
		  Function[NotebookClose[EvaluationNotebook[]]]
		]
    },
    Alignment -> Center
  ];
  
  mainWindow = CreateDocument[
    {
      Cell[
        BoxData @ ToBoxes[
          Pane[
          content,
		  Full,
		  Alignment -> {Center, Top}
          ]
        ],
        "Output",
        ShowCellBracket -> False,
        CellMargins -> {{0, 0}, {0, 0}}
      ]
    },
    WindowSize -> Full,
    WindowFrame -> "Frameless",
    WindowElements -> {},
    Background -> White,
    Editable -> False,
    Deployed -> True,
    WindowMargins -> {{0, 0}, {0, 0}},
    NotebookEventActions -> {
      {"KeyDown", "Escape"} :> NotebookClose[EvaluationNotebook[]]
    }
  ];
  
  mainWindow
]



End[];
EndPackage[];
