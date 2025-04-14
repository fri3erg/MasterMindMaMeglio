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
  {mainWindow, screenWidth = 1920, screenHeight = 1080, titleFontScale, currentScreen = "menu", content},
  
  (* Ottieni dimensioni schermo *)
  Quiet @ Check[
    {screenWidth, screenHeight} = FrontEndExecute @ FrontEnd`Value[FE`getScreenSize[]],
    {screenWidth, screenHeight} = {1920, 1080}
  ];

  titleFontScale = Min[screenWidth, screenHeight]/15;

  (* Funzione per cambiare schermata *)
  cambiaSchermata[nuovaSchermata_] := (currentScreen = nuovaSchermata);

  (* Schermata del menu principale *)
  creaHomepage[] := Column[
    {
      Spacer[{0, 50}],
      Style["MASTERMIND MA MEGLIO",
        FontSize -> titleFontScale,
        FontWeight -> Bold,
        FontColor -> Black,
        FontFamily -> "Consolas",
        TextAlignment -> Center
      ],
      
      Spacer[{0, 20}],
      
      Style["Made by Alessandro Modelli, Angelo Greco, Elia Friberg, Francesca Mazzetti, Gianpiero Tovo, Matteo Raggi",
        FontSize -> titleFontScale/5,
        FontWeight -> Normal,
        FontColor -> Gray,
        FontFamily -> "Consolas",
        TextAlignment -> Center
      ], 
      
      Spacer[{0, 125}],
      
      Row[{
        ClickPane[
          Framed[
            Grid[{
              {
                Graphics[Text[Style["\|01f331", FontSize -> 24]], ImageSize -> 24],
                Style["SEED GAME", Black, FontFamily -> "Consolas", FontSize -> 24, Bold]
              }
            },
            Alignment -> {Center, Center}, Spacings -> {1.5, 0}
            ],
            Background -> LightGray,
            FrameStyle -> None,
            RoundingRadius -> 10,
            FrameMargins -> {{15, 15}, {15, 15}},
            ImageSize -> Automatic
          ],
          Function[cambiaSchermata["gioco"]]
        ],
        Spacer[40],
        ClickPane[
          Framed[
            Grid[{
              {
                Graphics[Text[Style["\|01f3b2", Red, FontSize -> 24]], ImageSize -> 24],
                Style["RANDOM GAME", Black, FontFamily -> "Consolas", FontSize -> 24, Bold]
              }
            },
            Alignment -> {Center, Center}, Spacings -> {1.5, 0}
            ],
            Background -> LightGray,
            FrameStyle -> None,
            RoundingRadius -> 10,
            FrameMargins -> {{15, 15}, {15, 15}},
            ImageSize -> Automatic
          ],
          Function[cambiaSchermata["gioco"]]
        ],
        Spacer[40],
        ClickPane[
          Framed[
            Grid[{
              {
                Graphics[Text[Style["\|01f6e0\:fe0f", FontSize -> 24]], ImageSize -> 24],
                Style["CUSTOM GAME", Black, FontFamily -> "Consolas", FontSize -> 24, Bold]
              }
            },
            Alignment -> {Center, Center}, Spacings -> {1.5, 0}
            ],
            Background -> LightGray,
            FrameStyle -> None,
            RoundingRadius -> 10,
            FrameMargins -> {{15, 15}, {15, 15}},
            ImageSize -> Automatic
          ],
          Function[cambiaSchermata["gioco"]]
        ]
      }, Alignment -> Center],
      
      Spacer[{0, 125}],
      
      ClickPane[
        Framed[
          Style["ESCI", White, FontFamily -> "Consolas", FontSize -> 24, Bold],
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

  (* Contenuto principale dinamico *)
  content = Pane[
    Dynamic @ Refresh[
      Switch[currentScreen,
        "menu", creaHomepage[],
        "gioco", creaSchermataGioco[]
      ],
      TrackedSymbols :> {currentScreen}
    ],
    Full,
    Alignment -> {Center, Top}
  ];

  (* Finestra principale con centramento originale *)
  mainWindow = CreateDocument[
    {
      Cell[
        BoxData @ ToBoxes @ content,
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


 (* Schermata di gioco random - perfettamente centrata *)
  creaSchermataGioco[] := Column[
    {
      Spacer[{0, 50}],
      Style["PARTITA",
        FontSize -> 70,
        FontWeight -> Bold,
        FontColor -> Black,
        FontFamily -> "Consolas",
        TextAlignment -> Center
      ],
      
      Spacer[{0, 50}],
      
      Button["Torna al menu", cambiaSchermata["menu"],
        Background -> LightGray,
        Alignment -> Center
      ]
    },
    Alignment -> Center
  ];


End[];
EndPackage[];
