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
avviaSchermataDiGioco[] := DynamicModule[
  {
    mainWindow, screenWidth, screenHeight,
    titleFontScale, currentScreen = "menu",
    modalitaSeed = False, modalitaCustom = False,
    content,
    customSeed = "", customColori = 6, customColonne = 4, customTurni = 10
  },

  (* Ottieni dimensioni schermo *)
  Quiet @ Check[
    {screenWidth, screenHeight} = FrontEndExecute @ FrontEnd`Value[FE`getScreenSize[]],
    {screenWidth, screenHeight} = {1920, 1080}
  ];
  
  colorLetters = {"r" (* red *), "o" (* orange *), "g" (* green *), "b" (* blue *), "v" (* violet *), "w" (* brown *), "a" (* gray *), "y" (* yellow *)};
  
  seedInserito = "";
  
  Int? numeroColori;
  Int? numeroColonne;
  Int? numeroTurni;

  titleFontScale = Min[screenWidth, screenHeight]/15;

  (* Funzione per cambiare schermata *)
  cambiaSchermata[nuovaSchermata_] := (
    currentScreen = nuovaSchermata;
    modalitaSeed = False;
    modalitaCustom = False
  );

(* Schermata seed con InputField personalizzato *)
mostraInserimentoSeed[] := Column[{
    Spacer[{0, 50}],
    Style["Inserisci un seed:", FontSize -> 18, FontFamily -> "Consolas"],

    Row[{
        Item[
            Framed[
                InputField[Dynamic[seedInserito], String, ImageSize -> {250, 21},  Appearance -> "Frameless", BaselinePosition-> Center],
                Background -> LightGray,
                FrameStyle -> None, 
                RoundingRadius -> 10, 
                FrameMargins -> {{10, 10}, {5, 5}},
                ImageSize -> Automatic
            ],
            ItemSize -> Automatic
        ],
        Spacer[15],
        ClickPane[
            Framed[
                Style["\[FilledRightTriangle]", FontSize -> 18, FontColor -> White],
                Background -> RGBColor[0, 0.5, 0], 
                FrameStyle -> None, 
                RoundingRadius -> 5,
                FrameMargins -> {{10, 10}, {5, 5}}, 
                ImageSize -> Automatic
            ],
            Function[(
			  customSeed = seedInserito;
			  seedInserito = "";
			  cambiaSchermata["gioco"]
			)]
        ]
    }, Alignment -> Center],
    
      Spacer[{0, 20}],
      
	ClickPane[
	  Framed[
	    Row[{
	      Style["\[LeftArrow]", White, FontSize -> 30, FontFamily -> "Consolas", Bold],
	      Spacer[10],
	      Style["MENU", White, FontSize -> 24, FontFamily -> "Consolas", Bold]
	    }, Alignment -> Center],
	    Background -> Darker[Blue], FrameStyle -> None, RoundingRadius -> 10,
	    FrameMargins -> {{15, 15}, {10, 10}}, ImageSize -> Automatic
	  ],
	  Function[cambiaSchermata["menu"]]
	]

}, Spacings -> 2, Alignment -> Center];



  (* Schermata custom *)
  mostraConfigurazioneCustom[] := Column[{
    Spacer[{0, 20}], 
    Style["Settings", FontSize -> 18, FontFamily -> "Consolas"],
    Column[{
    Item[
            Framed[
                InputField[Dynamic[numeroColori], Number, FieldHint->"COLORI", FieldHintStyle -> {Bold, FontSlant -> "Plain"}, ImageSize -> {100, 21},  Appearance -> "Frameless", BaselinePosition-> Center],
                Background -> LightGray,
                FrameStyle -> None, 
                RoundingRadius -> 10,
                FrameMargins -> {{10, 10}, {5, 5}},
                ImageSize -> Automatic
            ],
            ItemSize -> Automatic
        ],
        Spacer[{0,5}],
        Item[
            Framed[
                InputField[Dynamic[numeroColonne], Number, FieldHint->"COLONNE", FieldHintStyle->{Bold, FontSlant -> "Plain"}, ImageSize -> {100, 21},  Appearance -> "Frameless", BaselinePosition-> Center],
                Background -> LightGray,
                FrameStyle -> None, 
                RoundingRadius -> 10, 
                FrameMargins -> {{10, 10}, {5, 5}},
                ImageSize -> Automatic
            ],
            ItemSize -> Automatic
        ],
        Spacer[{0,5}],
        Item[
            Framed[
                InputField[Dynamic[numeroTurni], Number, FieldHint->"TURNI", FieldHintStyle->{Bold, FontSlant -> "Plain"}, ImageSize -> {100, 21},  Appearance -> "Frameless", BaselinePosition-> Center],
                Background -> LightGray,
                FrameStyle -> None, 
                RoundingRadius -> 10, 
                FrameMargins -> {{10, 10}, {5, 5}},
                ImageSize -> Automatic
            ],
            ItemSize -> Automatic
        ]
    }],
	ClickPane[
	  Framed[
	    Pane[
	      Style["\[FilledRightTriangle]", FontSize -> 25, FontColor -> White],
	      Alignment -> Center,
	      ImageSize -> {120, 32}
	    ],
	    Background -> RGBColor[0, 0.5, 0], 
	    FrameStyle -> None, 
	    RoundingRadius -> 10,
	    FrameMargins -> 0
	  ],
	  Function[
	    customColori = numeroColori;
	    numeroColori = Null;
	    customColonne = numeroColonne;
	    numeroColonne = Null;
	    customTurni = numeroTurni;
	    numeroTurni = Null;
	    cambiaSchermata["gioco"]
	  ]
	],

    Spacer[{0, 20}],
      
	ClickPane[
	  Framed[
	    Row[{
	      Style["\[LeftArrow]", White, FontSize -> 30, FontFamily -> "Consolas", Bold],
	      Spacer[10],
	      Style["MENU", White, FontSize -> 24, FontFamily -> "Consolas", Bold]
	    }, Alignment -> Center],
	    Background -> Darker[Blue], FrameStyle -> None, RoundingRadius -> 10,
	    FrameMargins -> {{15, 15}, {10, 10}}, ImageSize -> Automatic
	  ],
	  Function[cambiaSchermata["menu"]]
	]



  }, Spacings -> 2, Alignment -> Center];

  (* Homepage *)
  creaHomepage[] := Column[{
    Spacer[{0, 50}],
    Style["MASTERMIND MA MEGLIO", FontSize -> titleFontScale, FontWeight -> Bold,
      FontColor -> Black, FontFamily -> "Consolas", TextAlignment -> Center],
    Spacer[{0, 20}],
    Style["Made by Alessandro Modelli, Angelo Greco, Elia Friberg, Francesca Mazzetti, Gianpiero Tovo, Matteo Raggi",
      FontSize -> titleFontScale/5, FontFamily -> "Consolas", FontColor -> Gray, TextAlignment -> Center],

    Dynamic[Which[
      modalitaSeed, mostraInserimentoSeed[],
      modalitaCustom, mostraConfigurazioneCustom[],
      True,
      Column[{
      Spacer[{0, 125}], 
      Row[{
        ClickPane[
          Framed[
            Grid[{{
              Graphics[Text[Style["\|01f331", FontSize -> 24]], ImageSize -> 24],
              Style["SEED GAME", Black, FontFamily -> "Consolas", FontSize -> 24, Bold]
            }}, Alignment -> {Center, Center}, Spacings -> {1.5, 0}],
            Background -> LightGray, FrameStyle -> None, RoundingRadius -> 10,
            FrameMargins -> {{15, 15}, {15, 15}}, ImageSize -> Automatic
          ],
          Function[(
            modalitaSeed = True;
            modalitaCustom = False
          )]
        ],
        Spacer[40],
		ClickPane[
		  Framed[
		    Grid[{{
		      Graphics[Text[Style["\|01f3b2", Red, FontSize -> 24]], ImageSize -> 24],
		      Style["RANDOM GAME", Black, FontFamily -> "Consolas", FontSize -> 24, Bold]
		    }}, Alignment -> {Center, Center}, Spacings -> {1.5, 0}],
		    Background -> LightGray, FrameStyle -> None, RoundingRadius -> 10,
		    FrameMargins -> {{15, 15}, {15, 15}}, ImageSize -> Automatic
		  ],
		  Function[(
		    Module[{randomColors, seed},
		      randomColors = StringJoin[RandomSample[colorLetters, 4]];
		      seed = StringJoin["04", randomColors, "08"];
		      customSeed = seed;
		      cambiaSchermata["gioco"];
		    ]
		  )]
		],
        Spacer[40],
        ClickPane[
          Framed[
            Grid[{{
              Graphics[Text[Style["\|01f6e0\:fe0f", FontSize -> 24]], ImageSize -> 24],
              Style["CUSTOM GAME", Black, FontFamily -> "Consolas", FontSize -> 24, Bold]
            }}, Alignment -> {Center, Center}, Spacings -> {1.5, 0}],
            Background -> LightGray, FrameStyle -> None, RoundingRadius -> 10,
            FrameMargins -> {{15, 15}, {15, 15}}, ImageSize -> Automatic
          ],
          Function[(
            modalitaSeed = False;
            modalitaCustom = True
          )]
        ]
      }, Alignment -> Center],
      
      Spacer[{0, 125}],
      
      ClickPane[
	      Framed[
	        Style["ESCI", White, FontFamily -> "Consolas", FontSize -> 24, Bold],
	        Background -> Red, FrameStyle -> None, RoundingRadius -> 10,
	        FrameMargins -> {{15, 15}, {5, 5}}, ImageSize -> Automatic
	      ],
	      Function[NotebookClose[EvaluationNotebook[]]]
	  ]
    }, Alignment -> Center]
    ]]
  }, Alignment -> Center];

  (* Contenuto dinamico *)
  content = Pane[
    Dynamic @ Refresh[
      Switch[currentScreen,
        "menu", creaHomepage[],
        "gioco", creaSchermataGioco[customSeed]
      ],
      TrackedSymbols :> {currentScreen, modalitaSeed, modalitaCustom}
    ],
    Full,
    Alignment -> {Center, Top}
  ];

  (* Finestra principale *)
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



(* Lista dei colori usati da Mastermind *)
paletteColori = {Red, Green, Yellow, Blue, Orange, Brown, Purple, Cyan, Magenta};


(* === Funzione per generare il codice segreto da indovinare ===
Prende in input la lunghezza del codice da generare come intero, ed un booleano che ammette o meno la presenza di colori duplicati.
Ritorna tale codice. Esempio: {Red, Purple, Purple, Green} *)
generaCodiceSegreto[lunghezza_Integer, allowDuplicates_: True] := Module[
  {},
  
  (* Check di sicurezza: Se non accettiamo duplicati, la lunghezza non deve superare il numero di colori disponibili *)
  If[!allowDuplicates && lunghezza > Length[paletteColori],
	Return[$Failed, Module]  (* Ritorna $Failed. Da gestire in modo opportuno (ma \[EGrave] meglio evitare che accada del tutto) *)
  ];

  If[allowDuplicates,
    RandomChoice[paletteColori, lunghezza],  (* Con duplicati *)
    RandomSample[paletteColori, lunghezza]   (* Senza duplicati *)
  ]
];


(* === Funzione di feedback del tentativo === 
Prende in input il codice soluzione e il codice appena tentato dall'utente.
Ritorna il feedback ottenuto confrontando tali codici, via simboli 'feedbackEsatto', 'feedbackParziale' e 'feedbackAssente'.
Esesmpio: {feedbackParziale, feedbackEsatto, feedbackParziale, feedbackAssente} *)
feedbackTentativo[soluzione_List, tentativo_List] := Module[
  {
    feedback,          (* Lista dei feedback per ogni posizione *)
    marcatiSoluzione,  (* Booleani per segnare se un colore nella soluzione \[EGrave] gi\[AGrave] stato "matchato". Serve nel caso di colori ripetuti *)
    marcatiTentativo,  (* Booleani per segnare se un colore nel tentativo \[EGrave] gi\[AGrave] stato usato. Utile per non sovrascrivere feedback *)
    lunghezza          (* Lunghezza del codice da indovinare *)
  },
  
  (* Inizializzazioni *)
  lunghezza = Length[soluzione];  (* Rende il codice eventualmente scalabile *)
  feedback = ConstantArray[feedbackAssente, lunghezza];
  marcatiSoluzione = ConstantArray[False, lunghezza];
  marcatiTentativo = ConstantArray[False, lunghezza];

  (* === Match esatti === *)
  Do[
    If[tentativo[[i]] === soluzione[[i]],
      feedback[[i]] = feedbackEsatto;  (* Match completo *)
      marcatiSoluzione[[i]] = True;    (* Marca l'elemento della soluzione come usato *)
      marcatiTentativo[[i]] = True;    (* Marca l'elemento del tentativo come usato *)
    ],
    {i, lunghezza}
  ];

  (* === Match parziali === *)
  Do[
    If[!marcatiTentativo[[i]],               (* Solo se il colore non \[EGrave] stato gi\[AGrave] marcato come "Esatto" *)
      Do[
        If[!marcatiSoluzione[[j]] && tentativo[[i]] === soluzione[[j]],
          feedback[[i]] = feedbackParziale;  (* Match parziale *)
          marcatiSoluzione[[j]] = True;      (* Marca colore nella soluzione come usato *)
          marcatiTentativo[[i]] = True;      (* Marca colore del tentativo come usato *)
          Break[];                         (* Interrompe il ciclo interno per evitare doppi match *)
        ],
        {j, lunghezza}
      ];
    ],
    {i, lunghezza}
  ];

  feedback  (* Ritorna la lista dei feedback testuali *)
];


(* === Funzione che valuta un tentativo nella sua interezza === 
Prende in input il codice soluzione e il codice appena tentato dall'utente, nonch\[EGrave] le informazioni sui tentativi.
Chiama feedbackTentativo[], ed esegue il confronto tra soluzione e tentativo per dare la valutazione.
Ritorna la valutazione simbolica ('mastermindVittoria', 'mastermindSconfitta' o 'mastermindProsegui') assieme al feedback.
Esempio: {mastermindProsegui, {feedbackParziale, feedbackEsatto, feedbackParziale, feedbackAssente}} *)
valutaTentativo[soluzione_List, tentativo_List, maxTentativi_:10, tentativoCorrente_:1] := Module[
  {feedback = feedbackTentativo[soluzione, tentativo]},  (* Calcola immediatamente il feedback per il tentativo *)
  
  If[tentativo === soluzione,
    {mastermindVittoria, feedback},                      (* Caso vincita *)
    
    If[tentativoCorrente >= maxTentativi,
      {mastermindSconfitta, feedback},                   (* Caso sconfitta *)
      {mastermindProsegui, feedback}                     (* Caso intermedio, si continua *)
    ]
  ]
];


 (* Schermata di gioco random - perfettamente centrata *)
creaSchermataGioco[seed_] := Column[{
      Spacer[{0, 50}],
      Style["PARTITA",
        FontSize -> 70,
        FontWeight -> Bold,
        FontColor -> Black,
        FontFamily -> "Consolas",
        TextAlignment -> Center
      ],
      
      Spacer[{0, 25}],
      
      Style["Avvio con seed: " <> ToString[seed], FontSize -> 24],
      
      Spacer[{0, 25}],
      
      Button["Torna al menu", cambiaSchermata["menu"],
        Background -> LightGray,
        Alignment -> Center
      ]
    },
    Alignment -> Center
  ];


End[];
EndPackage[];
