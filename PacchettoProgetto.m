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


(* Interfaccia della griglia di gioco con selezione di un elemento del primo turno con turni successivi disabilitati*)
interfacciaGriglia[] := DynamicModule[{tries=4,lengthCode=9,colors,clickedInfo="", clickedInfoColor,turn = 1},

	clickedInfoColor = Black;(* Colore messaggio di Feedback*)
	colors=Table[Black,{lengthCode},{tries}];
	Panel[
		Column[{
			Dynamic[Style[clickedInfo, clickedInfoColor]],
			Grid[
				Partition[
					Table[
						With[{x=col,y=row,id=tries*(row-1)+col},
							EventHandler[
								Dynamic@Graphics[
									{
									EdgeForm[Black],
									If[y === turn,colors[[y,x]],Opacity[0.2,Black] ],
									Circle[{0,0},1],
									If[y === turn,colors[[y,x]],Opacity[0.2,Black] ],
									Text[Style[ToString[id],12,Bold],{0,0}]
									},
									ImageSize->{50,50}
								],
								{
								"MouseClicked" :> (
									If[y === turn, {
										colors=Table[Black,{lengthCode},{tries}];
										colors[[y,x]]=Red;
										clickedInfo = StringTemplate["Cliccato cerchio ID: `id`, Posizione: (`x`,`y`)"][<|"id"->id,"x"->x,"y"->y|>];
										 clickedInfoColor = Darker@Green;
									}]
									If[y != turn, {
										 colors=Table[Black,{lengthCode},{tries}];
										clickedInfo = StringTemplate["Stai selezionando elementi del turno `y`, completa il turno `turn`"][<|"y"->y,"turn"->turn|>];
										 clickedInfoColor = Red;
									}]
									)
								}
							]
						],
						{row,1,lengthCode},{col,1,tries}
					] // Flatten,
					tries
				],
				Spacings->{0,0}
			]
			}
		],
		Background->GrayLevel[0.9]
	]
]

(* Pannello customizzabile *)
CustomPanel[
	content_,
	OptionsPattern[{
		BackgroundColor->GrayLevel[0.95],
		BorderColor->GrayLevel[0.4],
		BorderThickness->2,
		CornerRadius->10,
		Padding->0.05,
		Size->{500,Automatic}
	}]
	]:=Module[
	{
		bg=OptionValue[BackgroundColor],
		border=OptionValue[BorderColor],
		thick=OptionValue[BorderThickness],
		radius=OptionValue[CornerRadius],
		pad=OptionValue[Padding],
		size=OptionValue[Size]
	},
	Graphics[
	{
		EdgeForm[{Thickness[thick],border}],
		FaceForm[bg],
		(*Rettangolo arrotondato con Radius custom*)
		Rectangle[{0,0},{1,1},RoundingRadius->radius],
		(*Content*)
		Inset[Style[content,Background->None],{0.5,0.5},Center,Scaled[{1-2 pad,1-2 pad}]]
	},
	PlotRange->{{0,1},{0,1}},
	PlotRangePadding->None,
	ImageSize->size,
	Frame->False,
	Axes->False]
]

(* aaaaaa *)
avviaSchermataDiGioco[] := Module[
  {screenWidth = 1920, screenHeight = 1080, mainWindow, fontScale, 
   content},
  
  (* 1. Metodo affidabile per ottenere le dimensioni dello schermo *)
  Quiet @ Check[
    {screenWidth, screenHeight} = 
     FrontEndExecute @ FrontEnd`Value[FE`getScreenSize[]],
    {screenWidth, screenHeight} = {1920, 1080}
  ];
  
  (* 2. Calcolo dimensione font adattiva *)
  fontScale = Min[screenWidth, screenHeight]/20;
  
  (* 3. Creazione contenuto con pulsante di chiusura *)
  content = Column[{
     Spacer[1],
     Style["MASTERMIND GIOCO", 
      FontSize -> fontScale, 
      FontWeight -> Bold, 
      FontColor -> White,
      FontFamily -> "Impact"],
     Spacer[1],
     Button["ESCI", 
      NotebookClose[EvaluationNotebook[]], 
      Background -> Red, 
      BaseStyle -> {FontSize -> fontScale/3, FontColor -> White, Bold},
      ImageSize -> {Automatic, fontScale/2}
     ],
     Spacer[1],
	 (* Content*)
	 CustomPanel[
		Column[
			{
			interfacciaGriglia[]
			}
		],
	 BackgroundColor->GrayLevel[0.9],
	 BorderColor->Black,
	 BorderThickness->0.005,
	 CornerRadius->0.05,
	 Padding->0
	 ]

  }, Center];
  
  (* 4. Creazione finestra principale *)
  mainWindow = NotebookPut @ Notebook[{
     Cell[BoxData @ ToBoxes @ Panel[
        content,
        Background -> Black,
        ImageSize -> {screenWidth, screenHeight}
     ],
     CellBracketOptions -> {"ShowCellBracket" -> False}]
   },
   (* Propriet\[AGrave] finestra *)
   WindowSize -> Full,
   WindowFrame -> "Frameless",
   WindowElements -> {},
   WindowTitle -> None,
   Background -> Black,
   Editable -> False,
   
   (* Gestione eventi *)
   NotebookEventActions -> {
     {"KeyDown", "Escape"} :> NotebookClose[EvaluationNotebook[]]
   }
  ];
  
  (* 5. Forza focus sulla finestra *)
  SelectionMove[mainWindow, All, Notebook];
  FrontEndExecute @ FrontEndToken[mainWindow, "MoveNext"];
  
  mainWindow
]


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


End[];
EndPackage[];
