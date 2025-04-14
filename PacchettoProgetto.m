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
interfacciaGriglia[] := DynamicModule[
	{
		gridRowItems=4,(* Numero di colori da scegliere per tentativo*)
		gridColumnItems=9,(*Numero di tentativi*)
		gridItemsColors,(* Tabella di colori degli elementi della griglia*)
		gameGridFeedbackMsg="",(*Messaggio di feedback da click sulla griglia di gioco*)
		gameGridFeedbackMsgColor,(*Colore del messaggio di feedback da click sulla griglia di gioco*)
		turn = 1,(*Numero del tentativo*)
		colorsList={Red, Green, Blue, Yellow, Orange, Purple, Pink, Cyan},(*Lista di colori della palette di scelta*)
		colorsListFeedbackMsg=""(**),
		selectedItem={0,0} (* Elemento selezionato riga,colonna*)
	},

	gameGridFeedbackMsgColor = Black;(* Colore messaggio di Feedback*)
	gridItemsColors=Table[Black,{gridColumnItems},{gridRowItems}];
	
	Panel[
		Column[{
			Dynamic[ToString[selectedItem]],
			Dynamic[Style[gameGridFeedbackMsg, gameGridFeedbackMsgColor]],
			Spacer[10],
			Grid[
				Partition[
					Table[
						With[{x=col,y=row,id=gridRowItems*(row-1)+col},
							EventHandler[
								Dynamic@Graphics[
									{
									EdgeForm[
									  If[{y, x} === selectedItem,
									    Directive[Yellow, AbsoluteThickness[3]],
									    Directive[Black]
									   ]
									  ],
									If[y === turn,gridItemsColors[[y,x]],Opacity[0.2,Black] ],
									Disk[{0,0},1]
									(*If[y === turn,gridItemsColors[[y,x]],Opacity[0.2,Black] ]
									Text[Style[ToString[id],12,Bold],{0,0}]*)
									},
									ImageSize->{50,50}
								],
								{
								"MouseClicked" :> (
									If[y === turn, {
										selectedItem={y,x};
										(*gridItemsColors=Table[Black,{gridColumnItems},{gridRowItems}];*)
										(*gridItemsColors[[y,x]]=Red;*)
										gameGridFeedbackMsg = StringTemplate["Cliccato cerchio ID: `id`, Posizione: (`x`,`y`)"][<|"id"->id,"x"->x,"y"->y|>];
										 gameGridFeedbackMsgColor = Darker@Green;
									}]
									If[y != turn, {
										 (*gridItemsColors=Table[Black,{gridColumnItems},{gridRowItems}];*)
										 gameGridFeedbackMsg = StringTemplate["Stai selezionando elementi del turno `y`, completa il turno `turn`"][<|"y"->y,"turn"->turn|>];
										 gameGridFeedbackMsgColor = Red;
									}]
									)
								}
							]
						],
						{row,1,gridColumnItems},{col,1,gridRowItems}
					] // Flatten,
					gridRowItems
				],
				Spacings->{0,0}
			],
			Spacer[10],
			(* Color Palette *)
			Dynamic[Style[colorsListFeedbackMsg, Black]],
			Row[
			  Table[
				  With[{col=colorsCol},
					  EventHandler[
					  			Dynamic@Graphics[
								      {
								        EdgeForm[Black],
								        FaceForm[col],
								        Disk[{0, 0}, 1]
								      },
								      ImageSize -> 40
										],
										{
										"MouseClicked" :> (
												gridItemsColors[[Sequence @@ selectedItem]]=col;
												colorsListFeedbackMsg = StringTemplate["Stai selezionando il colore `color`"][<|"color"->col|>];
											)
										}
					  ]
				  ],
			    {colorsCol, colorsList}
			  ]
			]
			},
			 Alignment -> Center
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
		Size->{1000,Automatic}
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
	 Padding->0,
	 Size -> {screenWidth/3*2, screenHeight/3*2}
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
