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
DisplayTriviaQuestion::usage = "Displays a trivia question dialog with options. Returns True if correct answer is selected, False otherwise. Parameters: seed (Integer), questionsDataset (Dataset)";
LoadQuestionsFromCSV::usage = "Loads questions from CSV file into a Dataset. Parameter: path (String)";
InitializeQuestionInterface::usage = "Creates the main question interface window. Parameter: seed (Integer)";
ProvideHintFeedback::usage = "Provides feedback based on answer correctness. Parameter: guessedCorrectly (True|False)";
PrepareQuestionData::usage = "Selects and prepares question data. Returns {question, options, correctIndex}. Parameters: seed (Integer), questionsDataset (Dataset)";



Begin["`Private`"];

(* Global variables that need to be shared across functions *)

timeToWait = 2.0;
paneSize={500,80};
imageSize= {200, 40};
windowSize= {600, Automatic};
(* Ricorda di documentare ogni riga di codice: funzionalit\[AGrave],
variabili di input, variabili di lavoro, variabili di output, spiegazione dei singoli passaggi *)


(* Menu di avvio *)
(* Menu di avvio *)
avviaSchermataDiGioco[] := DynamicModule[
 {
  mainWindow,
  screenWidth,
  screenHeight,
  titleFontScale,
  currentScreen="menu",
  content,
  customSeed,
  customColori=6,
  customColonne=4,
  customTurni=8,
  allowDuplicates=True,
  seedInserito=""
 },
	
	
  aggiornaDimensioniSchermo[] := ( 
   (* Ottieni dimensioni schermo *)
	Quiet @ Check[
	  {screenWidth, screenHeight}=FrontEndExecute @ FrontEnd`Value[FE`getScreenSize[]],
	  {screenWidth, screenHeight}={1920, 1080}
	];
   titleFontScale=Min[screenWidth, screenHeight]/15;
   );
    
   aggiornaDimensioniSchermo[];
    
   (* Funzione per cambiare schermata *)
   cambiaSchermata[nuovaSchermata_] := ( 
     aggiornaDimensioniSchermo[];
	 currentScreen=nuovaSchermata;
   );

  (* Homepage *)
  creaHomepage[] := Column[{
        
    Spacer[{0, 50}],
    
    (*TITOLO ORIGINALE*)
    (*Dynamic @ Style[labels["titoloGioco"],
      FontSize->titleFontScale,
      FontWeight->Bold,
      FontColor->Black,
      FontFamily->"Consolas",
      TextAlignment->Center
    ],*)
    
    (*TITOLO COLORATO*)
    With[
     {stringa=labels["titoloGioco"]},
     Style[
      Row @ Table[
       With[
        {
         char=StringTake[stringa, {i}],
         color=RandomColor[]
        },
         Style[char, FontColor->color]
       ],
      {i, StringLength[stringa]}
      ],
     FontSize->titleFontScale,
     FontWeight->Bold,
     FontFamily->"Consolas",
     TextAlignment->Center
     ]
    ],
        
    Spacer[{0, 20}],
        
    Dynamic @ Row[
     {
      Style["Made with ", FontSize->titleFontScale/5, FontFamily->"Consolas", FontColor->Gray],
      Style["\:2665 ", FontSize->titleFontScale/5, FontFamily->"Consolas", FontColor->Red],
      Style[labels["fattoDa"], FontSize->titleFontScale/5, FontFamily->"Consolas", FontColor->Gray]
     },
    Alignment->Center
    ],
        
    Spacer[{0, 50}],
    
    Style[labels["inserisciSeed"], FontSize->18, FontFamily->"Consolas"],
	
	Spacer[{0, 25}],
	
	Row[{
	 ClickPane[
	  Framed[
	    Style[labels["randomSeed"], FontSize->18, FontColor->White],
		Background->Darker[Blue], 
		FrameStyle->None, 
		RoundingRadius->5,
		FrameMargins->{{10, 10}, {5, 5}}, 
		ImageSize->Automatic 
	  ],
	  Function[( 
	    seedInserito=RandomInteger[{1, 9999999999}];
	  )]
	 ],
		    
	 Spacer[15],
      
     Item[
      Framed[
       InputField[
         Dynamic[seedInserito],
         Number,
         FieldHint->labels["placeholderSeed"],
         FieldHintStyle->{Italic},
         ImageSize->{250, 21},
         Appearance->"Frameless",
         BaselinePosition->Center,
         ContinuousAction->True (* consente controllo costante per dis/abilitare tasto play *)
       ],
      Background->LightGray,
      FrameStyle->None,
      RoundingRadius->10,
      FrameMargins->{{10, 10}, {5, 5}},
      ImageSize->Automatic
      ],
     ItemSize->Automatic 
     ],
       
     Spacer[15],
       
     Dynamic[
      If[IntegerQ[seedInserito] && seedInserito > 0,
        (* seed inserito correttamente -> tasto abilitato *)
        ClickPane[
         Framed[
           Style[labels["play"], FontSize->18, FontColor->White],
           Background->RGBColor[0, 0.5, 0],
           FrameStyle->None,
           RoundingRadius->5,
           FrameMargins->{{10, 10}, {5, 5}},
           ImageSize->Automatic
         ],
         Function[( 
           partitaInCorso=True;
           customSeed=seedInserito;
           seedInserito="";
           (*SeedRandom[customSeed];*)
           cambiaSchermata["gioco"];
         )]
        ],
        
        (* seed non inserito -> tasto disabilitato  *)
        Framed[
          Style[labels["play"], FontSize->18, FontColor->GrayLevel[0.8]],
          Background->RGBColor[0, 0.5, 0],
          FrameStyle->None,
          RoundingRadius->5,
          FrameMargins->{{10, 10}, {5, 5}},
          ImageSize->Automatic
        ]
      ]
     ]
    },
    Alignment->Center
    ],
    
    Spacer[{0, 25}],
        
    Column[{
     Row[{
       Style[labels["nTurni"], FontSize->14, FontFamily->"Consolas", Bold],
        
       Spacer[20],
	    
	   SetterBar[
	    Dynamic[customTurni],
	    Table[
	      i->Style[ToString[i], FontFamily->"Consolas", Bold],
	      {i, 6, 14}
	    ],
	   Appearance->"Horizontal"
	   ]  
     }],
     
     Row[{
       Style[labels["nCombinazione"], FontSize->14, FontFamily->"Consolas", Bold],
                
       Spacer[53],
                
       SetterBar[
        Dynamic[customColonne],
        Table[
          j->Style[ToString[j], FontFamily->"Consolas", Bold],
          {j, 3, 7} 
        ],
       Appearance->"Horizontal"
       ]
     }],
     
     Row[{
       Style[labels["allowDuplicates"], FontSize->14, FontFamily->"Consolas", Bold],
                
       Spacer[93],
                
       Checkbox[Dynamic[allowDuplicates]]
     }]
    }],

    Spacer[{0, 125}],
      
    ClickPane[
	 Framed[
	   Style[labels["esci"], White, FontFamily->"Consolas", FontSize->24, Bold],
	   Background->Red,
	   FrameStyle->None,
	   RoundingRadius->10,
	   FrameMargins->{{15, 15}, {5, 5}},
	   ImageSize->Automatic
	 ],  
	 Function[
	   seedInserito=Null;
	   NotebookClose[EvaluationNotebook[]]
	 ]
	]
  },
  Alignment->Center
  ];

  (* Contenuto dinamico *)
  content=Pane[
   Dynamic @ Refresh[
    Switch[currentScreen,
      "menu", creaHomepage[],
      "gioco", creaSchermataGioco[customSeed, customTurni, customColonne, allowDuplicates, titleFontScale]
    ],
   TrackedSymbols:>{currentScreen, customTurni, customColonne, titleFontScale}
   ],
  Full,
  Alignment->{Center, Top}
  ];
    
  (* Finestra principale *)
  mainWindow=CreateDocument[
   {
    Cell[
      BoxData @ ToBoxes @ content,
      "Output",
      ShowCellBracket->False,
      CellMargins->{{0, 0}, {0, 0}}
    ]
   },
    
    WindowSize->Full,
    WindowFrame->"Frameless",
    WindowElements->{},
    Background->White,
    Editable->False,
    Deployed->True,
    WindowMargins->{{0, 0}, {0, 0}},
    NotebookEventActions->{
      {"KeyDown", "Escape"}:>NotebookClose[EvaluationNotebook[]]
    }
  ];
  
  mainWindow
]


(* Lista dei colori usati da Mastermind *)
paletteColori={Red, Green, Yellow, Blue, Orange, Brown, Purple, Cyan, Magenta, White, Gray, Black};
(* Stato della partita *)
partitaInCorso=True

labels=translations = <|
	"titoloGioco" -> "TRIVIA MASTERMIND",
	"fattoDa" -> "by Alessandro Modelli, Angelo Greco, Elia Friberg, Francesca Mazzetti, Gianpiero Tovo, Matteo Raggi",
	"inserisciSeed" -> "Insert seed: ",
	"placeholderSeed" -> "Write a numeric seed...",
	"play" -> "\[FilledRightTriangle]",
	"randomSeed" -> "\:21bb",
	"nTurni" -> "Turns",
	"nCombinazione" -> "Cipher Length",
	"allowDuplicates"->"Repeating colors",
	"esci" -> "EXIT",
	"partita" -> "GAME - DELETE ME",
	"seedSelezionato" -> "GAME STARTED WITH SEED: ",
	"colori" -> "Colors",
	"combinazione" -> "Cipher",
	"suggerimenti" -> "Feedback",
	"azione" -> "Actions",
	"restartVinto"->"YOU WON! Want to crack the same code?",
	"restartPerso"->"You lost... Want to crack the same code?",
	"vai"->"CHECK",
	"menu"->"BACK TO MENU",
	"trivia"->"NEED A TIP?",
	"idea"->"\[LightBulb]"
	|>;


(* === Funzione per generare il codice segreto da indovinare ===
Prende in input la lunghezza del codice da generare come intero, il seed, ed un booleano che ammette o meno la presenza di colori duplicati.
Ritorna tale codice. Esempio: {Red, Purple, Purple, Green} *)
generaCodiceSegreto[seed_, lunghezza_Integer, allowDuplicates_] := Module[
    {},
    SeedRandom[seed]  (* Set del generatore pseudorandom *)
  
    (* Check di sicurezza: Se non accettiamo duplicati, la lunghezza non deve superare il numero di colori disponibili *)
    If[!allowDuplicates && lunghezza > Length[paletteColori],
	    Return[$Failed, Module]  (* Ritorna $Failed. Conviene evitare che accada del tutto dalle impostazioni iniziali *)
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
    lunghezza=Length[soluzione];  (* Rende il codice eventualmente scalabile *)
    feedback=ConstantArray[feedbackAssente, lunghezza];
    marcatiSoluzione=ConstantArray[False, lunghezza];
    marcatiTentativo=ConstantArray[False, lunghezza];

    (* === Match esatti === *)
    Do[
        If[tentativo[[i]] === soluzione[[i]],
            feedback[[i]]=feedbackEsatto;  (* Match completo *)
            marcatiSoluzione[[i]]=True;    (* Marca l'elemento della soluzione come usato *)
            marcatiTentativo[[i]]=True;    (* Marca l'elemento del tentativo come usato *)
        ],
        {i, lunghezza}
    ];

    (* === Match parziali === *)
    Do[
        If[!marcatiTentativo[[i]],               (* Solo se il colore non \[EGrave] stato gi\[AGrave] marcato come "Esatto" *)
            Do[
                If[!marcatiSoluzione[[j]] && tentativo[[i]] === soluzione[[j]],
                    feedback[[i]]=feedbackParziale;  (* Match parziale *)
                    marcatiSoluzione[[j]]=True;      (* Marca colore nella soluzione come usato *)
                    marcatiTentativo[[i]]=True;      (* Marca colore del tentativo come usato *)
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
valutaTentativo[soluzione_List, tentativo_List, maxTentativi_:8, tentativoCorrente_:1] := Module[
{
    feedback=feedbackTentativo[soluzione, tentativo] (* Calcola immediatamente il feedback per il tentativo *)
},  
  
    If[tentativo === soluzione,
        {mastermindVittoria, feedback},                      (* Caso vincita *)
    
        If[tentativoCorrente >= maxTentativi,
            {mastermindSconfitta, feedback},                   (* Caso sconfitta *)
            {mastermindProsegui, feedback}                     (* Caso intermedio, si continua *)
        ]
    ]
];


(* === Funzione che seleziona automaticamente il prossimo piolo con cui interagire === 
Prende in input il piolo selezionato corrente, la lista dei tentativi e la lunghezza massima del tentativo, e ritorna il nuovo piolo selezionato.
Normalmente, passa sempre al successivo. Se sono stati rimossi dei colori e il successivo \[EGrave] gi\[AGrave] colorato, passa al primo vuoto successivo.
In caso non ci siano successivi, non fa nulla. *)
vaiAlProssimoPallinoVuoto[selectedItem_, tentativoList_, lunghezzaCombinazione_] := Module[
	{next, newSelectedItem},
	
	(* Cerca il primo piolo vuoto sul lato destro della selezione *)
	next = SelectFirst[
		Range[selectedItem[[2]] + 1, lunghezzaCombinazione],  (* Elementi a destra *)
		(tentativoList[[#]] === None)&,
		Missing["NotFound"]  (* Se non trova pioli vuoti, ritorna 'Missing["NotFound"]' (default, aggiunto per chiarezza)*)
	];
	(* Se ancora non \[EGrave] stato trovato un piolo vuoto, controlla anche alla sinistra *)
	If[next === Missing["NotFound"],
		next = SelectFirst[
			Range[1, selectedItem[[2]] - 1],  (* Elementi a Sinistra *)
			(tentativoList[[#]] === None)&,
			Missing["NotFound"]
		];
	];
	
	newSelectedItem = selectedItem;  (* Inizializza la nuova selezione *)
	If[next =!= Missing["NotFound"],
		newSelectedItem[[2]] = next  (* Se si \[EGrave] trovato un vuoto, ritorna la nuova selezione*)
    ];
    newSelectedItem  (* Ritorna la nuova selezione *)
]


(* Interfaccia della griglia di gioco con selezione di un elemento del primo turno con turni successivi disabilitati*)
interfacciaGriglia[seed_, lunghezzaCombinazione_, numeroTentativi_, allowDuplicates_, triviaData_] := DynamicModule[
{
	gridItemsColors=Table[Opacity[0.2, Black],{numeroTentativi},{lunghezzaCombinazione}],(* Tabella per memorizzare i colori degli elementi, inizialmente tutta nera(opacit\[AGrave] a 0.2)*)
	hintFeedbackHistory = ConstantArray[{}, numeroTentativi],
	turn = 1,(*Numero del tentativo*)
	colorsList=paletteColori,(*Lista di colori della palette di scelta*)
	selectedItem={1,1}, (* Elemento selezionato riga,colonna*)
	soluzioneList=generaCodiceSegreto[seed, lunghezzaCombinazione, allowDuplicates], (* Combinazione segreta *)
	tentativoList=ConstantArray[None, lunghezzaCombinazione], (* Tentativo corrente *)
	valutazioneTentativo={},
	questionCounter=0
},
	    Framed[
	    Column[{

				Column[{  
	   Dynamic[
	    Which[ 
		 Length[valutazioneTentativo]>0 && valutazioneTentativo[[1]] === mastermindSconfitta,
		  (
		   partitaInCorso=False;
		   Row[{
			 (*Bottone TRY*)
			 ClickPane[
			  Framed[
			   Grid[{{
			     Style[labels["play"], FontSize->12, White],
			     Style[labels["restartPerso"], White, FontSize->12, FontFamily->"Consolas"]
			   }},
			   Alignment->{Center, Center}
			   ],
			  Background->Blue,
			  FrameStyle->None,
			  RoundingRadius->10,
			  FrameMargins->{{10, 10}, {5, 5}},
			  ImageSize->Automatic
			  ],
			  Function[			    
			    gridItemsColors=Table[Opacity[0.2, Black],{numeroTentativi},{lunghezzaCombinazione}];
				hintFeedbackHistory=ConstantArray[{}, numeroTentativi];
				turn=1;(*Numero del tentativo*)
				colorsList=paletteColori;(*Lista di colori della palette di scelta*)
				selectedItem={1,1}; (* Elemento selezionato riga,colonna*)
				(*SeedRandom[seed];*)
				soluzioneList=generaCodiceSegreto[seed, lunghezzaCombinazione, allowDuplicates]; (* Combinazione segreta *)
				tentativoList=ConstantArray[None, lunghezzaCombinazione]; (* Tentativo corrente *)
				valutazioneTentativo={};
				partitaInCorso=True
			  ]
             ]
		   }]
		  ),
		 Length[valutazioneTentativo]>0 && valutazioneTentativo[[1]] === mastermindVittoria,
		  (
		   partitaInCorso=False;
		   Row[{
		     (*Bottone TRY*)
			 ClickPane[
			  Framed[
			   Grid[{{
			     Style[labels["play"], FontSize->12, White],
				 Style[labels["restartVinto"], White, FontFamily->"Consolas", FontSize->12]
			   }},
			   Alignment->{Center, Center}
			   ],
			  Background->RGBColor[0, 0.5, 0],
			  FrameStyle->None,
			  RoundingRadius->10,
			  FrameMargins->{{10, 10}, {5, 5}},
			  ImageSize->Automatic
			  ],
			  Function[
			    gridItemsColors=Table[Opacity[0.2, Black],{numeroTentativi},{lunghezzaCombinazione}];
				hintFeedbackHistory=ConstantArray[{}, numeroTentativi];
				turn=1;(*Numero del tentativo*)
				colorsList=paletteColori;(*Lista di colori della palette di scelta*)
				selectedItem={1,1}; (* Elemento selezionato riga,colonna*)
				(*SeedRandom[seed];*)
				soluzioneList=generaCodiceSegreto[seed, lunghezzaCombinazione, allowDuplicates]; (* Combinazione segreta *)
				tentativoList=ConstantArray[None, lunghezzaCombinazione]; (* Tentativo corrente *)
				valutazioneTentativo={};
				partitaInCorso=True
			  ] 
             ]
		   }]
		  ),
		 Length[valutazioneTentativo]>0 && valutazioneTentativo[[1]] === mastermindProsegui,
		  (
		   Row[{
		     (*Bottone TRIVIA*)
		     ClickPane[
		      Framed[
		       Grid[{{
		         Style[labels["idea"], FontSize->12, Yellow],
				 Style[labels["trivia"], White, FontFamily->"Consolas", FontSize->12]
		       }},
		       Alignment->{Center, Center}
		       ],
		      Background->Purple,
			  FrameStyle->None,
			  RoundingRadius->10,
			  FrameMargins->{{10, 10}, {5, 5}},
			  ImageSize->Automatic
		      ],
		      Function[ (*DA CAMBIARE*)
		        gridItemsColors=Table[Opacity[0.2, Black],{numeroTentativi},{lunghezzaCombinazione}];
				hintFeedbackHistory=ConstantArray[{}, numeroTentativi];
				turn=1;(*Numero del tentativo*)
				colorsList=paletteColori;(*Lista di colori della palette di scelta*)
				selectedItem={1,1}; (* Elemento selezionato riga,colonna*)
				(*SeedRandom[seed];*)
				soluzioneList=generaCodiceSegreto[seed, lunghezzaCombinazione, allowDuplicates]; (* Combinazione segreta *)
				tentativoList=ConstantArray[None, lunghezzaCombinazione]; (* Tentativo corrente *)
				valutazioneTentativo={};
				partitaInCorso=True
		      ] 
		     ]
		   }]
		  ),
		 True, Style["", FontSize->0]
		],
	   TrackedSymbols:>{valutazioneTentativo}
	   ]
	 }],
				
				Spacer[3],
				
				(*Header*)
				Pane[
				  Grid[{
				    {
				      Style[labels["colori"], FontSize -> 14, FontColor -> Black, FontFamily -> "Consolas"],
				      Style[labels["combinazione"], FontSize -> 14, FontColor -> Black, FontFamily -> "Consolas"],
				      Style[labels["suggerimenti"], FontSize -> 14, FontColor -> Black, FontFamily -> "Consolas"],
				      Style[labels["azione"], FontSize -> 14, FontColor -> Black, FontFamily -> "Consolas"]
				    }
				  },
				  Alignment -> Center,
				  ItemSize -> All,
				  Spacings -> {lunghezzaCombinazione+1.5, 1}
				  ],
				  Alignment -> Center
				],
				
				Spacer[2],
				
				(* Content *)
				Row[{
					Spacer[20],
					
					(*Palette colori*)
					Grid[
					 Partition[
					  Table[
						  With[{col=colorsCol},
							  EventHandler[
					  			Dynamic @ Graphics[
								      {
								          EdgeForm[Black],
								          FaceForm[col],
								          Disk[{0, 0}, 1]
								      },
								      ImageSize->35
									],
									{
										"MouseClicked":>(
											gridItemsColors[[Sequence @@ selectedItem]]=col;
											tentativoList[[selectedItem[[2]]]]=col;
											(* Passa la selezione al prossimo pallino indiscriminatamente *)
											(*If[selectedItem[[2]] < lunghezzaCombinazione, selectedItem[[2]]=selectedItem[[2]]+1]*)
											(* Passa la selezione al prossimo pallino vuoto *)
											selectedItem = vaiAlProssimoPallinoVuoto[selectedItem, tentativoList, lunghezzaCombinazione];
										)
									}
								]
							],
					        {colorsCol, colorsList}
						],
						2 (* due colonne *)
						],
						Spacings->{1, 1},
						Alignment->Center
					],
					
					Spacer[80],
					
					(*Griglia di gioco*)
					Grid[
						  Table[
						      With[{x = row},
						          Append[
						              Table[
						                  With[{y=col, id=lunghezzaCombinazione*(row-1)+col},
						                      EventHandler[
						                          Dynamic @ Graphics[
						                          {
						                              EdgeForm[
						                                  If[{x, y} === selectedItem,
						                                      Directive[Black, AbsoluteThickness[1]], None(*,
						                                      Directive[Black]*)
						                                  ]
						                              ],
						                              If[x<=turn, gridItemsColors[[x, y]], Opacity[0.1, Black]],
						                              Disk[{0, 0}, 1]
						                          },
						                          ImageSize->{35, 35}
						                          ],
						                      {
						                          "MouseClicked":>(
												    If[x === turn,
												        If[tentativoList[[y]] =!= None,  (* Se clicchi un colore nel tentativo, rimuovilo *)
												            (
												                selectedItem = {x, y};
												                tentativoList[[y]] = None;
												                gridItemsColors[[x, y]] = Opacity[0.2, Black];
												            ),
												            (
												                (* Altrimenti seleziona il pallino vuoto normalmente *)
												                selectedItem = {x, y};
												            )
												        ]
												    ]
												)
						                      }
						                      ]
						                  ],
						                  {col, 1, lunghezzaCombinazione}
						              ],
							      
							          Row[{
							          
							          Spacer[20],
							          
							          (* 2x2 FEEDBACK GRID *)
							          Dynamic @ Module[
							            {
										    feedbackSymbolsForDisplay,
										    feedbackColors
										  },
										  feedbackSymbolsForDisplay = If[hintFeedbackHistory[[x]] =!= {},
										      hintFeedbackHistory[[x]][[All, 2]], (* Extract all second elements (feedback symbols) *)
										      ConstantArray[feedbackAssente, lunghezzaCombinazione] (* Default to all 'feedbackAssente' or 'None' if turn not played *)
										  ];
										
										  feedbackColors = feedbackSymbolsForDisplay /. {
										      feedbackEsatto -> RGBColor[0.57,1,0.05],  
										      feedbackParziale -> RGBColor[1,0.85,0],   
										      feedbackAssente -> None (* Or whatever your 'None' color is for feedback pegs *)                  
										  };
						              Style[
									      Grid[
										      {Table[
											      Graphics[
											          {EdgeForm[Gray], FaceForm[hint], Disk[{0, 0}, 1]}, 
											          ImageSize -> 15
											      ],
											      {hint, feedbackColors}
											  ]},
											  Alignment -> Center
										  ],
										  Selectable -> False,
	                                       Editable -> False					                
									  ]
							          ],
							          
							          Spacer[50],
							          
							          Dynamic[
								          If[x === turn,
									      (*Bottone TRY*)
									          ClickPane[
										          Framed[
										              Grid[{
										              {
										                Style["\|01f3ae", FontSize -> 10],
										                Style[labels["vai"], White, FontFamily -> "Consolas", FontSize -> 12, Bold]
										              }
										            },
										            Alignment -> {Center, Center}, Spacings -> {1, 0}
										            ],
										            Background -> Orange,
										            FrameStyle -> None,
										            RoundingRadius -> 10,
										            FrameMargins -> {{10, 10}, {10, 10}},
										            ImageSize -> Automatic
										          ],
										          Function[
													If[partitaInCorso,
													  (
													    valutazioneTentativo = valutaTentativo[soluzioneList, tentativoList, numeroTentativi, turn];
													    
													    (* --- New way to populate hintFeedbackHistory --- *)
													    Module[{currentTurnFeedbackSymbols = valutazioneTentativo[[2]], combinedTurnData},
													      combinedTurnData = Table[
													        {tentativoList[[i]], currentTurnFeedbackSymbols[[i]]}, (* {guessedColor, feedbackSymbol} *)
													        {i, Length[tentativoList]}
													      ];
													      hintFeedbackHistory[[turn]] = combinedTurnData; 
													    ];
													    (* --- End of new population logic --- *)
													
													    If[valutazioneTentativo[[1]] === mastermindProsegui, turn++];
													    selectedItem = {turn, 1}; 
													    tentativoList = ConstantArray[None, lunghezzaCombinazione];
													  )
													]
													]
		                                       ],
												Framed[
										              Grid[{
										              {
										                Style["\|01f3ae", FontSize -> 10, FontColor->Directive[GrayLevel[0.9], Opacity[0]]],
										                Style[labels["vai"], FontFamily -> "Consolas", FontSize -> 12,FontColor->GrayLevel[0.9], Bold]
										              }
										            },
										            Alignment -> {Center, Center}, Spacings -> {1, 0}
										            ],
										            Background -> GrayLevel[0.9],
										            FrameStyle -> None,
										            RoundingRadius -> 10,
										            FrameMargins -> {{10, 10}, {10, 10}},
										            ImageSize -> Automatic
										         ]
											]
							            ]
							                 Spacer[50], (* Spacer before Hint Button *)

											Dynamic[
											    If[x === turn && partitaInCorso && triviaData =!= $Failed,
											        (* Hint Button Active *)
											        ClickPane[
											            Framed[
											                Grid[{
											                  {
											                    Style["\|01f4a1", FontSize -> 10], (* Hint Icon *)
											                    Style["HINT", White, FontFamily -> "Consolas", FontSize -> 12, Bold] (* Hint Label *)
											                  }
											                },
											                Alignment -> {Center, Center}, Spacings -> {1, 0}
											                ],
											                Background -> Blue, (* Changed background color *)
											                FrameStyle -> None,
											                RoundingRadius -> 10,
											                FrameMargins -> {{10, 10}, {10, 10}},
											                ImageSize -> Automatic
											            ],
											            Function[ Module[{correct}, (* Hint Action *)
											            Print[hintFeedbackHistory];
											                    correct = DisplayTriviaQuestion[seed + questionCounter, triviaData, CalcolaHintSemplice[hintFeedbackHistory, soluzioneList];];
											                    questionCounter++;
											                ]
											            ],
											            Method -> "Queued" 
											        ],
											        (* Hint Button Inactive *)
											        Framed[
											            Grid[{
											              {
											                Style["\|01f4a1", FontSize -> 10, FontColor->Directive[GrayLevel[0.7], Opacity[0]]], (* Dimmed Icon *)
											                Style["HINT", FontFamily -> "Consolas", FontSize -> 12,FontColor->Directive[GrayLevel[0.7], Opacity[0]], Bold] (* Dimmed Label *)
											              }
											            },
											            Alignment -> {Center, Center}, Spacings -> {1, 0}
											            ],
											            Background -> GrayLevel[0.9], (* Standard inactive background *)
											            FrameStyle -> None,
											            RoundingRadius -> 10,
											            FrameMargins -> {{10, 10}, {10, 10}},
											            ImageSize -> Automatic
											        ]
											    ]
											] (* End Hint Button Dynamic *)
							        }
						         ]
						      ]
						    ],
						    {row, 1, numeroTentativi}
						](*,
					ItemSize->All,
					Alignment->Center,
					Spacings->{2, 1}*)
					]
				},
				Alignment -> {{Left, Center, Center, Right}}(* align each column *)
				(*Background-> Red   DEBUG*)
			    ],
			    Spacer[20]
			},
			Alignment -> Center
			]
			,
            Background->GrayLevel[0.9],
            FrameStyle->None,
            RoundingRadius->15,
            FrameMargins->{{15, 15}, {5, 5}},
            ImageSize->Automatic
            ]
]

(* Schermata di gioco random - perfettamente centrata *)
creaSchermataGioco[seed_, tentativi_, combinazione_, allowDuplicates_, fontSize_] := 
DynamicModule[{paletteRandom, triviaData},
    
    (*SeedRandom[seed];*)
    (*paletteRandom=Table[RandomColor[], {12}];*)
    triviaData = Check[
        LoadQuestionsFromCSV["science-technology.csv"], (* <<< ADJUST PATH IF NEEDED *)
        Print["\:274c Failed to load trivia questions! Hint button will be disabled."];
        $Failed
    ];

    Pane[
      Column[{
      
        Panel[
      Column[{
        (*Style[labels["partita"], FontSize->fontSize/2, FontFamily->"Consolas", Bold],*)
        Style[labels["seedSelezionato"] <> ToString[seed], FontSize->12, FontFamily->"Consolas", FontColor->Red, Bold]
      },
      Alignment->Center
      ],
     Background->White
     ],
            
     Dynamic[
      Pane[
        interfacciaGriglia[seed, combinazione, tentativi, allowDuplicates, triviaData],
        {Automatic, Scaled[0.7]}, (* massimo 80% in altezza *)
        Scrollbars->False,
        Alignment->Center 
      ]
     ],
    
    ClickPane[
      Framed[
        Style[labels["menu"], White, FontSize->12, FontFamily->"Consolas", Bold],
        Background->Red,
        FrameStyle->None,
        RoundingRadius->10,
        FrameMargins->{{15, 15}, {5, 5}},
        ImageSize->Automatic
      ],  
      Function[
        cambiaSchermata["menu"]
      ]
     ]
    
},
Alignment -> Center
],
Alignment->Center,
ImageSize->Scaled[1] (* prende tutto lo schermo *)
]
];

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
  
  dataset
];


(*
  Displays a trivia question dialog with multiple choice options
  @param seed: Integer used to select and shuffle the question
  @param questionsDataset: Dataset containing all trivia questions
  @return: True if correct answer selected, False otherwise
*)
(* Assumes PrepareQuestionData is defined in the package *)
(* Assumes global/config variables like paneSize, imageSize are defined *)
(* Assumes hintData is calculated BEFORE calling this function and passed in *)

DisplayTriviaQuestion[seed_Integer, questionsDataset_, hintData_] := Module[
  {
   (* Outer Module Variables *)
   questionWindow, 
   result = $Failed,         (* Final result to return *)
   dialogOpen = True,        (* Flag to control waiting loop *)
   
   (* Data prepared once before creating the dialog *)
   initialQuestionData, 
   initialOptionsData, 
   initialCorrectIndexData
  },

  (* Prepare question data *outside* the DynamicModule *)
  {initialQuestionData, initialOptionsData, initialCorrectIndexData} = PrepareQuestionData[seed, questionsDataset];

  (* Create the dialog *)
  questionWindow = CreateDialog[
    DynamicModule[
      { 
        (* Internal State Variables *)
        displayState = "question", (* Possible states: "question", "correct_show_hint", "incorrect_show_message" *)
        internalResult = $Failed, (* Stores True/False based on the answer *)
        
        (* Local copies initialized from outer scope *)
        localQuestion = initialQuestionData,
        localOptions = initialOptionsData,
        localCorrectIndex = initialCorrectIndexData
      }, 
      
      Dynamic@Refresh[
        Switch[displayState,

          "question", 
           (* --- Show Question View --- *)
           Column[{
             Pane[
               Style[localQuestion["Question"], 16, Bold, TextAlignment -> Center], 
               ImageSize -> paneSize, (* Use configured paneSize *)
               Scrollbars -> False, Alignment -> Center
             ],
             Spacer[20],
             Grid[
               Partition[
                 MapIndexed[
                   Function[{optionText, optionIndex},
                     (* Inner DynamicModule for button visual state *)
                     DynamicModule[{clicked = False, isCorrect = Null, position = First[optionIndex]}, 
                       Button[
                         optionText,
                         (* --- Button Action --- *)
                         isCorrect = (position == localCorrectIndex); 
                         clicked = True;
                         internalResult = isCorrect; (* Store the result internally *)
                         
                         (* Change the view based on correctness *)
                         If[isCorrect,
                           displayState = "correct_show_hint" (* Go to hint view *)
                         ,
                           displayState = "incorrect_show_message" (* Go to incorrect message view *)
                         ];
                         (* --- End Button Action --- *)
                         , 
                         Background -> Dynamic[If[clicked, If[isCorrect, Green, Red], White]],
                         ImageSize -> imageSize, (* Use configured imageSize *)
                         BaseStyle -> {FontColor -> Black, FontWeight -> Bold, FontFamily -> "Arial", FontSize -> 14},
                         FrameMargins -> 12
                       ] (* End Button *)
                     ] (* End Inner DynamicModule *)
                   ], (* End Function *)
                   localOptions (* Use local options data *)
                 ], (* End MapIndexed *)
                 UpTo[Ceiling[Length[localOptions]/2]] 
               ], (* End Partition *)
               Spacings -> {1, 1}, Alignment -> Center
             ] (* End Grid *)
           }, Alignment -> Center], (* End Column for question view *)

         "correct_show_hint", 
           (* --- Show Correct Message & Hint View --- *)
           Column[{
             Style["Correct!", 18, Bold, Green], (* Correct text *)
             Spacer[15],
             (* Display the hint using hintData passed into the function *)
             If[hintData === Missing["NoHintAvailable"], 
                Style["Hint: (No simple hint available this time)", 16],
                Row[{
                  Style["Hint: Position ", 16],
                  Style[hintData[[2]], 16, Bold], 
                  Style[" should be ", 16],
                  Graphics[{hintData[[1]], Disk[]}, ImageSize -> 30] 
                }, Alignment -> Center]
             ],
             Spacer[25],
             (* Close button INSIDE the Column *)
             Button["Close", NotebookClose[EvaluationNotebook[]]] 
            }, Alignment -> Center], (* End Column for correct/hint view *)

         "incorrect_show_message", 
           (* --- Show Incorrect Message View --- *)
           Column[{
             Style["Incorrect!", 18, Bold, Red], (* Incorrect text, maybe Red color *)
             Spacer[25],
              (* Close button INSIDE the Column *)
             Button["Close", NotebookClose[EvaluationNotebook[]]]
            }, Alignment -> Center], (* End Column for incorrect view *)

        _, (* Default/Error case *)
         Style["Error displaying dialog content.", Red]
         
        ], (* End Switch *)
      TrackedSymbols :> {displayState} (* Refresh only when displayState changes *)
      ] (* End Dynamic@Refresh *)
    ], (* End DynamicModule *)
    
    (* --- Dialog Options --- *)
    WindowTitle -> "Trivia Mastermind Hint",
    WindowSize -> {600, Automatic}, (* Or use configured windowSize *)
    Modal -> True,
    WindowElements -> {},
    WindowFrame -> "ModalDialog",
    Background -> White,
    
    (* *** CRUCIAL: This handles closing and returning the result *** *)
    NotebookEventActions -> {"WindowClose" :> (
        dialogOpen = False;   (* Allows the While loop below to terminate *)
        result = internalResult (* Copies the T/F answer to the outer variable *)
    )} 
    
  ]; (* End CreateDialog *)
  
  (* --- Wait for Dialog Closure --- *)
  (* Reset state just before waiting *)
  result = $Failed; 
  dialogOpen = True;
  (* Pause execution here until the dialog is closed (manually or via button) *)
  While[dialogOpen, Pause[0.1]];
  
  (* --- Return Result --- *)
  result (* Return True if correct answer was selected, False otherwise *)
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


(* CalcolaHintSemplice adapted for new hintFeedbackHistory structure *)

CalcolaHintSemplice[hintFeedbackHistoryInput_List, soluzioneListInput_List] := Catch[
  Module[
    {
     (* Parameters after validation *)
     soluzioneList, 
     hintFeedbackHistory,
     n, (* Length of the solution *)
     
     (* Derived from inputs *)
     actualTurnsData, (* List of actual played turns, each turn is {{color,feedback},...} *)
     uniqueSolutionColors,
     confirmedSolutionColors, (* Association: solutionColor -> True/False *)

     (* Loop/temp variables *)
     currentTurnData, currentPegData, guessedColor, feedbackSymbol, colorKey,
     
     (* For Priority 2 *)
     lastTurnData, targetPosition, correctColorAtTarget
    },

    (* --- 1. RIGOROUS INPUT VALIDATION --- *)
    If[!ListQ[soluzioneListInput],
      Print["Error: CalcolaHintSemplice - 'soluzioneListInput' is not a list. Received: ", Head[soluzioneListInput]];
      Throw[Missing["InvalidInputSolutionNotList"]]
    ];
    soluzioneList = soluzioneListInput;
    n = Length[soluzioneList];

    If[!ListQ[hintFeedbackHistoryInput],
      Print["Error: CalcolaHintSemplice - 'hintFeedbackHistoryInput' is not a list. Received: ", Head[hintFeedbackHistoryInput]];
      Throw[Missing["InvalidInputHistoryNotList"]]
    ];
    If[Length[hintFeedbackHistoryInput] > 0 && !AllTrue[hintFeedbackHistoryInput, ListQ],
      Print["Error: CalcolaHintSemplice - 'hintFeedbackHistoryInput' is not a list of lists."];
      Throw[Missing["InvalidInputHistoryNotListOfLists"]]
    ];
    hintFeedbackHistory = hintFeedbackHistoryInput;

    (* --- 2. SPECIAL CASE: No Actual Guesses Made Yet --- *)
    (* This checks if all entries in hintFeedbackHistory are empty lists {} *)
    If[AllTrue[hintFeedbackHistory, # === {} &],
      If[n > 0, 
          Throw[{RandomChoice[soluzioneList], Missing["PositionNotApplicable"]}]
      ,
          Throw[Missing["SolutionIsEmpty"]] 
      ];
    ];

    (* --- 3. Filter for Actual Played Turns --- *)
    actualTurnsData = Select[hintFeedbackHistory, # =!= {} &];

    (* If, after filtering, there are no actual turns (e.g., if hintFeedbackHistory was empty initially,
       though the AllTrue check should handle the {{},{},...} case) *)
    If[Length[actualTurnsData] == 0,
      If[n > 0,
          Throw[{RandomChoice[soluzioneList], Missing["PositionNotApplicable"]}]
      ,
          Throw[Missing["SolutionIsEmpty"]]
      ];
    ];

    (* --- 4. Priority 1: Check for Unconfirmed Solution Colors --- *)
    (* This part uses the pre-calculated feedback in actualTurnsData *)
    
    uniqueSolutionColors = DeleteDuplicates[soluzioneList];
    (* Ensure uniqueSolutionColors is not empty if solution wasn't, before creating Association *)
    If[n > 0 && Length[uniqueSolutionColors] == 0,
        Print["Error: CalcolaHintSemplice - 'uniqueSolutionColors' became empty from a non-empty solution. Solution content: ", soluzioneList];
        Throw[Missing["ProblemWithSolutionContent"]]
    ];
    If[Length[uniqueSolutionColors] == 0, (* Implies n was 0 or solution was problematic *)
        Throw[Missing["SolutionEffectivelyEmpty"]]
    ];
    
    confirmedSolutionColors = Association[# -> False & /@ uniqueSolutionColors]; 

    Do[ (* Iterate through each played turn's data *)
      currentTurnData = turnIter; (* turnIter is {{color,feedback}, {color,feedback}, ...} *)
      Do[ (* Iterate through each peg's data in that turn *)
        currentPegData = pegIter; (* pegIter is {guessedColor, feedbackSymbol} *)
        guessedColor = currentPegData[[1]];
        feedbackSymbol = currentPegData[[2]];
        
        If[feedbackSymbol === feedbackEsatto || feedbackSymbol === feedbackParziale,
          If[KeyExistsQ[confirmedSolutionColors, guessedColor],
             confirmedSolutionColors = AssociateTo[confirmedSolutionColors, guessedColor -> True];
          ]
          (* If guessedColor that got feedback isn't a solution color, we don't track it in confirmedSolutionColors *)
        ];
      , {pegIter, currentTurnData}];
    , {turnIter, actualTurnsData}];

    (* Check if any solution color remains unconfirmed *)
    Do[
      colorKey = solColorIter; (* Use distinct iterator *)
      If[KeyExistsQ[confirmedSolutionColors, colorKey] && confirmedSolutionColors[colorKey] === False,
        Print["partial"]; (* User's debug print *)
        Throw[{colorKey, Missing["PositionNotApplicable"]}]
      ];
    , {solColorIter, uniqueSolutionColors}];

    (* --- 5. Priority 2: Hint from Last Actual Guess's Partial Match --- *)
    (* If we reach here, it means all solution colors have been confirmed by some feedback *)
    
    lastTurnData = Last[actualTurnsData]; (* lastTurnData is {{color,feedback}, ...} *)

    For[i = 1, i <= n, i++,
      feedbackSymbol = lastTurnData[[i, 2]]; (* Get feedback for i-th peg of last guess *)
      
      If[feedbackSymbol === feedbackParziale,
        targetPosition = i;
        correctColorAtTarget = soluzioneList[[targetPosition]];
        Print["full"]; (* User's debug print *)
        Throw[{correctColorAtTarget, targetPosition}]
      ];
    ];

    (* --- 6. Fallback --- *)
    Throw[Missing["NoSimpleHintAvailable"]]
  ] (* End Module *)
] (* End Catch *)



End[];
EndPackage[];
