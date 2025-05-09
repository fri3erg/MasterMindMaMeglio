(* ::Package:: *)

(* :Title:Trivia Mastermind*)
(* :Context:PacchettoProgetto`*)
(* :Author:Gruppo 10 - I Ludopatici*)
(* :Summary:Package per "Trivia Mastermind", progetto di MC Unibo anno 24/25*)
(* :Package Version:0.2*)
(* :History:last modified 8/5/2025*)
(* :Copyright:\[Copyright] 2025 Gruppo 10 - Trivia Mastermind*)
(* :License:MIT License*)

BeginPackage["PacchettoProgetto`"];
(*ClearAll["PacchettoProgetto`*"];*)

(* USAGES DI FUNZIONI CHIAMATE ESPLICITAMENTE NEL NOTEBOOK *)
avviaSchermataDiGioco::usage="Avvia l\[CloseCurlyQuote]interfaccia grafica principale, visualizzando una schermata iniziale da cui \[EGrave] possibile personalizzare i parametri del gioco e avviare una nuova partita.";


Begin["`Private`"];

(* Global variables that need to be shared across functions *)

PacchettoProgetto`Private`triviaData;
(* Accessor function or a rule for triviaData that loads it on first use and not each time.
(Scope difference between functions put either loading inside the game or everywhere also) *)
PacchettoProgetto`Private`triviaData := (
    PacchettoProgetto`Private`triviaData = LoadQuestionsFromCSV["science-technology.csv"]
);

(* Lista dei colori usati da Mastermind *)
paletteColori={RGBColor[0.9,0,0], Green, Yellow, Blue, Orange, Brown, Purple, Cyan, Magenta, White, Gray, Black};
(* Stato della partita *)
partitaInCorso=True

(* Libreria di etichette *)
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
	"esci" -> "QUIT",
	"seedSelezionato" -> "GAME STARTED WITH SEED: ",
	"colori" -> "Colors",
	"combinazione" -> "Cipher",
	"suggerimenti" -> "Feedback",
	"azione" -> "Actions",
	"restartVinto"->"YOU WON! Want to crack the same code?",
	"restartPerso"->"You lost... Want to crack the same code?",
	"vai"->"CHECK",
	"menu"->"\[LongLeftArrow]",
	"trivia"->"NEED A TIP?",
	"idea"->"\[LightBulb]"
	|>;


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
        
       Spacer[63.5],
	    
	   SetterBar[
	    Dynamic[customTurni],
	    Table[
	      i->Style[ToString[i], FontFamily->"Consolas", Bold],
	      {i, 6, 12}
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
Esempio: {feedbackParziale, feedbackEsatto, feedbackParziale, feedbackAssente} *)
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
        {mastermindVittoria, feedback},                        (* Caso vincita *)
    
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
interfacciaGriglia[seed_, lunghezzaCombinazione_, numeroTentativi_, allowDuplicates_] := DynamicModule[
{
	gridItemsColors=Table[Opacity[0.2, Black],{numeroTentativi},{lunghezzaCombinazione}],(* Tabella per memorizzare i colori degli elementi, inizialmente tutta nera(opacit\[AGrave] a 0.2)*)
	hintFeedbackHistory = ConstantArray[{}, numeroTentativi],
	turn = 1,(*Numero del tentativo*)
	colorsList=paletteColori,(*Lista di colori della palette di scelta*)
	selectedItem={1,1}, (* Elemento selezionato riga,colonna*)
	soluzioneList=generaCodiceSegreto[seed, lunghezzaCombinazione, allowDuplicates], (* Combinazione segreta *)
	tentativoList=ConstantArray[None, lunghezzaCombinazione], (* Tentativo corrente *)
	valutazioneTentativo={},
	questionCounter=0,
	correct={}
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
				partitaInCorso=True;
				questionCounter=0;
				correct= {};
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
				partitaInCorso=True;
				questionCounter=0;
				correct= {};
			  ] 
             ]
		   }]
		  ),
		 True, Style["", FontSize->0]
		],
	   TrackedSymbols:>{valutazioneTentativo}
	   ]
	 }],
				
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
											If[partitaInCorso,  (* Per fermare ulteriori interazioni una volta conclusa la partita *)
									        (
									          gridItemsColors[[Sequence @@ selectedItem]] = col;
									          tentativoList[[selectedItem[[2]]]] = col;
									          selectedItem = vaiAlProssimoPallinoVuoto[selectedItem, tentativoList, lunghezzaCombinazione];
									        )
									      ]
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
												    If[partitaInCorso && x === turn,  (* Per fermare ulteriori interazioni una volta conclusa la partita, e per agire solo sulla riga corrente *)
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
										          If[Length[correct]<x,
										          AppendTo[correct, {}];(* used to fill the hint column even when hint not used*)
										          ]
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
										(*
										  emptyResultPlaceholder:
										  Represents a state where a result for correct[[x]] is not yet available or not yet set.
										*)
										emptyResultPlaceholder = Missing["NoResultSetYet"];
										
										Dynamic[ (* This entire block will update dynamically when its dependent variables change *)
										  Module[
										    {
										      currentValForRowX, (* Holds the processed value of correct[[x]] or the placeholder *)
										      displayOutput      (* Will hold the final UI element to be rendered by Dynamic *)
										    },
										
										    (* --- Step 1: Determine the Current Value for Row x 
										      This block retrieves the value from correct[[x]] if it's valid and not a placeholder.
										    *)
										    currentValForRowX = If[
										        ListQ[correct] && Length[correct] >= x && x >= 1 && 
										        correct[[x]] =!= Null && correct[[x]] =!= {} && correct[[x]] =!= emptyResultPlaceholder, (* Ensure it's not various forms of empty or our specific placeholder *)
										        correct[[x]], (* Use the actual value from correct[[x]] *)
										        emptyResultPlaceholder (* Otherwise, use the placeholder *)
										    ];
										
										    (* --- Step 2: Determine the UI to Display Based on currentValForRowX 
										    *)
										    displayOutput = Which[
										
										      (* === Case 1: The result for row x indicates a "WrongAnswer" === *)
										      currentValForRowX === Missing["WrongAnswer"],
										      Framed[
										        "", (* Display an empty, colored box as a visual marker *)
										        Background -> Red,
										        FrameStyle -> None,
										        RoundingRadius -> 10,
										        FrameMargins -> {{10, 10}, {10, 10}}, 
										        ImageSize -> {80, 35}                 
										      ],
										
										      (* === Case 2: The result is a "Correct Answer" (a Hint, typically {color, value}) === *)
										      MatchQ[currentValForRowX, {_?ColorQ, _}], (* Check if currentValForRowX matches the pattern {a Color, someValue} *)
										      Module[ (* Localize variables for displaying the correct hint *)
										        {
										          resultColor = First[currentValForRowX], (* Extract the color from the hint *)
										          resultValue = Last[currentValForRowX]  (* Extract the value/position from the hint *)
										        },
										        Framed[
										          (*
										            The content of the green "Correct Hint" box depends on the 'resultValue'.
										          *)
										          If[resultValue =!= Missing["PositionNotApplicable"] && resultValue =!= Missing["NoSimpleHintAvailable"],
										            (* If the hint value is a specific position/number: display color + value *)
										            Row[{
										              Graphics[{EdgeForm[Gray], resultColor, Disk[]}, ImageSize -> {20, 20}],
										              Spacer[5],
										              Style[ToString[resultValue], Medium, Bold, FontFamily -> "Arial"]
										            }],
										            (* Else (hint value indicates position not applicable or no simple hint): display only the color *)
										            Graphics[{EdgeForm[Gray], resultColor, Disk[]}, ImageSize -> {20, 20}]
										          ],
										          Background -> Green,
										          FrameStyle -> None,
										          RoundingRadius -> 10,
										          FrameMargins -> {{30, 10}, {6, 6}}, 
										          ImageSize -> {80, 35}
										        ]
										      ],
										
										      (* === Case 3: No specific result yet (i.e., currentValForRowX is emptyResultPlaceholder) === *)
										      (* This is the default branch of the 'Which' statement. *)
										      (* It will decide whether to show an active "HINT" button or an inactive placeholder. *)
										      True,
										      If[x === turn  && triviaData =!= $Failed,
										        (* --- Display Active "HINT" Button --- *)
										        (* Conditions: current row (x) is the active turn, game is ongoing, and trivia data is loaded. *)
										        ClickPane[
										          Framed[ (* Visual appearance of the active HINT button *)
										            Grid[{
										              {
										                Style["\|01f4a1", FontSize -> 10],
										                Style["HINT", White, FontFamily -> "Consolas", FontSize -> 12, Bold]
										              }
										            },
										            Alignment -> {Center, Center}, Spacings -> {1, 0}
										            ],
										            Background -> Blue,
										            FrameStyle -> None,
										            RoundingRadius -> 10,
										            FrameMargins -> {{10, 10}, {10, 10}},
										            ImageSize -> {80, 35}
										          ],
										          (* --- ClickPane Action: Called when the HINT button is clicked --- *)
										          Function[ (* Anonymous function for the button's action *)
										            (*
										              'AppendTo' is the correct operation for their specific logic as i append {} even when not using hint in the CHECK function above
										            *)
										            If[partitaInCorso,
										            AppendTo[correct, DisplayTriviaQuestion[seed + questionCounter, CalcolaHintSemplice[hintFeedbackHistory, soluzioneList]]];
										            questionCounter++; (* Increment a counter, for unique trivia question seeds *)
										          ]],
										          Method -> "Queued" (* Ensures UI responsiveness by queuing the action. *)
										        ],
										        
										        (* --- Display Inactive/Placeholder "HINT" Button --- *)
										        (* Shown if the conditions for an active hint button are not met. *)
										        Framed[
										          Grid[{
										            {
										              Style["\|01f4a1", FontSize -> 10, FontColor -> Directive[GrayLevel[0.7], Opacity[0]]], (* invisible icon *)
										              Style["HINT", FontFamily -> "Consolas", FontSize -> 12, FontColor -> Directive[GrayLevel[0.7], Opacity[0]], Bold] (* invisible text *)
										            }
										          },
										          Alignment -> {Center, Center}, Spacings -> {1, 0}
										          ],
										          Background -> GrayLevel[0.9], 
										          FrameStyle -> None,
										          RoundingRadius -> 10,
										          FrameMargins -> {{10, 10}, {10, 10}},
										          ImageSize -> {80, 35} 
										        ]
										      ] (* End If for active/inactive hint button logic *)
										    ]; (* End Which statement determining displayOutput *)
										
										    displayOutput (* The Dynamic expression evaluates to this UI element *)
										  ] (* End Module for localizing currentValForRowX and displayOutput *)
										] (* End Dynamic *)
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
				Alignment -> {{Left, Center, Center, Right}}
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
DynamicModule[{},
    
    Pane[
      Column[{
      
        Panel[
      Row[{
	      ClickPane[
	      Framed[
	        Style[labels["menu"], White, FontSize->12, FontFamily->"Consolas", Bold],
	        Background->Red,
	        FrameStyle->None,
	        RoundingRadius->5,
	        FrameMargins->{{6, 6}, {3, 3}},
	        ImageSize->Automatic
	      ],  
	      Function[
	        cambiaSchermata["menu"]
	      ]
	     ],
	     
	     Spacer[5],
     
        Style[labels["seedSelezionato"] <> ToString[seed], FontSize->12, FontFamily->"Consolas", FontColor->Red, Bold]
      },
      Alignment->Center
      ],
     Background->White
     ],
            
     Dynamic[
      Pane[
        interfacciaGriglia[seed, combinazione, tentativi, allowDuplicates],
        {Automatic, Scaled[0.7]}, (* massimo 80% in altezza *)
        Scrollbars->False,
        Alignment->Center 
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
  Displays a trivia question dialog with multiple choice options.
  The dialog is modal and the function will pause execution until the dialog is closed.

  @param seed: Integer used to select and shuffle the question via PrepareQuestionData.
  @param hintToGive: The hint structure, typically {color, position_or_missing_code}, 
                     which is to be displayed if the answer is correct. This same structure
                     will be the return value of the function if the answer is correct.
  @return: The 'hintToGive' structure if the correct answer was selected.
           Returns Missing["WrongAnswer"] if an incorrect answer was selected or if the
           dialog was closed prematurely (e.g., via the OS window close button).
           Returns $Failed if the dialog was closed before any interaction and result 
           was not explicitly set (should be rare given NotebookEventActions).
*)

DisplayTriviaQuestion[seed_Integer, hintToGive_] := Module[
  {
    (* --- Outer Module Variables --- *)
    questionWindow,                 (* Stores the DialogObject *)
    result = $Failed,               (* Final result to return. Initialized to $Failed. *)
    dialogOpen = True,              (* Flag to control the While loop, making the function synchronous. *)
    
    (* Data prepared once before creating the dialog (output from PrepareQuestionData) *)
    initialQuestionData,            
    initialOptionsData,             
    initialCorrectIndexData,        
    
    stopDialogLoopFunc              (* Function to handle premature dialog closure *)
  },

  (*
    stopDialogLoopFunc:
    This function is called when the dialog window is closed using the OS's native
    close button (e.g., the 'x' button on the window frame).
    It ensures that the main While loop in DisplayTriviaQuestion terminates and
    sets an appropriate 'result'.
  *)
  stopDialogLoopFunc = Function[{}, 
    (* Call performCloseAction to set the result and close the dialog cleanly. *)
    (* The result Missing["WrongAnswer"] indicates incorrect choice but i count it as incorrect if you close the window, because you already saw the question *)
    performCloseAction[Missing["WrongAnswer"]];
  , HoldAll]; 

  (* Prepare question data once, outside the DynamicModule, using the provided seed. *)
  {initialQuestionData, initialOptionsData, initialCorrectIndexData} = PrepareQuestionData[seed];

  (* --- Create the Dialog Window --- *)
  questionWindow = CreateDialog[
    (*
      The main content of the dialog is a DynamicModule.
      This allows for interactive content that can change state without closing the dialog.
    *)
    DynamicModule[
      { 
        displayState = "question", (* Controls the current view: "question", "correct_show_hint", "incorrect_show_message" *)
        
        (* Local copies of question data, initialized from the outer Module's scope. *)
        (* This ensures the DynamicModule has its own stable copy of the data. *)
        localQuestion = initialQuestionData,
        localOptions = initialOptionsData,
        localCorrectIndex = initialCorrectIndexData
      },
      
      (*
        performCloseAction[currentDynamicResult_]:
        This function is called by the "Close" buttons within the dialog.
        It sets the 'result' and 'dialogOpen' variables in the outer DisplayTriviaQuestion Module's scope.
        Lexical scoping allows this inner function to modify variables of its parent Module.
      *)
      performCloseAction[currentDynamicResult_] := (
        result = currentDynamicResult; (* Set the final result of DisplayTriviaQuestion. *)
        dialogOpen = False;           (* Signal the outer While loop to terminate. *)
        NotebookClose[EvaluationNotebook[]]; (* Programmatically close this dialog notebook. *)
      );
      
      (*
        Dynamic@Refresh[ ... , TrackedSymbols :> {displayState}]
        The main UI of the dialog. It's wrapped in Dynamic and Refresh so that it updates
        automatically when 'displayState' changes, showing the appropriate view.
      *)
      Dynamic@Refresh[
        Switch[displayState,

          "question",
          (* --- View 1: Displaying the Question and Answer Options --- *)
          Column[{
            Pane[
              Style[localQuestion["Question"], 16, Bold, TextAlignment -> Center], 
              ImageSize -> {500,120}, 
              Scrollbars -> False, Alignment -> Center
            ],
            Spacer[20],
            Grid[
              Partition[ (* Arrange answer buttons into a grid, typically 2 columns *)
                MapIndexed[
                  Function[{optionText, optionIndex}, (* For each answer option... *)
                    (*
                      Inner DynamicModule for each button:
                      This gives each answer button its own state for visual feedback (clicked, color).
                    *)
                    DynamicModule[{clicked = False, isCorrect = Null, position = First[optionIndex]}, 
                      Button[
                        optionText,
                        (* --- Button Action (when an answer option is clicked) --- *)
                        (
                          isCorrect = (position == localCorrectIndex); (* Check if the selected option is correct. *)
                          clicked = True; (* Mark this button as clicked for visual feedback. *)
                          
                          (* Transition to the appropriate feedback view. *)
                          If[isCorrect,
                            displayState = "correct_show_hint",
                            displayState = "incorrect_show_message"
                          ];
                        ),
                        (* --- End Button Action --- *)
                        Background -> Dynamic[If[clicked, If[isCorrect, Green, Red], White]], (* Dynamic background color *)
                        ImageSize -> {200, 80},
                        BaseStyle -> {FontColor -> Black, FontWeight -> Bold, FontFamily -> "Arial", FontSize -> 14},
                        FrameMargins -> 12
                      ] (* End Button *)
                    ] (* End Inner DynamicModule for button state *)
                  ], (* End Function for MapIndexed *)
                  localOptions (* List of answer strings *)
                ], (* End MapIndexed over options *)
                UpTo[Ceiling[Length[localOptions]/2]] (* Controls items per row in Partition for Grid *)
              ], (* End Partition *)
              Spacings -> {1, 1}, Alignment -> Center
            ] (* End Grid for answer buttons *)
          }, Alignment -> Center], (* End Column for "question" view *)

          "correct_show_hint",
          (* --- View 2: Displayed when the User Answers Correctly --- *)
          Column[{
            Style["Correct!", Bold, Green, FontFamily -> "Arial", FontSize -> 36, TextAlignment -> Center],
            Pane[
              (* This inner Module is just for localizing 'theCol' and 'posVal' if needed,*)
              Module[{theCol, posVal},
                {theCol, posVal} = hintToGive; (* Destructure the hint passed to DisplayTriviaQuestion. *)
                
                (* Display the hint based on its structure. *)
                Which[
                  posVal === Missing["PositionNotApplicable"],
                  Row[{
                    Style["This color is present in the combination:", Medium, FontFamily -> "Arial", FontSize -> 18],
                    Spacer[8],
                    Tooltip[Graphics[{EdgeForm[Gray], theCol, Disk[]}, ImageSize -> {25, 25}], ToString[theCol]]
                  }, Alignment -> Center],

                  posVal === Missing["NoSimpleHintAvailable"],
                  Row[{
                    Style["No hint available, you have all the information", Medium, FontFamily -> "Arial", FontSize -> 18]
                  }, Alignment -> Center],
                  
                  True, (* Default case: assumes posVal is an integer representing a position. *)
                  Row[{
                    Style["The color: ", Medium, FontFamily -> "Arial", FontSize -> 18], Spacer[8],
                    Tooltip[Graphics[{EdgeForm[Gray], theCol, Disk[]}, ImageSize -> {25, 25}], ToString[theCol]], Spacer[8],
                    Style["is at position", Medium, FontFamily -> "Arial", FontSize -> 18], Spacer[8],
                    Style[ToString[posVal], Medium, Bold, FontFamily -> "Arial", FontSize -> 24]
                  }, Alignment -> Center]
                ] (* End Which for hint display logic *)
              ], (* End Module for hint destructuring *)
              {500, Automatic}, (* Use configured width, automatic height *)
              Alignment -> Center
            ],
            Button[ (* "Close" button for the "Correct!" view *)
              Style["Close", Bold, FontSize -> 24],
              performCloseAction[hintToGive] (* Return the original hintToGive structure as the result. *)
            ]
          }, Alignment -> Center, Spacings -> 15], (* End Column for "correct_show_hint" view *)

          "incorrect_show_message",
          (* --- View 3: Displayed when the User Answers Incorrectly --- *)
          Column[{
            Pane[
              Style["Incorrect!", 36, Bold, Red, FontFamily -> "Arial", TextAlignment -> Center],
              {500, Automatic}, Alignment -> Center
            ],
            Pane[
               Style["The correct answer was: ", Medium, FontFamily -> "Arial", FontSize -> 18],
              {500, Automatic}, Alignment -> Center
            ],
            Pane[
                    Style[localOptions[[localCorrectIndex]], Medium, Bold, FontFamily -> "Arial", FontSize -> 18],
              {500, Automatic}, Alignment -> Center
            ],
            Button[ (* "Close" button for the "Incorrect!" view *)
              Style["Close", Bold, FontSize -> 24],
              performCloseAction[Missing["WrongAnswer"]] (* Return Missing["WrongAnswer"] as the result. *)
            ]
          }, Alignment -> Center, Spacings -> 8], (* End Column for "incorrect_show_message" view *)

          _, (* Default case for Switch[displayState, ...]: handles any unexpected state. *)
          Style["Error: Dialog display state is invalid. Please report this.", Red, Bold]
          
        ], (* End Switch on displayState *)
        TrackedSymbols :> {displayState} (* Ensures the Dynamic content updates only when 'displayState' changes. *)
      ] (* End Dynamic@Refresh *)
    ], (* End DynamicModule for dialog content *)
    
    (* --- Standard Dialog Options --- *)
    WindowTitle -> "Trivia Mastermind Hint",
    WindowSize -> {520, 400}, 
    Modal -> True,             (* Dialog blocks interaction with other notebooks. *)
    WindowElements -> {},       
    WindowFrame -> "ModalDialog", (* Standard frame for modal dialogs, usually includes an OS close button. *)
    Background -> White,
    NotebookEventActions -> {
      "WindowClose" :> stopDialogLoopFunc[] (* Custom action when OS close button is clicked. *)
    }
  ]; (* End CreateDialog *)
  
  (* --- Wait for Dialog Closure --- *)
  (* The 'result' and 'dialogOpen' variables are initialized at the start of the Module. *)
  (* This loop pauses the execution of DisplayTriviaQuestion until 'dialogOpen' becomes False 
     (which happens inside performCloseAction or stopDialogLoopFunc).
     so that the result gets passed only after the dialog is closed and user has a chance to respond *)
  While[dialogOpen, Pause[0.1]];
  
  (* --- Return Final Result --- *)
  result (* The value set by performCloseAction. *)
];


(*
  Prepares question data by selecting and shuffling options
  @param seed: Integer used for random selection
  @return: {question, options, correctIndex}
*)
PrepareQuestionData[seed_Integer] := Module[
  {questionIndex, options, rawCorrectIndex, selectedQuestion, optionKeys, correctIndex},
  
  SeedRandom[seed];
  (* Select question based on seed *)
  questionIndex = Mod[seed, Length[triviaData], 1];
  selectedQuestion = Normal[triviaData[[questionIndex]]];

  (* Get correct answer index *)
  rawCorrectIndex = Lookup[selectedQuestion, "Correct Index", Missing["NotAvailable"]];
  rawCorrectIndex = If[NumberQ[rawCorrectIndex], Round[rawCorrectIndex], 1];

  (* Extract all available options *)
  optionKeys = {"Option A", "Option B", "Option C", "Option D"};
  options = DeleteCases[Lookup[selectedQuestion, optionKeys, ""], _Missing | "" | Null];

  {selectedQuestion, options, rawCorrectIndex}
];


(*
  CalcolaHintSemplice

  Provides a prioritized hint for a Mastermind-like game based on the history of guesses
  and the known solution. The function uses a Catch/Throw mechanism for all its return paths.

  The function assumes 'feedbackEsatto' (correct color in correct position) and
  'feedbackParziale' (correct color in wrong position) are globally defined symbols
  or accessible in the calling scope.

  Hint Priorities:
  1. No Guesses Yet: If no actual guesses have been made, suggest a random color from the solution.
  2. Unconfirmed Solution Color: Suggest a color from the solution that has not yet appeared in
     any guess that received 'feedbackEsatto' or 'feedbackParziale'.
  3. Partially Matched, Not Exactly Matched Color: If all solution colors have received some
     feedback, suggest a color that has received 'feedbackParziale' but never 'feedbackEsatto',
     along with its correct position in the solution.
  4. Fallback: If no other specific hint can be generated.

  @param hintFeedbackHistoryInput_List:
    A list representing the history of all played turns. Each element is a turn.
    - A turn is a list of peg data: {{color1, feedback1}, {color2, feedback2}, ...}.
    - Each peg data is a list: {guessedColor, feedbackSymbol}.
      'guessedColor' can be any color type (e.g., RGBColor, a named color string).
      'feedbackSymbol' is expected to be one of 'feedbackEsatto', 'feedbackParziale', or
      another symbol indicating an incorrect guess (though only esatto/parziale are actively used
      for positive hint generation).
    - Empty lists '{}' within hintFeedbackHistoryInput represent turns where no guess was made
      or that are not yet played.
    Example: {{{RGBColor[1,0,0], feedbackEsatto}, {RGBColor[0,1,0], feedbackParziale}}, {}}

  @param soluzioneListInput_List:
    A list representing the secret code or solution. Each element is a color.
    Example: {RGBColor[1,0,0], RGBColor[0,1,0], RGBColor[0,0,1]}

  @return (via Throw):
    The function always exits by Throwing one of the following structures:

    - Missing["SolutionIsEmpty"]:
        If 'soluzioneListInput' is empty or contains no valid colors.

    - {color_SymbolOrObject, Missing["PositionNotApplicable"]}:
        - If no actual guesses have been made yet (a random color from the solution is provided).
        - For a Priority 1 hint (an unconfirmed solution color is provided).

    - {color_SymbolOrObject, position_Integer}:
        For a Priority 2 hint. 'color' is the suggested color, and 'position' is its
        1-based index in the 'soluzioneListInput'.

    - {"Red", Missing["NoSimpleHintAvailable"]}:
        The fallback hint if no other hint priority is met. "Red" is an arbitrary
        placeholder color. The Missing object indicates the reason.
*)
CalcolaHintSemplice[hintFeedbackHistoryInput_List, soluzioneListInput_List] := Catch[
  Module[
    {
      (* Parameters after validation/assignment *)
      soluzioneList = soluzioneListInput,
      hintFeedbackHistory = hintFeedbackHistoryInput,
      n = Length[soluzioneListInput], (* Length of the solution *)

      (* Derived from inputs *)
      actualTurnsData, (* List of actual played turns, each turn is {{color,feedback},...} *)
      uniqueSolutionColors,
      confirmedSolutionColors, (* Association: solutionColor -> True/False indicating if a color has received 'esatto' or 'parziale' feedback *)

      (* Loop/temp variables *)
      currentTurnData, currentPegData, guessedColor, feedbackSymbol, colorKey,
      
      (* For Priority 2 *)
      lastTurnData, 
      colorList = {}, (* List of colors that received 'feedbackParziale' and not 'feedbackEsatto' *)
      
      (* Iterators for loops *)
      turnIter, pegIter, solColorIter, i, j
    },
    
    If[n == 0,
      Throw[Missing["SolutionIsEmpty"]]
    ];

    (* --- 2. Filter for Actual Played Turns & Handle "No Guesses Yet" Case --- *)
    
    (* Select only turns that have actual guess data (i.e., are not empty lists) *)
    actualTurnsData = Select[hintFeedbackHistory, # =!= {} &];

    (* If no actual guesses have been made yet *)
    If[Length[actualTurnsData] == 0,
      (* Provide a random color from the solution as a starting hint *)
      (* The position is not applicable for this type of hint *)
      Throw[{RandomChoice[soluzioneList], Missing["PositionNotApplicable"]}]
    ];

    (* --- 3. Priority 1: Check for Unconfirmed Solution Colors --- *)
    (* Goal: Suggest a color from the solution that has not yet been part of any guess receiving 'feedbackEsatto' or 'feedbackParziale'. *)

    uniqueSolutionColors = DeleteDuplicates[soluzioneList];
    
    (* Initialize an association to track if each unique solution color has been "confirmed" (received positive feedback) *)
    (* A color is considered "confirmed" if it was guessed and received either 'feedbackEsatto' or 'feedbackParziale' *)
    confirmedSolutionColors = Association[# -> False & /@ uniqueSolutionColors];

    (* Iterate through each played turn and each peg in that turn *)
    Do[
      currentTurnData = turnIter; (* currentTurnData is a list of {guessedColor, feedbackSymbol} for one turn *)
      Do[
        currentPegData = pegIter; (* currentPegData is a single {guessedColor, feedbackSymbol} pair *)
        guessedColor = currentPegData[[1]];
        feedbackSymbol = currentPegData[[2]];
        
        (* If the guessed color received 'feedbackEsatto' or 'feedbackParziale' *)
        If[feedbackSymbol === feedbackEsatto || feedbackSymbol === feedbackParziale,
          (* And if this color is one of the colors in the actual solution *)
          If[KeyExistsQ[confirmedSolutionColors, guessedColor],
            (* Mark this solution color as "confirmed" *)
            confirmedSolutionColors = AssociateTo[confirmedSolutionColors, guessedColor -> True];
          ]
          (* If a guessedColor that received feedback isn't a solution color, it's not tracked in confirmedSolutionColors *)
        ];
      , {pegIter, currentTurnData}]; (* End loop over pegs in the current turn *)
    , {turnIter, actualTurnsData}]; (* End loop over actual played turns *)

    (* Check if any solution color remains unconfirmed *)
    Do[
      colorKey = solColorIter; (* colorKey is one of the unique solution colors *)
      (* If the color exists in our tracking and is marked as False (unconfirmed) *)
      If[Lookup[confirmedSolutionColors, colorKey, True] === False,
        (* Suggest this unconfirmed solution color. Position is not applicable for this hint type. *)
        Throw[{colorKey, Missing["PositionNotApplicable"]}]
      ];
    , {solColorIter, uniqueSolutionColors}]; (* End loop over unique solution colors *)

    (* --- 4. Priority 2: Hint from a Color with Partial Match but No Exact Match --- *)
    (* Goal: If all solution colors have been "confirmed" (i.e., received some form of positive feedback),
       find a color that has received 'feedbackParziale' but has NOT received 'feedbackEsatto' in any guess.
       Then, suggest this color and its actual position in the solution. *)

    (* Step 4a: Collect all colors that received 'feedbackParziale' in any position across all guesses *)
    For[i = 1, i <= Length[actualTurnsData], i++,
      lastTurnData = actualTurnsData[[i]]; (* A single turn's data: {{color,feedback}, ...} *)
      For[j = 1, j <= n, j++, (* Iterate through each peg position of the guess *)
        Module[{colorInGuess, feedbackForPeg},
          colorInGuess = lastTurnData[[j, 1]];
          feedbackForPeg = lastTurnData[[j, 2]];
          
          If[feedbackForPeg === feedbackParziale,
            AppendTo[colorList, colorInGuess];
          ];
        ];
      ]; (* End loop over pegs for the current turn (lastTurnData) *)
    ]; (* End loop over actualTurnsData *)

    (* Step 4b: Remove any colors from 'colorList' if they ever received 'feedbackEsatto' in any position *)
    For[i = 1, i <= Length[actualTurnsData], i++,
      lastTurnData = actualTurnsData[[i]];
      For[j = 1, j <= n, j++,
        Module[{colorInGuess, feedbackForPeg},
          colorInGuess = lastTurnData[[j, 1]];
          feedbackForPeg = lastTurnData[[j, 2]];
        
          If[feedbackForPeg === feedbackEsatto,
            (* If a color received 'feedbackEsatto', it's no longer a candidate for this type of hint *)
            colorList = DeleteCases[colorList, colorInGuess];
          ];
        ];
      ]; (* End loop over pegs for the current turn (lastTurnData) *)
    ]; (* End loop over actualTurnsData *)

    (* 'colorList' now contains colors that have received 'feedbackParziale' at least once, 
       and have never received 'feedbackEsatto'. It might contain duplicates. *)

    (* Step 4c: If candidate colors exist, take the first one and find an instance where it got 'feedbackParziale' *)
    If[colorList =!= {},
      Module[{targetHintColor = First[colorList], firstOccurrencePositionInSolution},
        (* This loop re-iterates to find a specific instance where First[colorList] got 'feedbackParziale'.
           This is part of the original logic to confirm the condition before throwing. *)
        For[i = 1, i <= Length[actualTurnsData], i++,
          lastTurnData = actualTurnsData[[i]];
          For[j = 1, j <= n, j++,
            Module[{colorInGuess, feedbackForPeg},
              colorInGuess = lastTurnData[[j, 1]];
              feedbackForPeg = lastTurnData[[j, 2]];
            
              If[feedbackForPeg === feedbackParziale && colorInGuess == targetHintColor,
                (* Found an instance. Now get the true position of this color in the solution. *)
                firstOccurrencePositionInSolution = Position[soluzioneList, targetHintColor][[1, 1]];
                Throw[{targetHintColor, firstOccurrencePositionInSolution}]
              ];
            ];
          ]; (* End loop over pegs for the current turn (lastTurnData) *)
        ]; (* End loop over actualTurnsData *)
      ] (* End Module for targetHintColor *)
    ]; (* End If *)
    
    (* --- 5. Fallback Hint --- *)
    (* If no other specific hint could be generated based on the priorities above. *)
    (* "Red" is an arbitrary placeholder color *)
    Throw[{"Red", Missing["NoSimpleHintAvailable"]}]
    
  ] (* End Module *)
] (* End Catch *)


(* === Codice usato per il bottone di avvio nel notebook ===
Button["Avvia Programma", FrontEndExecute[FrontEndToken[InputNotebook[], "EvaluateNotebook"]],
 BaseStyle -> {"GenericButton", 16, Bold}, ImageSize -> {175, 50}] *)


End[];
EndPackage[];
