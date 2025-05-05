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
interfacciaGriglia[seed_, lunghezzaCombinazione_, numeroTentativi_, allowDuplicates_] := DynamicModule[
{
	gridItemsColors=Table[Opacity[0.2, Black],{numeroTentativi},{lunghezzaCombinazione}],(* Tabella per memorizzare i colori degli elementi, inizialmente tutta nera(opacit\[AGrave] a 0.2)*)
	hintFeedbackHistory = ConstantArray[{}, numeroTentativi],
	turn = 1,(*Numero del tentativo*)
	colorsList=paletteColori,(*Lista di colori della palette di scelta*)
	selectedItem={1,1}, (* Elemento selezionato riga,colonna*)
	soluzioneList=generaCodiceSegreto[seed, lunghezzaCombinazione, allowDuplicates], (* Combinazione segreta *)
	tentativoList=ConstantArray[None, lunghezzaCombinazione], (* Tentativo corrente *)
	valutazioneTentativo={}
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
							              (* Sostituisce il feedBack con il colore associato*)
							              feedbackColors=If[hintFeedbackHistory[[x]] =!= {},
							                  (*Length[valutazioneTentativo] > 1(* && row === turn*),*)
							                  hintFeedbackHistory[[x]] /. {
							                      feedbackEsatto->RGBColor[0.57,1,0.05],  (* Un verde chiaro *)
							                      feedbackParziale->RGBColor[1,0.85,0],   (* Un giallo dorato *)
							                      feedbackAssente->None                   (* Vuoto *)
							                  },
							              ConstantArray[None, lunghezzaCombinazione]
							              ]
							          },
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
														   hintFeedbackHistory[[turn]] = valutazioneTentativo[[2]]; (*Set dei feedback*)
														   If[valutazioneTentativo[[1]] === mastermindProsegui, turn++];
														   selectedItem = {turn, 1}; (*Item successivo*)
														   (* RESET TENTATIVO*)
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
DynamicModule[{paletteRandom},
    
    (*SeedRandom[seed];*)
    (*paletteRandom=Table[RandomColor[], {12}];*)
    
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
        interfacciaGriglia[seed, combinazione, tentativi, allowDuplicates],
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


End[];
EndPackage[];
