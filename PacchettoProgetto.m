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

	(* Schermata custom *)
	(*
	Int? seedInserito;
	Int? numeroColori;
	Int? numeroColonne;
	Int? numeroTurni;
	
	mostraConfigurazioneCustom[] := Column[{
	
		Spacer[{0, 20}], 
	
		Style["Settings", FontSize->18, FontFamily->"Consolas"],
		Column[{
			Item[
				Framed[
					InputField[
						Dynamic[numeroColori],
						Number,
						FieldHint->"COLORI",
						FieldHintStyle->{Bold, FontSlant -> "Plain"},
						ImageSize->{100, 21},
						Appearance->"Frameless",
						BaselinePosition->Center
					],
				Background->LightGray,
                FrameStyle->None, 
                RoundingRadius->10,
                FrameMargins->{{10, 10}, {5, 5}},
                ImageSize->Automatic
            ],
            ItemSize -> Automatic
		],
        
        Spacer[{0,5}],
        
        Item[
            Framed[
                InputField[
                    Dynamic[numeroColonne],
                    Number,
                    FieldHint->"COLONNE",
                    FieldHintStyle->{Bold, FontSlant->"Plain"},
                    ImageSize->{100, 21},
                    Appearance->"Frameless",
                    BaselinePosition->Center
                ],
                Background->LightGray,
                FrameStyle->None, 
                RoundingRadius->10, 
                FrameMargins->{{10, 10},{5, 5}},
                ImageSize->Automatic
            ],
            ItemSize->Automatic
        ], 
         
        Spacer[{0,5}],
        
        Item[
            Framed[
                InputField[
                    Dynamic[numeroTurni],
                    Number,
                    FieldHint->"TURNI",
                    FieldHintStyle->{Bold, FontSlant->"Plain"},
                    ImageSize->{100, 21},
                    Appearance->"Frameless",
                    BaselinePosition->Center
                ],
                Background->LightGray,
                FrameStyle->None, 
                RoundingRadius->10, 
                FrameMargins->{{10, 10},{5, 5}},
                ImageSize->Automatic
            ],  
            ItemSize->Automatic
        ]   
    }],
     
	ClickPane[
	    Framed[
	        Pane[
	            Style["\[FilledRightTriangle]", FontSize->25, FontColor->White],
	            Alignment->Center,
	            ImageSize->{120, 32}
	        ],
	        Background->RGBColor[0, 0.5, 0], 
	        FrameStyle->None, 
	        RoundingRadius->10,
	        FrameMargins->0
	    ],
	    Function[
	        customColori=numeroColori;
	        numeroColori=Null;
	        customColonne=numeroColonne;
	        numeroColonne=Null;
	        customTurni=numeroTurni;
	        numeroTurni=Null;
	        cambiaSchermata["gioco"]
	    ]
	],

    Spacer[{0, 20}]

    }, Spacings->2, Alignment->Center]; *)

    (* Homepage *)
    creaHomepage[] := Column[{
        
        Spacer[{0, 50}],
    
        Dynamic @ Style[labels["titoloGioco"],
            FontSize->titleFontScale,
            FontWeight->Bold,
            FontColor->Black,
            FontFamily->"Consolas",
            TextAlignment->Center
        ],
        
        Spacer[{0, 20}],
        
        Dynamic @ Style[labels["fattoDa"],
            FontSize->titleFontScale/5,
            FontFamily->"Consolas",
            FontColor->Gray,
            TextAlignment->Center
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
                ItemSize -> Automatic 
            ],
       
            Spacer[15],
       
            Dynamic[
                If[IntegerQ[seedInserito] && seedInserito>0,
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
                        SeedRandom[customSeed];
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
        }, Alignment->Center],
    
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
                
                Spacer[60],
                
                SetterBar[
                    Dynamic[customColonne],
                    Table[
                        j->Style[ToString[j], FontFamily->"Consolas", Bold],
                        {j, 3, 7} 
                    ],
                    Appearance->"Horizontal"
                ]
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
    }, Alignment -> Center];

    (* Contenuto dinamico *)
    content=Pane[
        Dynamic @ Refresh[
            Switch[currentScreen,
                "menu", creaHomepage[],
                "gioco", creaSchermataGioco[customSeed, customTurni, customColonne, titleFontScale]
            ],
            TrackedSymbols:>{currentScreen, customTurni, customColonne, titleFontScale}
        ],
        Full,
        Alignment->{Center, Top}
    ];
    
    (* Finestra principale *)
    mainWindow = CreateDocument[
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
paletteColori={Red, Green, Yellow, Blue, Orange, Brown, Purple, Cyan, Magenta};
(* Stato della partita *)
partitaInCorso=True

labels=translations = <|
	"titoloGioco" -> "MASTERMIND MA MEGLIO",
	"fattoDa" -> "Made with \:2665 by Alessandro Modelli, Angelo Greco, Elia Friberg, Francesca Mazzetti, Gianpiero Tovo, Matteo Raggi",
	"inserisciSeed" -> "Inserisci un seed: ",
	"placeholderSeed" -> "Scrivi seed prima di iniziare...",
	"play" -> "\[FilledRightTriangle]",
	"randomSeed" -> "\:21bb",
	"nTurni" -> "N. turni",
	"nCombinazione" -> "N. combinazione",
	"esci" -> "ESCI",
	"partita" -> "PARTITA",
	"seedSelezionato" -> "Avvio con seed: ",
	"colori" -> "Colori",
	"combinazione" -> "Combinazione",
	"suggerimenti" -> "Suggerimenti",
	"azione" -> "Azione",
	"restart" -> "Rigioca",
	"vai" -> "VAI!"
	|>;


(* === Funzione per generare il codice segreto da indovinare ===
Prende in input la lunghezza del codice da generare come intero, il seed, ed un booleano che ammette o meno la presenza di colori duplicati.
Ritorna tale codice. Esempio: {Red, Purple, Purple, Green} *)
generaCodiceSegreto[lunghezza_Integer, allowDuplicates_:True] := Module[
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


(* Interfaccia della griglia di gioco con selezione di un elemento del primo turno con turni successivi disabilitati*)
interfacciaGriglia[lunghezzaCombinazione_:4, numeroTentativi_:8] := DynamicModule[
{
	gridItemsColors=Table[Opacity[0.2, Black],{numeroTentativi},{lunghezzaCombinazione}],(* Tabella per memorizzare i colori degli elementi, inizialmente tutta nera(opacit\[AGrave] a 0.2)*)
	hintFeedbackHistory = ConstantArray[{}, numeroTentativi],
	turn = 1,(*Numero del tentativo*)
	colorsList=paletteColori,(*Lista di colori della palette di scelta*)
	selectedItem={1,1}, (* Elemento selezionato riga,colonna*)
	soluzioneList=generaCodiceSegreto[lunghezzaCombinazione, False], (* Combinazione segreta *)
	tentativoList=ConstantArray[None, lunghezzaCombinazione], (* Tentativo corrente *)
	valutazioneTentativo={},
	messaggioTemporaneo="",
	taskMessaggio=Null
},
	    Framed[
	    Column[{

				(* Debug tentativo*)
				Column[{
				Spacer[10],
				    Dynamic[
					    Which[ 
				            Length[valutazioneTentativo]>0 && valutazioneTentativo[[1]] === mastermindSconfitta,
				            (
				                partitaInCorso=False;
				                messaggioTemporaneo="";
				                Row[{
								  Style["Hai perso!", FontSize->24, FontColor->Red, FontFamily->"Consolas", Bold],
								  Spacer[20],
					  			(*Bottone TRY*)
						          ClickPane[
							          Framed[
							              Grid[{
							              {
							                Style[labels["randomSeed"], FontSize->20, FontColor->Black],
							                Style[labels["restart"], Black, FontFamily -> "Consolas", FontSize -> 20, Bold]
							              }
							            },
							            Alignment -> {Center, Center}, Spacings -> {1.5, 0}
							            ],
							            Background -> LightGray,
							            FrameStyle -> None,
							            RoundingRadius -> 10,
							            FrameMargins -> {{10, 10}, {10, 10}},
							            ImageSize -> Automatic
							          ],
							          Function[
										gridItemsColors=Table[Opacity[0.2, Black],{numeroTentativi},{lunghezzaCombinazione}];
										hintFeedbackHistory = ConstantArray[{}, numeroTentativi];
										turn = 1;(*Numero del tentativo*)
										colorsList=paletteColori;(*Lista di colori della palette di scelta*)
										selectedItem={1,1}; (* Elemento selezionato riga,colonna*)
										soluzioneList=generaCodiceSegreto[lunghezzaCombinazione, False]; (* Combinazione segreta *)
										tentativoList=ConstantArray[None, lunghezzaCombinazione]; (* Tentativo corrente *)
										valutazioneTentativo={};
										messaggioTemporaneo="";
										taskMessaggio=Null;
										partitaInCorso=True
										]
                                       ]
								}]
				                
				            ),
				            Length[valutazioneTentativo]>0 && valutazioneTentativo[[1]] === mastermindVittoria,
				            (
				                partitaInCorso=False;
				                messaggioTemporaneo="";
				                Row[{
								  Style["Hai vinto!", FontSize -> 24, FontColor -> Darker@Green, FontFamily -> "Consolas", Bold],
								  Spacer[20],
								(*Bottone TRY*)
						          ClickPane[
							          Framed[
							              Grid[{
							              {
							                Graphics[Text[Style[labels["randomSeed"], FontSize -> 12]], ImageSize -> 20],
							                Style[labels["restart"], Black, FontFamily -> "Consolas", FontSize -> 20, Bold]
							              }
							            },
							            Alignment -> {Center, Center}, Spacings -> {1.5, 0}
							            ],
							            Background -> LightGray,
							            FrameStyle -> None,
							            RoundingRadius -> 10,
							            FrameMargins -> {{10, 10}, {10, 10}},
							            ImageSize -> Automatic
							          ],
							          Function[
										gridItemsColors=Table[Opacity[0.2, Black],{numeroTentativi},{lunghezzaCombinazione}];
										hintFeedbackHistory = ConstantArray[{}, numeroTentativi];
										turn = 1;(*Numero del tentativo*)
										colorsList=paletteColori;(*Lista di colori della palette di scelta*)
										selectedItem={1,1}; (* Elemento selezionato riga,colonna*)
										soluzioneList=generaCodiceSegreto[lunghezzaCombinazione, False]; (* Combinazione segreta *)
										tentativoList=ConstantArray[None, lunghezzaCombinazione]; (* Tentativo corrente *)
										valutazioneTentativo={};
										messaggioTemporaneo="";
										taskMessaggio=Null;
										partitaInCorso=True
										]
                                       ]
								}]
				            ),
				            Length[valutazioneTentativo]>0 && valutazioneTentativo[[1]] === mastermindProsegui,
				            (
				                messaggioTemporaneo="Riprova!";
				                If[taskMessaggio =!= Null, RemoveScheduledTask[taskMessaggio]];
				                taskMessaggio=RunScheduledTask[(messaggioTemporaneo=""), 2];
				                Style["", FontSize->20, FontColor->Orange, FontFamily->"Consolas", Bold]
				            ),
				            Length[valutazioneTentativo] === 0,
				            (
				                If[messaggioTemporaneo === "",
				                (
				                    messaggioTemporaneo="Inizia a giocare!";
				                    If[taskMessaggio =!= Null, RemoveScheduledTask[taskMessaggio]];
				                    taskMessaggio=RunScheduledTask[messaggioTemporaneo="", 2.5];
				                )];
				                Style[" ", FontSize->15, FontColor->Orange, FontFamily->"Consolas", Bold]
				            )
				        ],
				        TrackedSymbols:>{valutazioneTentativo}
				    ],
				    Dynamic[
				        If[messaggioTemporaneo =!= "",
				            Style[messaggioTemporaneo, FontSize->15, FontColor->Orange, FontFamily->"Consolas", Bold],
				            ""
				        ]
				    ]  
				}],
				
				Spacer[10],
				
				(*Header*)
				Pane[
				  Grid[{
				    {
				      Style[labels["colori"], FontSize -> 20, FontColor -> Black, FontFamily -> "Consolas"],
				      Style[labels["combinazione"], FontSize -> 20, FontColor -> Black, FontFamily -> "Consolas"],
				      Style[labels["suggerimenti"], FontSize -> 20, FontColor -> Black, FontFamily -> "Consolas"],
				      Style[labels["azione"], FontSize -> 20, FontColor -> Black, FontFamily -> "Consolas"]
				    }
				  },
				  Alignment -> Center,
				  ItemSize -> All,
				  Spacings -> {5, 1}
				  ],
				  Alignment -> Center
				],
				
				Spacer[50],
				
				(* Content *)
				Row[{
					Spacer[60],
					
					(*Palette colori*)
					Column[
					  Table[
						  With[{col=colorsCol},
							  EventHandler[
					  			Dynamic @ Graphics[
								      {
								          EdgeForm[Black],
								          FaceForm[col],
								          Disk[{0, 0}, 1]
								      },
								      ImageSize->40
									],
									{
										"MouseClicked":>(
											gridItemsColors[[Sequence @@ selectedItem]]=col;
											tentativoList[[selectedItem[[2]]]]=col;
											If[selectedItem[[2]] < lunghezzaCombinazione, selectedItem[[2]]=selectedItem[[2]]+1]
										)
									}
								]
							],
					        {colorsCol, colorsList}
						],
						Spacings->1
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
						                          ImageSize->{50, 50}
						                          ],
						                      {
						                          "MouseClicked":>(
						                              If[x === turn,
						                              (
						                                  selectedItem={x, y};
						                              )]
						                          )
						                      }
						                      ]
						                  ],
						                  {col, 1, lunghezzaCombinazione}
						              ],
							      
							          Row[{
							          
							          Spacer[50],
							          
							          (* 2x2 FEEDBACK GRID *)
							          Dynamic @ Module[
							          {
							              (* Sostituisce il feedBack con il colore associato*)
							              feedbackColors=If[hintFeedbackHistory[[x]] =!= {},
							                  (*Length[valutazioneTentativo] > 1(* && row === turn*),*)
							                  hintFeedbackHistory[[x]] /. {
							                      feedbackEsatto->Green,
							                      feedbackParziale->Yellow,
							                      feedbackAssente->None
							                  },
							              ConstantArray[None, lunghezzaCombinazione]
							              ]
							          },
						              Style[
									      Grid[
										      {Table[
											      Graphics[
											          {EdgeForm[Black], FaceForm[hint], Disk[{0, 0}, 1]}, 
											          ImageSize -> 20
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
										                Graphics[Text[Style["\|01f3ae", FontSize -> 12]], ImageSize -> 20],
										                Style[labels["vai"], Black, FontFamily -> "Consolas", FontSize -> 20, Bold]
										              }
										            },
										            Alignment -> {Center, Center}, Spacings -> {1.5, 0}
										            ],
										            Background -> LightGray,
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
										                Graphics[{Opacity[0],Text[Style["\|01f3ae", FontSize -> 12, FontColor->GrayLevel[0]]]}, ImageSize -> 20],
										                Style[labels["vai"], FontFamily -> "Consolas", FontSize -> 20,FontColor->LightGray, Bold]
										              }
										            },
										            Alignment -> {Center, Center}, Spacings -> {1.5, 0}
										            ],
										            Background -> LightGray,
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
creaSchermataGioco[seed_, tentativi_, combinazione_, fontSize_] := Column[{
    Spacer[{0, 50}],
    
    Style[labels["partita"],
        FontSize->fontSize,
        FontWeight->Bold,
        FontColor->Black,
        FontFamily->"Consolas",
        TextAlignment->Center
    ],
      
    Spacer[{0, 25}],
      
    Style[labels["seedSelezionato"] <> ToString[seed], FontSize->24],
      
    Spacer[{0, 25}],
    
    interfacciaGriglia[combinazione, tentativi],
	
	Spacer[{0,25}],
    
(*    Button[
        Style["\:2b05\:fe0f Torna al menu", FontSize->24, FontFamily->"Consolas"],
        cambiaSchermata["menu"],
        Background->LightGray,
        Alignment->Center
    ],*)
    ClickPane[
        Framed[
            Style["\[LeftArrow] Torna al menu", FontSize->24, FontFamily->"Consolas"],
            Background->LightGray,
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
];


End[];
EndPackage[];
