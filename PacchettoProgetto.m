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

(*
  Spiegazione del funzionamento per 'triviaData':
  Questa definizione impiega una tecnica nota come "caricamento differito" (o "lazy loading") con "memoizzazione".
  L'operatore ':=' fa s\[IGrave] che l'operazione specificata (in questo caso, LoadQuestionsFromCSV)
  non venga eseguita immediatamente, ma solo la prima volta che si fa effettivamente uso di 'triviaData', come lei sa bene.

  Durante questo primo utilizzo, l'istruzione interna 'triviaData = ...' esegue due compiti:
  anzitutto, calcola il valore (caricando i dati dal file CSV); in secondo luogo, e crucialmente,
  ridefinisce 'triviaData' stessa, associandola direttamente al valore appena ottenuto.

  Di conseguenza, tutti gli accessi successivi a 'triviaData' restituiranno immediatamente
  questo valore precedentemente memorizzato, senza la necessit\[AGrave] di eseguire nuovamente
  il caricamento o il calcolo. In questo modo, l'operazione pi\[UGrave] onerosa avviene una sola volta.
  
  Calcolando triviaData = LoadQuestions...  dentro  avviaSchermataDiGioco per spostare questa operazione di ~3 sec fuori 
  cos\[IGrave] da non bloccare l'interfaccia azzerava la variabile quando veniva passata a creaSchermataGioco
  (probabilmente per problemi di scoping tra notebook e pacchetto)
  
  Calcolando triviaData = LoadQuestions...  qui sotto come variabile globale porta alla ricalcolazione della variabile decine di volte,
  testato aggiungendo un Print alla funzione di LoadQuestionsFromCSV
  Calcolando triviaData := LoadQuestions... nello stesso modo porta lo stesso problema, entrambi i casi portano a 
  perdite di prestazione, anche con cache automatiche per il risultato
  
  le parentesi chiuse non sono necessarie, ma lo trovo molto meno intuitivo scrivere:
  triviaData := triviaData = LoadQuestionsFromCSV["science-technology.csv"];
  
  le giuro per\[OGrave] che \[EGrave] l'unica volta che uso le parentesi, le ho tolte in tutte le altri parti

  erano presenti un paio di cose extra che pensavo fossero necessario per le variabili globali fatte cos\[IGrave] 
  ma aveva ragione che potevano essere tolte.
*)
triviaData := (
    triviaData = LoadQuestionsFromCSV["science-technology.csv"]
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
interfacciaGriglia[seed_, lunghezzaCombinazione_, numeroTentativi_, allowDuplicates_] :=
    DynamicModule[
        {
            gridItemsColors = Table[Opacity[0.2, Black], {numeroTentativi}, {lunghezzaCombinazione}], (* Tabella per memorizzare i colori degli elementi, inizialmente tutta nera(opacit\[AGrave] a 0.2)*)
            hintFeedbackHistory = ConstantArray[{}, numeroTentativi],
            turn = 1, (*Numero del tentativo*)
            colorsList = paletteColori, (*Lista di colori della palette di scelta*)
            selectedItem = {1, 1}, (* Elemento selezionato riga,colonna*)
            soluzioneList = generaCodiceSegreto[seed, lunghezzaCombinazione, allowDuplicates], (* Combinazione segreta *)
            tentativoList = ConstantArray[None, lunghezzaCombinazione], (* Tentativo corrente *)
            valutazioneTentativo = {},
            questionCounter = 0,
            correct = {}
        },
        Framed[
            Column[{
                Column[{
                    Dynamic[
                        Which[
                            Length[valutazioneTentativo] > 0 && valutazioneTentativo[[1]] === mastermindSconfitta,
                            (
                                partitaInCorso = False;
                                Row[{
                                    (*Bottone TRY*)
                                    ClickPane[
                                        Framed[
                                            Grid[{{
                                                Style[labels["play"], FontSize -> 12, White],
                                                Style[labels["restartPerso"], White, FontSize -> 12, FontFamily -> "Consolas"]
                                            }},
                                            Alignment -> {Center, Center}
                                            ],
                                            Background -> Blue,
                                            FrameStyle -> None,
                                            RoundingRadius -> 10,
                                            FrameMargins -> {{10, 10}, {5, 5}},
                                            ImageSize -> Automatic
                                        ],
                                        Function[
                                            gridItemsColors = Table[Opacity[0.2, Black], {numeroTentativi}, {lunghezzaCombinazione}];
                                            hintFeedbackHistory = ConstantArray[{}, numeroTentativi];
                                            turn = 1; (*Numero del tentativo*)
                                            colorsList = paletteColori; (*Lista di colori della palette di scelta*)
                                            selectedItem = {1, 1}; (* Elemento selezionato riga,colonna*)
                                            (*SeedRandom[seed];*)
                                            soluzioneList = generaCodiceSegreto[seed, lunghezzaCombinazione, allowDuplicates]; (* Combinazione segreta *)
                                            tentativoList = ConstantArray[None, lunghezzaCombinazione]; (* Tentativo corrente *)
                                            valutazioneTentativo = {};
                                            partitaInCorso = True;
                                            questionCounter = 0;
                                            correct = {};
                                        ]
                                    ]
                                }]
                            ),
                            Length[valutazioneTentativo] > 0 && valutazioneTentativo[[1]] === mastermindVittoria,
                            (
                                partitaInCorso = False;
                                Row[{
                                    (*Bottone TRY*)
                                    ClickPane[
                                        Framed[
                                            Grid[{{
                                                Style[labels["play"], FontSize -> 12, White],
                                                Style[labels["restartVinto"], White, FontFamily -> "Consolas", FontSize -> 12]
                                            }},
                                            Alignment -> {Center, Center}
                                            ],
                                            Background -> RGBColor[0, 0.5, 0],
                                            FrameStyle -> None,
                                            RoundingRadius -> 10,
                                            FrameMargins -> {{10, 10}, {5, 5}},
                                            ImageSize -> Automatic
                                        ],
                                        Function[
                                            gridItemsColors = Table[Opacity[0.2, Black], {numeroTentativi}, {lunghezzaCombinazione}];
                                            hintFeedbackHistory = ConstantArray[{}, numeroTentativi];
                                            turn = 1; (*Numero del tentativo*)
                                            colorsList = paletteColori; (*Lista di colori della palette di scelta*)
                                            selectedItem = {1, 1}; (* Elemento selezionato riga,colonna*)
                                            (*SeedRandom[seed];*)
                                            soluzioneList = generaCodiceSegreto[seed, lunghezzaCombinazione, allowDuplicates]; (* Combinazione segreta *)
                                            tentativoList = ConstantArray[None, lunghezzaCombinazione]; (* Tentativo corrente *)
                                            valutazioneTentativo = {};
                                            partitaInCorso = True;
                                            questionCounter = 0;
                                            correct = {};
                                        ]
                                    ]
                                }]
                            ),
                            True, Style["", FontSize -> 0]
                        ],
                        TrackedSymbols :> {valutazioneTentativo}
                    ]
                }],

                (* Content *)
                Row[{
                    Spacer[20],

                    (*Palette colori*)
                    Grid[
                        Partition[
                            Table[
                                With[{col = colorsCol},
                                    EventHandler[
                                        Dynamic @ Graphics[
                                            {
                                                EdgeForm[Black],
                                                FaceForm[col],
                                                Disk[{0, 0}, 1]
                                            },
                                            ImageSize -> 35
                                        ],
                                        {
                                            "MouseClicked" :> (
                                                If[partitaInCorso, (* Per fermare ulteriori interazioni una volta conclusa la partita *)
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
                        Spacings -> {1, 1},
                        Alignment -> Center
                    ],

                    Spacer[80],

                    (*Griglia di gioco*)
                    Grid[
                        Table[
                            With[{x = row},
                                Append[
                                    Table[
                                        With[{y = col, id = lunghezzaCombinazione*(row - 1) + col},
                                            EventHandler[
                                                Dynamic @ Graphics[
                                                    {
                                                        EdgeForm[
                                                            If[{x, y} === selectedItem,
                                                                Directive[Black, AbsoluteThickness[1]],
                                                                None(*, Directive[Black]*)
                                                            ]
                                                        ],
                                                        If[x <= turn, gridItemsColors[[x, y]], Opacity[0.1, Black]],
                                                        Disk[{0, 0}, 1]
                                                    },
                                                    ImageSize -> {35, 35}
                                                ],
                                                {
                                                    "MouseClicked" :> (
                                                        If[partitaInCorso && x === turn, (* Per fermare ulteriori interazioni una volta conclusa la partita, e per agire solo sulla riga corrente *)
                                                            If[tentativoList[[y]] =!= None, (* Se clicchi un colore nel tentativo, rimuovilo *)
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
                                                hintFeedbackHistory[[x]][[All, 2]], (* Estrae i simboli di feedback (il secondo elemento di ogni coppia) dalla cronologia dei tentativi per la riga x *)
                                                ConstantArray[feedbackAssente, lunghezzaCombinazione] (* Se non ci sono dati per questa riga (es. turno non ancora giocato), usa un array di feedback assenti *)
                                            ];

                                            feedbackColors = feedbackSymbolsForDisplay /. {
                                                feedbackEsatto -> RGBColor[0.57, 1, 0.05],
                                                feedbackParziale -> RGBColor[1, 0.85, 0],
                                                feedbackAssente -> None (* 'None' qui significa che il piolo di feedback non avr\[AGrave] un colore visibile *)
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
                                                (* Se \[EGrave] il turno corrente, mostra il pulsante "VAI" attivo *)
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
                                                        (* Prima di processare il tentativo, assicuriamoci che esista una entry per l'eventuale aiuto in questa riga. *)
                                                        (* Questo serve per mantenere l'allineamento della griglia degli aiuti, anche se l'aiuto non viene usato. *)
                                                        If[Length[correct] < x,
                                                            AppendTo[correct, {}];
                                                        ];
                                                        If[partitaInCorso,
                                                            (
                                                                valutazioneTentativo = valutaTentativo[soluzioneList, tentativoList, numeroTentativi, turn];

                                                                (* Aggiorniamo la cronologia dei feedback con il tentativo corrente. *)
                                                                Module[{currentTurnFeedbackSymbols = valutazioneTentativo[[2]], combinedTurnData},
                                                                    (* Combiniamo ogni colore tentato con il suo simbolo di feedback. *)
                                                                    combinedTurnData = Table[
                                                                        {tentativoList[[i]], currentTurnFeedbackSymbols[[i]]},
                                                                        {i, Length[tentativoList]}
                                                                    ];
                                                                    hintFeedbackHistory[[turn]] = combinedTurnData;
                                                                ];

                                                                If[valutazioneTentativo[[1]] === mastermindProsegui, turn++]; (* Se il gioco continua, passa al turno successivo *)
                                                                selectedItem = {turn, 1}; (* Reimposta la selezione per il prossimo input *)
                                                                tentativoList = ConstantArray[None, lunghezzaCombinazione]; (* Svuota la lista del tentativo per il prossimo turno *)
                                                            )
                                                        ]
                                                    ]
                                                ],
                                                (* Altrimenti, mostra una versione disabilitata (grigia) del pulsante "VAI" *)
                                                Framed[
                                                    Grid[{
                                                        {
                                                            Style["\|01f3ae", FontSize -> 10, FontColor -> Directive[GrayLevel[0.9], Opacity[0]]],
                                                            Style[labels["vai"], FontFamily -> "Consolas", FontSize -> 12, FontColor -> GrayLevel[0.9], Bold]
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
                                        ],

                                        Spacer[50], (* Spazio prima del pulsante Hint *)
                                        (* Definiamo un valore per rappresentare uno stato in cui il risultato per l'aiuto (correct[[x]]) non \[EGrave] ancora disponibile. *)
                                        emptyResultPlaceholder = Missing["NoResultSetYet"];
                                        Dynamic[ 
                                            Module[
                                                {
                                                    currentValForRowX, (* Conterr\[AGrave] il valore elaborato di correct[[x]] (l'esito di un aiuto) o il segnaposto *)
                                                    displayOutput (* Conterr\[AGrave] l'elemento UI finale da visualizzare (pulsante HINT, risultato dell'aiuto, o segnaposto) *)
                                                },

                                                (* Fase 1: Determina il valore attuale per la riga x, relativo all'esito di un eventuale aiuto. *)
                                                currentValForRowX = If[
                                                    ListQ[correct] && Length[correct] >= x && x >= 1 &&
                                                    correct[[x]] =!= Null && correct[[x]] =!= {} && correct[[x]] =!= emptyResultPlaceholder, (* Assicuriamoci che correct[[x]] sia valido e non un segnaposto *)
                                                    correct[[x]], (* Usa il valore reale da correct[[x]] *)
                                                    emptyResultPlaceholder (* Altrimenti, usa il segnaposto *)
                                                ];

                                                (* Fase 2: Determina quale interfaccia utente visualizzare in base a currentValForRowX *)
                                                displayOutput = Which[

                                                    (* Caso 1: Il risultato per la riga x indica che la risposta alla domanda per l'aiuto \[EGrave] stata sbagliata. *)
                                                    currentValForRowX === Missing["WrongAnswer"],
                                                    Framed[
                                                        Style["\:274c", FontSize -> 18],
                                                        Background -> GrayLevel[0.95], 
                                                        FrameStyle -> Red, 
                                                        RoundingRadius -> 10,
                                                        FrameMargins -> {{10, 10}, {0, 0}},
                                                        ImageSize -> {80, 35},
                                                        Alignment -> Center
                                                    ],

                                                    (* Caso 2: Il risultato \[EGrave] una risposta corretta alla domanda per l'aiuto (tipicamente un indizio del tipo {colore, valore/posizione}). *)
                                                    MatchQ[currentValForRowX, {_?ColorQ, _}], (* Controlla se currentValForRowX corrisponde al pattern {un Colore, un qualcheValore} *)
                                                    Module[ 
                                                        {
                                                            resultColor = First[currentValForRowX],
                                                            resultValue = Last[currentValForRowX] 
                                                        },
                                                        Framed[
                                                            (* Il contenuto del box verde "Indizio Corretto" dipende da 'resultValue'. *)
                                                            If[resultValue =!= Missing["NoSimpleHintAvailable"],
                                                                If[resultValue =!= Missing["PositionNotApplicable"],
                                                                    (* Se l'indizio ha un valore di posizione specifico: mostra colore + valore *)
                                                                    Row[{
                                                                        Graphics[{EdgeForm[Gray], resultColor, Disk[]}, ImageSize -> {20, 20}],
                                                                        Spacer[5],
                                                                        Column[{
																        Style[ToString[resultValue], 16, Bold, FontFamily -> "Arial"] (*mette apposto lo spazio verticale, 
																        pensavo di dover metter Spacer ma Column lo sposta gi\[AGrave] abbastanza*)
																    },
																    Spacings -> 0
																     ]
                                                                    }],
                                                                    (* Altrimenti (l'indizio indica che la posizione non \[EGrave] applicabile o non c'\[EGrave] un indizio semplice): mostra solo il colore *)
                                                                    Graphics[{EdgeForm[Gray], resultColor, Disk[]}, ImageSize -> {20, 20}]
                                                                ],
                                                                "" (* Se non ci sono altri suggerimenti da dare \[EGrave] vuoto *)
                                                            ],
                                                            Background -> GrayLevel[0.95], 
                                                            FrameStyle -> Darker[Green],
                                                            RoundingRadius -> 10,
                                                            FrameMargins -> {{30, 10}, {6, 6}},
                                                            ImageSize -> {80, 35}
                                                        ]
                                                    ],

                                                    (* Caso 3: Nessun risultato specifico ancora disponibile (cio\[EGrave], currentValForRowX \[EGrave] emptyResultPlaceholder). *)
                                                    (* Decider\[AGrave] se mostrare un pulsante "HINT" attivo o un segnaposto inattivo. *)
                                                    True,
                                                    If[x === turn && triviaData =!= $Failed,
                                                        (* --- Mostra il Pulsante "HINT" Attivo --- *)
                                                        (* Condizioni: la riga corrente (x) \[EGrave] il turno attivo, la partita \[EGrave] in corso, e i dati trivia sono caricati. *)
                                                        ClickPane[
                                                            Framed[ (* Aspetto visivo del pulsante HINT attivo *)
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
                                                            Function[ 
                                                                (* 'AppendTo' aggiunge l'esito della domanda per l'aiuto. Questo \[EGrave] coerente con la logica del pulsante "VAI",
                                                                dove aggiungiamo {} per mantenere la struttura anche se l'aiuto non viene usato. *)
                                                                If[partitaInCorso,
                                                                    AppendTo[correct, DisplayTriviaQuestion[seed + questionCounter, CalcolaHintSemplice[hintFeedbackHistory, soluzioneList]]];
                                                                    questionCounter++; (* Incrementa un contatore, per avere seed unici per le domande trivia *)
                                                                ]
                                                            ],
                                                            Method -> "Queued"
                                                        ],

                                                        (* --- Mostra un Pulsante "HINT" Inattivo/Segnaposto --- *)
                                                        (* Mostrato se le condizioni per un pulsante HINT attivo non sono soddisfatte, 
                                                        \[EGrave] un copia incolla di quello sopra per preservare interazione eventuale tra le cose, e per evitare problemi  *)
                                                        Framed[
                                                            Grid[{
                                                                {
                                                                    Style["\|01f4a1", FontSize -> 10, FontColor -> Directive[GrayLevel[0.7], Opacity[0]]], (* Icona resa invisibile *)
                                                                    Style["HINT", FontFamily -> "Consolas", FontSize -> 12, FontColor -> Directive[GrayLevel[0.7], Opacity[0]], Bold] (* Testo reso invisibile *)
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
                                                    ]
                                                ];

                                                displayOutput (* Dynamic si valuta a questo elemento UI *)
                                            ]
                                        ]
                                    }]
                                ]
                            ],
                            {row, 1, numeroTentativi}
                        ]
                    ]
                },
                Alignment -> {{Left, Center, Center, Right}}
                ],
                Spacer[20]
            },
            Alignment -> Center
            ],
            Background -> GrayLevel[0.9],
            FrameStyle -> None,
            RoundingRadius -> 15,
            FrameMargins -> {{15, 15}, {5, 5}},
            ImageSize -> Automatic
        ]
    ]
    
    (*
  Costruisce e restituisce l'interfaccia utente dinamica per la schermata di gioco principale.
  Questa schermata \[EGrave] composta da una barra superiore, contenente un pulsante per tornare al menu
  e l'indicazione del seme (seed) della partita corrente, e dall'area di gioco principale
  dove viene visualizzata e aggiornata la griglia interattiva.

  Parametri:
    seed_: il seme casuale che determina la combinazione segreta e altri aspetti della partita.
    tentativi_: informazioni relative ai tentativi effettuati o massimi.
    combinazione_: la combinazione segreta da indovinare (potrebbe non essere usata direttamente qui se gestita altrove).
    allowDuplicates_: valore booleano che indica se la combinazione segreta pu\[OGrave] contenere colori duplicati.
    fontSize_: dimensione del carattere per alcuni testi nell'interfaccia.
*)
creaSchermataGioco[seed_, tentativi_, combinazione_, allowDuplicates_, fontSize_] :=
DynamicModule[{},
  Pane[
    Column[{
      Panel[ (* Barra superiore contenente i controlli e informazioni sulla partita *)
        Row[{
          ClickPane[
            Framed[
              Style[labels["menu"], White, FontSize -> 12, FontFamily -> "Consolas", Bold],
              Background -> Red, (* Pulsante "menu" con sfondo rosso per visibilit\[AGrave] *)
              FrameStyle -> None,
              RoundingRadius -> 5,
              FrameMargins -> {{6, 6}, {3, 3}}
            ],
            Function[
              cambiaSchermata["menu"] (* Azione: torna alla schermata del menu principale *)
            ]
          ],
          Spacer[5],
          (* Visualizzazione del seme (seed) della partita corrente *)
          Style[labels["seedSelezionato"] <> ToString[seed], FontSize -> 12, FontFamily -> "Consolas", FontColor -> Red, Bold]
        },
        Alignment -> Center
        ],
        Background -> White (* Sfondo bianco per la barra superiore *)
      ],
      Dynamic[ (* L'area della griglia di gioco si aggiorna dinamicamente *)
        Pane[
          interfacciaGriglia[seed, combinazione, tentativi, allowDuplicates], (* Funzione che genera la griglia di gioco effettiva *)
          {Automatic, Scaled[0.7]}, 
          Scrollbars -> False, 
          Alignment -> Center 
        ]
      ]
    },
    Alignment -> Center
    ],
    Alignment -> Center,
    ImageSize -> Scaled[1] (* Il pannello principale occupa l'intera area disponibile *)
  ]
];

(*
  Carica le domande da un file CSV, le elabora e le restituisce come un Dataset strutturato.
  
  Parametri:
    path_String: Il percorso completo del file CSV da cui importare le domande.

  Valore di ritorno:
    Un Dataset Mathematica in cui ogni riga \[EGrave] un'associazione (nome_colonna -> valore_dato),
    oppure $Failed se si verifica un errore durante il caricamento del file o
    l'interpretazione del suo contenuto come dati CSV.
*)
LoadQuestionsFromCSV[path_String] := Module[
  {csvText, data, headers, rows, dataset},

  (* Fase 1: Importa il contenuto grezzo del file CSV come testo. *)
  (* Si utilizza Quiet@Check per gestire errori di importazione senza interrompere bruscamente. *)
  csvText = Quiet@Check[
    Import[path, "Text"], (* Importa l'intero file come una singola stringa *)
    (Print["\:274c Errore: Impossibile importare il testo dal CSV."]; Return[$Failed]) (* Gestione errore importazione testo *)
  ];

  (* Fase 2: Se l'importazione del testo \[EGrave] riuscita, interpreta la stringa come dati CSV. *)
  data = Quiet@Check[
    ImportString[csvText, "CSV"], (* Converte la stringa di testo in una lista di liste (righe/colonne) *)
    (Print["\:274c Errore: Impossibile interpretare il contenuto CSV."]; Return[$Failed]) (* Gestione errore interpretazione CSV *)
  ];

  (* Fase 3: Trasforma i dati grezzi del CSV in un Dataset per una manipolazione pi\[UGrave] agevole. *)
  headers = data[[1]]; (* La prima riga del CSV \[EGrave] assunta come intestazione *)
  rows = data[[2 ;;]]; (* Le righe rimanenti sono i dati effettivi *)
  (* Crea un Dataset dove ogni riga \[EGrave] un'associazione tra l'intestazione e il valore corrispondente. *)
  dataset = Dataset[AssociationThread[headers, #] & /@ rows];

  dataset
];


(* Mostra una finestra di dialogo (dialog) con una domanda trivia e opzioni a scelta multipla. *)
(* Il dialogo \[EGrave] modale, quindi la funzione sospende l'esecuzione finch\[EAcute] il dialogo non viene chiuso. *)
(* @param seed: Numero intero usato per selezionare e mescolare la domanda tramite PrepareQuestionData. *)
(* @param hintToGive: La struttura dell'indizio (solitamente {colore, posizione_o_codice_mancante}) che verr\[AGrave] mostrata se la risposta \[EGrave] corretta. Questa stessa struttura sar\[AGrave] il valore restituito dalla funzione in caso di risposta corretta. *)
(* @return: La struttura 'hintToGive' se \[EGrave] stata selezionata la risposta corretta. *)
(* Restituisce Missing["WrongAnswer"] se \[EGrave] stata selezionata una risposta errata o se il dialogo \[EGrave] stato chiuso prematuramente (es. tramite il pulsante di chiusura della finestra del sistema operativo). *)
(* Restituisce $Failed se il dialogo \[EGrave] stato chiuso prima di qualsiasi interazione e il risultato non \[EGrave] stato impostato esplicitamente (raro, data la gestione con NotebookEventActions). *)
DisplayTriviaQuestion[seed_Integer, hintToGive_] := Module[
  {
    (* --- Variabili del modulo esterno (DisplayTriviaQuestion) --- *)
    questionWindow,                (* Memorizza l'oggetto Dialog (la finestra stessa) *)
    result = $Failed,              (* Risultato finale da restituire. Inizializzato a $Failed come valore di default. *)
    dialogOpen = True,             (* Flag per controllare il ciclo While, rendendo la funzione sincrona (attende la chiusura del dialogo). *)
    
    (* Dati della domanda, preparati una sola volta prima di creare il dialogo (output da PrepareQuestionData) *)
    initialQuestionData,           
    initialOptionsData,            
    initialCorrectIndexData,       
    
    stopDialogLoopFunc             (* Funzione per gestire la chiusura prematura del dialogo (es. con il tasto 'x' della finestra). *)
  },

  (* stopDialogLoopFunc:
    Questa funzione viene chiamata quando la finestra di dialogo \[EGrave] chiusa usando il pulsante nativo del sistema operativo (ad es. la 'x').
    Assicura che il ciclo While principale in DisplayTriviaQuestion termini e imposti un risultato appropriato.
  *)
  stopDialogLoopFunc = Function[{}, 
    (* Chiama performCloseAction per impostare il risultato e chiudere il dialogo in modo pulito. *)
    (* Consideriamo la chiusura della finestra come una risposta errata (Missing["WrongAnswer"]), dato che l'utente ha gi\[AGrave] visto la domanda. *)
    performCloseAction[Missing["WrongAnswer"]];
  , HoldAll]; 

  (* Prepara i dati della domanda (testo, opzioni, indice corretto) una sola volta, usando il seed fornito. *)
  {initialQuestionData, initialOptionsData, initialCorrectIndexData} = PrepareQuestionData[seed];

  questionWindow = CreateDialog[
    DynamicModule[
      { 
        displayState = "question", (* Controlla la vista corrente del dialogo: "question" (mostra domanda), "correct_show_hint" (risposta corretta), "incorrect_show_message" (risposta errata). *)
        
        (* Copie locali dei dati della domanda, inizializzate dallo scope del Modulo esterno perch\[EGrave] ho avuto problemi di scope *)
        localQuestion = initialQuestionData,
        localOptions = initialOptionsData,
        localCorrectIndex = initialCorrectIndexData
      },
      
      (*
        performCloseAction[currentDynamicResult_]:
        Questa funzione interna viene chiamata dai pulsanti "Chiudi" nel dialogo.
        Imposta le variabili 'result' e 'dialogOpen' del Modulo esterno DisplayTriviaQuestion.
        usato per superare problemi di scoping
      *)
      performCloseAction[currentDynamicResult_] := (
        result = currentDynamicResult; (* Imposta il risultato finale di DisplayTriviaQuestion. *)
        dialogOpen = False;           (* Segnala al ciclo While esterno di terminare e ritornare il volare. *)
        NotebookClose[EvaluationNotebook[]]; (* Chiude programmaticamente questo specifico notebook di dialogo. *)
      );
      
      (*
        L'interfaccia utente principale del dialogo. \[CapitalEGrave] avvolta in Dynamic e Refresh in modo che si aggiorni
        automaticamente quando 'displayState' cambia, mostrando la vista appropriata.
      *)
      Dynamic@Refresh[
        Switch[displayState,

          "question",
          Column[{
            Pane[
              Style[localQuestion["Question"], 16, Bold, TextAlignment -> Center], 
              ImageSize -> {500,120}, 
              Scrollbars -> False, Alignment -> Center
            ],
            Spacer[20],
            Grid[
              Partition[ (* Dispone i pulsanti di risposta in una griglia, solitamente 2 colonne. *)
                MapIndexed[
                  Function[{optionText, optionIndex}, 
                    DynamicModule[{clicked = False, isCorrect = Null, position = First[optionIndex]}, 
                      Button[
                        optionText,
                        (
                          isCorrect = (position == localCorrectIndex); 
                          clicked = True; (* Contrassegna questo pulsante come cliccato per il feedback visivo. *)
                          
                          (* Passa alla vista di feedback appropriata. *)
                          If[isCorrect,
                            displayState = "correct_show_hint",
                            displayState = "incorrect_show_message"
                          ];
                        ),
                        Background -> Dynamic[If[clicked, If[isCorrect, Green, Red], White]], (* Colore di sfondo dinamico: verde se corretto e cliccato, rosso se errato e cliccato, altrimenti bianco. *)
                        ImageSize -> {200, 80},
                        BaseStyle -> {FontColor -> Black, FontWeight -> Bold, FontFamily -> "Arial", FontSize -> 14},
                        FrameMargins -> 12
                      ] 
                    ] 
                  ], 
                  localOptions 
                ], 
                UpTo[Ceiling[Length[localOptions]/2]] (* Controlla il numero di elementi per riga nella griglia (tipicamente 2 per riga). *)
              ], 
              Spacings -> {1, 1}, Alignment -> Center
            ] 
          }, Alignment -> Center], 

          "correct_show_hint",
          (* Mostrata quando l'Utente Risponde Correttamente *)
          Column[{
            Style["Corretto!", Bold, Green, FontFamily -> "Arial", FontSize -> 36, TextAlignment -> Center],
            Pane[
              (* Questo Modulo interno serve solo per localizzare 'theCol' e 'posVal', se necessario, usato perch\[EGrave] ho avuto problemi di scoping *)
              Module[{theCol, posVal},
                {theCol, posVal} = hintToGive; (* Scompatta la struttura dell'indizio 'hintToGive'. *)
                
                (* Mostra l'indizio in base alla sua struttura. *)
                Which[
                  posVal === Missing["PositionNotApplicable"],
                  Row[{
                    Style["Questo colore \[EGrave] presente nella combinazione:", Medium, FontFamily -> "Arial", FontSize -> 18],
                    Spacer[8],
                    Tooltip[Graphics[{EdgeForm[Gray], theCol, Disk[]}, ImageSize -> {25, 25}], ToString[theCol]]
                  }, Alignment -> Center],

                  posVal === Missing["NoSimpleHintAvailable"],
                  Row[{
                    Style["Nessun indizio disponibile, hai tutte le informazioni", Medium, FontFamily -> "Arial", FontSize -> 18]
                  }, Alignment -> Center],
                  
                  True, (* Caso di default: si assume che posVal sia un intero che rappresenta una posizione. *)
                  Row[{
                    Style["Il colore: ", Medium, FontFamily -> "Arial", FontSize -> 18], Spacer[8],
                    Tooltip[Graphics[{EdgeForm[Gray], theCol, Disk[]}, ImageSize -> {25, 25}], ToString[theCol]], Spacer[8],
                    Style["\[EGrave] alla posizione", Medium, FontFamily -> "Arial", FontSize -> 18], Spacer[8],
                    Style[ToString[posVal], Medium, Bold, FontFamily -> "Arial", FontSize -> 24]
                  }, Alignment -> Center]
                ] 
              ], 
              {500, Automatic}, 
              Alignment -> Center
            ],
            Button[ (* Pulsante "Chiudi" per "Corretto!" *)
              Style["Chiudi", Bold, FontSize -> 24],
              performCloseAction[hintToGive] (* Restituisce la struttura originale hintToGive come risultato. *)
            ]
          }, Alignment -> Center, Spacings -> 15], 

          "incorrect_show_message",
          (*Mostrata quando l'Utente Risponde Incorrettamente  *)
          Column[{
            Pane[
              Style["Incorretto!", 36, Bold, Red, FontFamily -> "Arial", TextAlignment -> Center],
              {500, Automatic}, Alignment -> Center
            ],
            Pane[
               Style["La risposta corretta era: ", Medium, FontFamily -> "Arial", FontSize -> 18],
              {500, Automatic}, Alignment -> Center
            ],
            Pane[
                  Style[localOptions[[localCorrectIndex]], Medium, Bold, FontFamily -> "Arial", FontSize -> 18],
              {500, Automatic}, Alignment -> Center
            ],
            Button[ (* Pulsante "Chiudi" per "Incorretto!" *)
              Style["Chiudi", Bold, FontSize -> 24],
              performCloseAction[Missing["WrongAnswer"]] (* Restituisce Missing["WrongAnswer"] come risultato. *)
            ]
          }, Alignment -> Center, Spacings -> 8], 

          _, (* Caso di default per Switch[displayState, ...]: gestisce qualsiasi stato imprevisto. *)
          Style["Errore: Lo stato di visualizzazione del dialogo non \[EGrave] valido. Si prega di segnalare.", Red, Bold]
          
        ], 
        TrackedSymbols :> {displayState} (* Assicura che il contenuto Dinamico si aggiorni solo quando 'displayState' cambia. *)
      ] 
    ], 
    
    WindowTitle -> "Suggerimento Trivia Mastermind",
    WindowSize -> {520, 400}, 
    Modal -> True,              (* Il dialogo blocca l'interazione con altri notebook. *)
    WindowElements -> {},       
    WindowFrame -> "ModalDialog", (* Cornice standard per dialoghi modali, di solito include un pulsante di chiusura del SO. *)
    Background -> White,
    NotebookEventActions -> {
      "WindowClose" :> stopDialogLoopFunc[] (* Azione personalizzata quando il pulsante di chiusura del SO viene cliccato. *)
    }
  ]; 
  
  (* Questo ciclo sospende l'esecuzione di DisplayTriviaQuestion finch\[EAcute] 'dialogOpen' non diventa False *)
  (* in modo che il risultato venga passato solo dopo la chiusura del dialogo e l'utente abbia avuto la possibilit\[AGrave] di rispondere. *)
  While[dialogOpen, Pause[0.1]];
  
  
  result (* Il valore impostato da performCloseAction o stopDialogLoopFunc. *)
];


(*
  Prepara i dati di una domanda: seleziona la domanda in base al seed,
  estrae le opzioni di risposta e l'indice della risposta corretta.

  @param seed: Intero usato per la selezione (pseudo)casuale della domanda.
  @return: Lista contenente {domandaSelezionata, opzioniDisponibili, indiceRispostaCorretta}.
*)
PrepareQuestionData[seed_Integer] := Module[
  {questionIndex, options, rawCorrectIndex, selectedQuestion, optionKeys, correctIndex},
  
  SeedRandom[seed];
  
  questionIndex = Mod[seed, Length[triviaData], 1];
  selectedQuestion = Normal[triviaData[[questionIndex]]];

  rawCorrectIndex = Lookup[selectedQuestion, "Correct Index", Missing["NotAvailable"]];
  rawCorrectIndex = If[NumberQ[rawCorrectIndex], Round[rawCorrectIndex], 1];

  optionKeys = {"Option A", "Option B", "Option C", "Option D"};
  options = DeleteCases[Lookup[selectedQuestion, optionKeys, ""], _Missing | "" | Null];

  {selectedQuestion, options, rawCorrectIndex}
];


(*
  Calcola un indizio per un gioco tipo Mastermind, basato sulla cronologia dei tentativi e sulla soluzione.
  Usa Catch/Throw per gestire tutti i percorsi di uscita.

  Priorit\[AGrave] degli Indizi (la funzione restituisce il primo indizio applicabile in questa lista):
  1. Nessun Tentativo Giocato: Se non ci sono ancora stati tentativi, suggerisce un colore casuale dalla soluzione.
  2. Colore della Soluzione Non Confermato: Suggerisce un colore presente nella soluzione che non ha ancora 
      ricevuto n\[EAcute] 'feedbackEsatto' n\[EAcute] 'feedbackParziale' in nessun tentativo.
  3. Colore con Feedback Parziale (Mai Esatto): Se tutti i colori della soluzione sono stati 'confermati' 
      (hanno ricevuto qualche feedback), suggerisce un colore che ha ricevuto 'feedbackParziale' ma mai 
      'feedbackEsatto', insieme alla sua posizione corretta nella soluzione.
  4. Fallback: Se nessun altro indizio specifico pu\[OGrave] essere generato.

  @param hintFeedbackHistoryInput_List: Cronologia dei turni. Ogni turno \[EGrave] una lista di 
                                        {{colore_tentato, simbolo_feedback}, ...}. 
                                        Turni vuoti {} (non ancora giocati) sono ammessi.
  @param soluzioneListInput_List: Lista dei colori che compongono la soluzione segreta.

  @return (tramite Throw): Uno dei seguenti:
    - Missing["SolutionIsEmpty"]: Se 'soluzioneListInput' \[EGrave] vuota.
    - {colore, Missing["PositionNotApplicable"]}: Indizio senza posizione specifica (Priorit\[AGrave] 1 e 2).
    - {colore, posizione_Integer}: Indizio con posizione (Priorit\[AGrave] 3).
    - {colore_placeholder, Missing["NoSimpleHintAvailable"]}: Indizio di fallback (Priorit\[AGrave] 4).
*)
CalcolaHintSemplice[hintFeedbackHistoryInput_List, soluzioneListInput_List] := Catch[
  Module[
    {
      soluzioneList = soluzioneListInput,
      hintFeedbackHistory = hintFeedbackHistoryInput,
      n = Length[soluzioneListInput],

      actualTurnsData, (* Lista dei turni effettivamente giocati (filtrati da quelli vuoti {} ) *)
      uniqueSolutionColors,
      (* Associazione: colore_soluzione -> True se ha ricevuto feedback esatto/parziale, altrimenti False *)
      confirmedSolutionColors,

      colorList = {}, (* Lista per Priorit\[AGrave] 3: colori che hanno ricevuto 'feedbackParziale' e non 'feedbackEsatto' *)
      
      (* Variabili di iterazione e temporanee usate nei cicli *)
      currentTurnData, currentPegData, guessedColor, feedbackSymbol, colorKey,
      lastTurnData, turnIter, pegIter, solColorIter, i, j
    },
    
    If[n == 0,
      Throw[Missing["SolutionIsEmpty"]]
    ];

    (* --- Priorit\[AGrave] 1: Gestione del caso "Nessun Tentativo Giocato" --- *)
    actualTurnsData = Select[hintFeedbackHistory, # =!= {} &]; (* Filtra e conserva solo i turni con dati (non vuoti) *)

    If[Length[actualTurnsData] == 0,
      (* Nessun tentativo ancora fatto: suggerisce un colore casuale dalla soluzione. *)
      Throw[{RandomChoice[soluzioneList], Missing["PositionNotApplicable"]}]
    ];

    (* --- Priorit\[AGrave] 2: Cerca colori della soluzione non ancora "confermati" (mai ricevuto feedbackEsatto o feedbackParziale) --- *)
    uniqueSolutionColors = DeleteDuplicates[soluzioneList];
    (* Inizializza 'confirmedSolutionColors' per tracciare i colori della soluzione che hanno ricevuto un feedback utile. *)
    confirmedSolutionColors = Association[# -> False & /@ uniqueSolutionColors];

    Do[
      currentTurnData = turnIter;
      Do[
        currentPegData = pegIter;
        guessedColor = currentPegData[[1]];
        feedbackSymbol = currentPegData[[2]];
        
        If[feedbackSymbol === feedbackEsatto || feedbackSymbol === feedbackParziale,
          If[KeyExistsQ[confirmedSolutionColors, guessedColor],
            (* Segna questo colore (se parte della soluzione) come "confermato" avendo ricevuto un feedback. *)
            confirmedSolutionColors = AssociateTo[confirmedSolutionColors, guessedColor -> True];
          ]
        ];
      , {pegIter, currentTurnData}];
    , {turnIter, actualTurnsData}];

    (* Controlla se esiste un colore della soluzione non ancora confermato. *)
    Do[
      colorKey = solColorIter;
      If[Lookup[confirmedSolutionColors, colorKey, True] === False, (* Default a True per sicurezza se non trovato, ma dovrebbe esistere. *)
        (* Suggerisci questo colore non confermato; la posizione non \[EGrave] rilevante. *)
        Throw[{colorKey, Missing["PositionNotApplicable"]}]
      ];
    , {solColorIter, uniqueSolutionColors}];

    (* --- Priorit\[AGrave] 3: Indizio per un colore con 'feedbackParziale' ma mai 'feedbackEsatto' --- *)
    (* Obiettivo: se tutti i colori della soluzione sono "confermati", identifica un colore che ha ricevuto *)
    (* 'feedbackParziale' in passato, ma MAI 'feedbackEsatto', e suggeriscilo con la sua posizione corretta. *)

    (* 3a: Raccogli tutti i colori che hanno ricevuto 'feedbackParziale' in qualsiasi tentativo. *)
    For[i = 1, i <= Length[actualTurnsData], i++,
      lastTurnData = actualTurnsData[[i]];
      For[j = 1, j <= n, j++,
        Module[{colorInGuess, feedbackForPeg},
          colorInGuess = lastTurnData[[j, 1]];
          feedbackForPeg = lastTurnData[[j, 2]];
          If[feedbackForPeg === feedbackParziale, AppendTo[colorList, colorInGuess]];
        ];
      ];
    ];

    (* 3b: Dalla lista appena creata, rimuovi i colori che hanno ricevuto anche solo una volta 'feedbackEsatto'. *)
    For[i = 1, i <= Length[actualTurnsData], i++,
      lastTurnData = actualTurnsData[[i]];
      For[j = 1, j <= n, j++,
        Module[{colorInGuess, feedbackForPeg},
          colorInGuess = lastTurnData[[j, 1]];
          feedbackForPeg = lastTurnData[[j, 2]];
          If[feedbackForPeg === feedbackEsatto, colorList = DeleteCases[colorList, colorInGuess]];
        ];
      ];
    ];

    (* 'colorList' ora contiene colori candidati (potrebbero esserci duplicati e colori non presenti nella soluzione). *)

    (* 3c: Se ci sono colori candidati, prendi il primo, verifica che sia valido e trova la sua posizione reale nella soluzione. *)
    If[colorList =!= {},
      Module[{targetHintColor = First[colorList], firstOccurrencePositionInSolution},
        (* Questo ciclo assicura che il 'targetHintColor' selezionato abbia effettivamente ricevuto 'feedbackParziale'. *)
        (* Serve come ulteriore conferma della logica, specialmente se 'colorList' potesse contenere colori non validi. *)
        For[i = 1, i <= Length[actualTurnsData], i++,
          lastTurnData = actualTurnsData[[i]];
          For[j = 1, j <= n, j++,
            Module[{colorInGuess, feedbackForPeg},
              colorInGuess = lastTurnData[[j, 1]];
              feedbackForPeg = lastTurnData[[j, 2]];
              If[feedbackForPeg === feedbackParziale && colorInGuess == targetHintColor,
                (* Trovato il colore candidato con feedback parziale. Ottieni la sua prima posizione nella soluzione. *)
                firstOccurrencePositionInSolution = Position[soluzioneList, targetHintColor][[1, 1]];
                Throw[{targetHintColor, firstOccurrencePositionInSolution}]
              ];
            ];
          ];
        ];
      ] 
    ]; 
    
    (* --- Priorit\[AGrave] 4: Indizio di Fallback --- *)
    (* Se nessun altro indizio specifico \[EGrave] stato generato, fornisce un indizio generico. *)
    (* 'Green' \[EGrave] un placeholder; potrebbe essere un colore non usato o un simbolo specifico. *)
    Throw[{Green , Missing["NoSimpleHintAvailable"]}]
    
  ] (* Fine Module *)
] (* Fine Catch *)


(* === Codice usato per il bottone di avvio nel notebook ===
Button["Avvia Programma", FrontEndExecute[FrontEndToken[InputNotebook[], "EvaluateNotebook"]],
 BaseStyle -> {"GenericButton", 16, Bold}, ImageSize -> {175, 50}] *)


End[];
EndPackage[];
