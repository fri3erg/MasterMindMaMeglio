(* ::Package:: *)

(* :Title:Trivia Mastermind*)
(* :Context:PacchettoTriviaMastermind`*)
(* :Author:Gruppo 10 - I Ludopatici*)
(* :Summary:Package per "Trivia Mastermind", progetto di MC Unibo anno 24/25*)
(* :Package Version:1.1*)
(* :History:last modified 16/5/2025*)
(* :Copyright:\[Copyright] 2025 Gruppo 10 - Alessandro Modelli, Angelo Greco, Elia Friberg, Francesca Mazzetti, Gianpiero Tovo, Matteo Raggi*)
(* :License:MIT License*)

BeginPackage["PacchettoTriviaMastermind`"];
(*ClearAll["PacchettoTriviaMastermind`*"];*)

(* USAGE DI FUNZIONI CHIAMATE ESPLICITAMENTE NEL NOTEBOOK *)
avviaSchermataDiGioco::usage="Avvia l\[CloseCurlyQuote]interfaccia grafica principale, visualizzando una schermata iniziale da cui \[EGrave] possibile personalizzare i parametri del gioco e avviare una nuova partita.";


Begin["`Private`"];

(* Variabili Globali *)

(*
  Spiegazione del funzionamento per 'triviaData':
  Questa definizione impiega una tecnica nota come "caricamento differito" (o "lazy loading") con "memoizzazione".
  L'operatore ':=' fa s\[IGrave] che l'operazione specificata (in questo caso, CaricaTriviaDaCSV)
  non venga eseguita immediatamente, ma solo la prima volta che si fa effettivamente uso di 'triviaData'.

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
  testato aggiungendo un Print alla funzione di CaricaTriviaDaCSV
  Calcolando triviaData := LoadQuestions... nello stesso modo porta lo stesso problema, entrambi i casi portano a 
  perdite di prestazione, anche con cache automatiche per il risultato

  erano presenti un paio di cose extra che pensavo fossero necessario per le variabili globali fatte cos\[IGrave] 
  ma aveva ragione che potevano essere tolte.
*)
triviaData := triviaData = CaricaTriviaDaCSV["trivia.csv"];


(* Lista dei colori usati da Mastermind *)
(* In particolare, \[EGrave] stato utilizzato RGBColor al posto di Red per ottenere una tonalit\[AGrave] di rosso pi\[UGrave] scura *)
paletteColori={RGBColor[0.9,0,0], Green, Yellow, Blue, Orange, Brown, Purple, Cyan, Magenta, White, Gray, Black};


(* Stato della partita *)
partitaInCorso=True;
screenWidth = 0;
screenHeight = 0;
titleFontScale = 0;

seedInserito = "";
customSeed =.;
customTurni = 8;
customLunghezzaCodice = 4;
allowDuplicates = True;
currentScreen = "menu";


(* Libreria di etichette *)
labels=translations = <|
	"titoloGioco"->"TRIVIA MASTERMIND",
	"fattoDa"->"by Alessandro Modelli, Angelo Greco, Elia Friberg, Francesca Mazzetti, Gianpiero Tovo, Matteo Raggi",
	"inserisciSeed"->"Insert a seed: ",
	"placeholderSeed"->"Write a positive integer...",
	"play"->"\[FilledRightTriangle]",
	"randomSeed"->"\:21bb",
	"nTurni"->"Turns",
	"nCombinazione"->"Code length",
	"allowDuplicates"->"Allow duplicate colors",
	"esci"->"QUIT",
	"seedSelezionato"->"GAME STARTED WITH SEED: ",
	"restartButton"->"Play with the same code",
	"closeDialog"->"Close dialog",
	"vai"->"CHECK",
	"menu"->"\[LongLeftArrow]"
|>;


    (* Calcola una dimensione proporzionale alla risoluzione dello schermo, ottenuta tramite i valori di width e height.
    Il valore restituito \[EGrave] usato per scalare i titoli della schermata inziale.  
	La funzione non viene richiamata nella schermata successiva perch\[EGrave] l\[IGrave] le dimensioni degli elementi grafici sono fisse.
	Tuttavia, il risultato visivo finale anche in questa schermata rimane ben proporzionato.
	La risoluzione dello schermo viene ottenuta tramite il FrontEnd.
	In caso di errore, usa un valore di default (1920x1080) *)
	aggiornaDimensioniSchermo[] := Module[{w, h, scale},
	  Quiet@Check[
	    {w, h} = FrontEndExecute @ FrontEnd`Value[FE`getScreenSize[]],
	    {w, h} = {1920, 1080}
	  ];
	  scale = Min[w, h]/15;
	  {w, h, scale}
	];


(* === Menu di avvio ===
Questa funzione attiva l'interfaccia grafica principale del gioco Trivia Mastermind.
Utilizza una DynamicModule che consente una gestione dinamica e interattiva della finestra di gioco.
La schermata iniziale (menu)consente all'utente di:
- Inserire manualmente o generare un seed casuale per inizializzare la partita,
- Configurare i parametri iniziali di gioco (come duplicati, lunghezza del codice, nuumero di turni),
- Avviare la partita o uscire dal gioco.
Restituisce l'oggetto mainWindow, ovvero la finestra interattiva che contiene l'interfaccia utente del gioco.
Il contenuto di mainWindow \[EGrave] controllato dalla variabile content, che si aggiorna in base al valore della variabile currentScreen.
Le schermate visualizzabili sono:
- menu, che mostra il menu principale,
- gioco, per l'interfaccia di gioco.
Il passaggio da una schermata all'altra \[EGrave] gestita dalla funzione cambiaSchermata, che, oltre a modificare
currentScreen, ricalcola automaticamente la risoluzione dello schermo (altezza e larghezza) per adattare proporzionalmente
la dimensione del titolo, garantendo una visualizzazione ottimale per qualsiasi dispositivo.
La schermata iniziale \[EGrave] generata dalla funzione creaHomepage, definita all'interno di avviaSchermataDiGioco.
mentre la scgermata di giocoAl contrario, la funzione creaSchermataGioco, che gestisce l'interfaccia di gioco vera e propria, \[EGrave] definita
separatamente, vicino alla funzione interfacciaGriglia, poich\[EGrave] quest'ultima si occupa della logica e della visualizzazione
degli elementi interattivi della partita *)
avviaSchermataDiGioco[] := DynamicModule[
 {mainWindow, content},
    
    
    
    (* All'inizio del gioco, quando la schermata viene aperta per la prima volta, vengono
    calcolate le dimensioni di altezza e larghezza necessarie per visualizzare correttamente i titoli *)
    {screenWidth, screenHeight, titleFontScale} = aggiornaDimensioniSchermo[];

	(* La variabile Content serve come contenitore dinamico per l'interfaccia utente.
	In altre parole, \[EGrave] l'elemento che contiene il contenuto visivo che viene visualizzato nella finestra,
	e questo contenuto cambia in modo dinamico a seconda dello stato del gioco.
	Content \[EGrave] visualizzato all'interno dell finestra principale mainWindow *)
	content=Pane[
	    Dynamic @ Refresh[
	        (* Viene valutato il valore di currentScreen per selezionare quale schermata mostrare *)
            Switch[currentScreen,
                "menu", creaHomepage[],      (* Se currentScreen \[EGrave] "menu", chiama creaHomePage per la schermata inziale *)
                "gioco", creaSchermataGioco[ (* Se currentScreen \[EGrave] "gioco", chiama creaSchermataGioco con i parametri del gioco *)
                customSeed, customTurni, customLunghezzaCodice, allowDuplicates,(currentScreen = #)&]
            ],
            TrackedSymbols:>{currentScreen, customTurni, customLunghezzaCodice} (* Vengono monitorate le variabili che influenzano l'interfaccia dimamica *)
        ],
        (* Propriet\[AGrave] relative al posizionamento del content all'interno della finestra visualizzata *)
        Full, 
        Alignment->{Center, Top}
    ];
  
      
    (* MainWindow rappresenta la finestra principale di Trivia Mastermind.
    Contiene e visualizza dinamicamente l'interfaccia utente del gioco, passando tra la schermata
    del menu iniziale e quella di gioco, in base allo stato corrente.
    La finestra si chiude premendo il tasto QUIT *)
	mainWindow = CreateDocument[
	    {
	        Cell[
	            BoxData @ ToBoxes @ content,
	            "Output",
	            ShowCellBracket -> False,
	            CellMargins -> {{0, 0}, {0, 0}}
	        ]
	    },
	    WindowSize -> Automatic,
	    WindowFrame -> "ModelessDialog", (* Cornice standard di Windows *)
	    WindowElements -> {},     (* Rimuove elementi dell'interfaccia di Mathematica *)
	    WindowTitle -> "Trivia Mastermind", (* Titolo della finestra *)
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


     (* Funzione per creare la homepage del gioco.
    E' la schermata che si apre all'inizio della partita e permette all'utente di impostare le sue 
    preferenze di gioco prima di visualizzare la griglia di Trivia Mastermind *)

 creaHomepage[] := Column[{
        
        Spacer[{0, 50}],
        
        (* Titolo del gioco con effetti di colore casuali per ogni carattere.
        L'intento \[EGrave] di aggiungere un tocco giocoso e visivamente gradevole alla schermata, che ha uno sfondo bianco. 
        Il colore dei caratteri cambia ad ogni riapertura della homepage, anche senza dover uscire dal gioco *)
        With[
	        {stringa=labels["titoloGioco"]},  (* Prende il titolo del gioco dalle etichette labels *)
	        Style[
		        Row @ Table[
			        With[
			        {
			            char=StringTake[stringa, {i}], (* Estrae ogni singolo carattere dalla stringa del titolo *)
			            color=RandomColor[] (* Memorizza nella variabile un colore casuale *)
			        },
			            Style[char, FontColor->color] (* Applica il colore casuale al carattere *)
			        ],
			        {i, StringLength[stringa]} (* Itera su ogni carattere della stringa del titolo *)
			    ],
			(* Propriet\[AGrave] relative al titolo del gioco *)
			FontSize->titleFontScale, (* La dimensione del testo \[EGrave] definita dalla variabile titleFontScale prima calcolata *)
			FontWeight->Bold,
		    FontFamily->"Consolas",
		    TextAlignment->Center (* Il testo \[EGrave] allineato al centro *)
	        ]
        ],
        
	    Spacer[{0, 20}],
	    
	    (* Sono indicate le persone che hanno contribuito al progetto. 
	    Il testo \[EGrave] suddiviso in tre elementi, allineati sulla stessa riga, perch\[EGrave] il carattere speciale
	    a forma di cuore \[EGrave] stato colorato in rosso, tra caratteri precedenti e successivi invece di colore grigio *)    
	    Dynamic @ Row[
	    {
		    Style["Made with ", FontSize->titleFontScale/5, FontFamily->"Consolas", FontColor->Gray],
		    Style["\:2665 ", FontSize->titleFontScale/5, FontFamily->"Consolas", FontColor->Red],
		    Style[labels["fattoDa"], FontSize->titleFontScale/5, FontFamily->"Consolas", FontColor->Gray]
	    },
	    Alignment->Center
	    ],
	        
	    Spacer[{0, 50}],
	    
	    (* Etichetta che indica la necessit\[AGrave] da parte dell'utente di dover inserire
	    un seed per iniziare una nuova partita *)
	    Style[labels["inserisciSeed"], FontSize->18, FontFamily->"Consolas"],
		
		Spacer[{0, 25}],
		
		(* Riga composta da tre elementi principali, allineati orizzontalmente.
		Vi \[EGrave] il pulsante per la generazione del seed, un campo di input numerico dove 
		l'utente pu\[OGrave] inserire manualmente un seed personalizzato, oppure visualizzare il
		seed appena generato casualmente. Infine, il pulsante play a fine riga, se cliccato,
		permette di iniziare una nuova partita *)
		Row[{
		    (* Pulsante che se premuto genera un seed casuale tra 0 e 9999 *)
			ClickPane[
				Framed[
				(* Stile del tasto *)
				Style[labels["randomSeed"], FontSize->18, FontColor->White],
				Background->Darker[Blue], 
				FrameStyle->None, 
				RoundingRadius->5,
				FrameMargins->{{10, 10}, {5, 5}}, 
				ImageSize->Automatic 
				],
			    Function[ (* Azione al click: viene generato e assegnato il seed *)
			        seedInserito=RandomInteger[{0, 9999}]; 
			    ]
			],
			    
		    Spacer[15],
	        
	        (* Campo di input per inserire o visualizzare il seed *)
			Item[
				Framed[
					InputField[
						Dynamic[
							seedInserito,
							(* Setter personalizzato: accetta solo interi >= 0 *)
							(If[IntegerQ[#] && # >= 0, seedInserito = #] &)
							
							(* Alternativa: permette la presenza di stringhe vuote, ma il punto decimale resetta il campo del seed 
							Function[val,
								If[IntegerQ[val] && val >= 0,
									seedInserito = val,  (* Se \[EGrave] un intero positivo, mantienilo *)
									seedInserito = Missing["NotAvailable"]  (* Se viene cancellato tutto o valore invalido -> reset *)
								]
							]*)
						],
						Number, (* Per propriet\[AGrave] di Number, la stringa vuota non \[EGrave] ammessa, ergo una vota iniziato a scrivere il seed, deve rimanere almeno una cifra (si pu\[OGrave] comunque cambiare selezionandola) *)
						FieldHint -> labels["placeholderSeed"], FieldHintStyle -> {Italic},
						ImageSize -> {250, 21}, Appearance -> "Frameless",
						BaselinePosition -> Center, ContinuousAction -> True (* Azione continua *)
					],
					Background -> LightGray,
					FrameStyle -> None,
					RoundingRadius -> 10,
					FrameMargins -> {{10, 10}, {5, 5}},
					ImageSize -> Automatic
				],
				ItemSize -> Automatic
			],
	       
		    Spacer[15],
		    
		    (* Pulsante play, attivo solo se il seed inserito \[EGrave] valido (numero intero positivo).
		    L'aspetto del tasto varia visivamente in base al suo stato: se il seed non \[EGrave] valido, il 
		    tasto appare disattivato e non \[EGrave] cliccabile; se invece il seed \[EGrave] corretto, il tasto si
		    colora, consentendo l'avvio della partita *)
		    Dynamic[
			    If[IntegerQ[seedInserito] && seedInserito >= 0,
			    (* Caso seed valido -> tasto abilitato *)
				    ClickPane[
					    Framed[
						    Style[labels["play"], FontSize->18, FontColor->White],
						    Background->RGBColor[0, 0.5, 0],
						    FrameStyle->None,
						    RoundingRadius->5,
						    FrameMargins->{{10, 10}, {5, 5}},
						    ImageSize->Automatic
					    ],
					    Function[
						    partitaInCorso=True;
						    (* Il seed memorizzato su seedInserito, generato causalmente o inserito manualmente,
						    viene salvato su customSeed per essere passato alle funzioni di apertura del gioco *)
						    customSeed=seedInserito; 
						    seedInserito="";
						    currentScreen = "gioco";
						    aggiornaDimensioniSchermo[]
					    ]
				    ],
			        
				    (* Caso seed non valido -> tasto disabilitato  *)
				    Framed[
					    Style[labels["play"], FontSize->18, FontColor->GrayLevel[0.8]],
					    Background->RGBColor[0,0.3,0], (* Sfondo verde, ma scurito per indicare la disattivazione *)
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
	    
	    (* Sezione dedicata alla personalizzazione della partita, dove l'utente pu\[OGrave] scegliere
	    le impostazioni inziali prima di avviare un nuovo gioco.
	    Al primo avvio, i valori predefiniti corrispondono alle variabili locali della funzione
	    avviaSchermataDiGioco: customTurni, customLunghezzaCodice, allowDuplicates.
	    La opzioni proposte riflettono le configurazioni classiche del gioco Mastermind originale *)
	    Column[{
		    Row[{
		        (* Viene scelto il numero di tentativi a disposizione per indovinare la combinazione segreta.
		        Un valore pi\[UGrave] alto rende il gioco pi\[UGrave] semplice, offrendo maggiori possibilit\[AGrave] di vittoria *)
			    Style[labels["nTurni"], FontSize->14, FontFamily->"Consolas", Bold],
			        
			    Spacer[110],
			   
				SetterBar[
					Dynamic[customTurni],
					Table[
						i->Style[ToString[i], FontFamily->"Consolas", Bold],
						{i, 6, 12} (* E' possibile scegliere tra un minimo di 6 e un massimo di 12 turni *)
					],
					Appearance->"Horizontal"
				]  
		    }],
	        
		    Row[{
		        (* L'utente indica la lunghezza della combinazinoe segreta *)
			    Style[labels["nCombinazione"], FontSize->14, FontFamily->"Consolas", Bold],
		                
		        Spacer[114],
		                
			    SetterBar[
				    Dynamic[customLunghezzaCodice],
				    Table[
					    j->Style[ToString[j], FontFamily->"Consolas", Bold],
					    {j, 3, 7} (* Lunghezza della combinazione segreta selezionabile tra 3 a 7 *)
				    ],
				    Appearance->"Horizontal"
			    ]
		    }],
		    
		    Row[{
		        (* Questa parte gestisce la ripetizione di colori nella combinazione che si andr\[AGrave] a creare *)
			    Style[labels["allowDuplicates"], FontSize->14, FontFamily->"Consolas", Bold],
			                
			    Spacer[93],
			                
			    Checkbox[Dynamic[allowDuplicates]] (* Se la casella \[EGrave] deselezionata, la combinazione segreta non presenter\[AGrave] ripetizioni di colori *)
		    }]
	    }],
	
	    Spacer[{0, 50}],
	    
	    (* Individua il tasto per uscire dal gioco, riportando l'utente al notebook *)
	    ClickPane[
			Framed[
			    (* Stile del pulsante QUIT *)
				Style[labels["esci"], White, FontFamily->"Consolas", FontSize->24, Bold],
				Background->Red,
				FrameStyle->None,
				RoundingRadius->10,
				FrameMargins->{{15, 15}, {5, 5}},
				ImageSize->Automatic
			],  
			Function[
				seedInserito=Null;
				NotebookClose[EvaluationNotebook[]] (* Chiude la finestra del gioco *)
			]
		]
	},
	Alignment->Center
    ]


(* === Funzione per la creazione della schermata di gioco ===
Costruisce l'interfaccia utente dinamica per il gioco Trivia Mastermind.
Questa schermata viene visualizzata quando l'utente, dalla schermata iniziale, avvia una nuova
partita cliccando sul pulsante verde con il seed selezionato o inserito manualmente.
L'interfaccia mostra:
- Un pulsante per tornare alla schermata principale,
- Il seed scelto per la partita corrente (che consente di ricreare la stessa partita se usato con le stesse impostazioni,
- La griglia di gioco, che si aggiorna dinamicamente in base alle azioni dell'utente e alle impostazioni selezionate.
Parametri:
- seed: seme utilizzato per generare la combinazione segreta e altri aspetti (come le domande del quiz Trivia presenti come suggerimenti),
- tentativi: rappresenta il numero di tentativi massimo disponibile,
- combinazione: la combinazione segreta da indovinare,
- allowDuplicates: valore booleano che indica se nella combinazione sono ammessi colori ripetuti *)
creaSchermataGioco[seed_, tentativi_, combinazione_, allowDuplicates_, setScreen_] := DynamicModule[
	{},
	
	Pane[
		Column[{
		    (* Barra superiore *)
			Panel[
				Row[{
				    (* Pulsante per tornare la menu principale *)
					ClickPane[
						Framed[
						    (* Stile del bottone *)
							Style[labels["menu"], White, FontSize->20, FontFamily->"Consolas", Bold],
							Background->Red,
							FrameStyle->None,
							RoundingRadius->5,
							FrameMargins->{{6, 6}, {0, 0}}
						],
						Function[
						setScreen["menu"];
						aggiornaDimensioniSchermo[] 
					]
					],
					
					Spacer[5],
					
					(* Visualizzazione del seed selezionato nella partita corrente *)
					Framed[
						Style[labels["seedSelezionato"] <> ToString[seed], FontSize->20, 
						FontFamily->"Consolas", FontColor->Black, Bold],
						FrameStyle->None,
						FrameMargins->{{6, 6}, {0, 0}}
					]
				},
				Alignment->Center (* Allineamento degli elementi al centro della schermata *)
				],
			Background->White 
			],
			
			(* Area di gioco dinamica *)
			Dynamic[
				Pane[
				    (* InterfacciaGriglia gestisce la griglia di gioco ed \[EGrave] inclusa nella schermata creata da
				    creaSchermataGioco, sotto al testo che mostra il seed attuale.
				    Qui l'utente effettua i tentativi per cercare di vincere la partita *)
					interfacciaGriglia[seed, combinazione, tentativi, allowDuplicates, setScreen], 
					{Automatic, Scaled[0.8]}, (* Griglia centrata e ridimensionata per una visualizzazione ottimale senza scorrimento o zoom *)
					Scrollbars->False,
					Alignment->Center 
				]
			]
		},
		Alignment->Center
		],
	Alignment->Center,
	ImageSize->Scaled[1] (* Il contenuto occupa l'intera area disponibile della finestra *)
	]
];



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


(* Funzione di generazione dell'interfaccia della griglia di gioco dinamica, prende in input il seed,
il numero di colori che formano la combinazione segreta, il numero di tentativi e un flag per l'utilizzo di colori duplicati.
Genera: 
 -Griglia di tentativi e relative combinazioni
 -Griglia di feedback per ciascun tentativo
 -Bottone di check del tentativo
 -Bottone di Hint per l'avvio del Trivia
 -Funzioni di restart e termina partita
*)
interfacciaGriglia[seed_, lunghezzaCombinazione_, numeroTentativi_, allowDuplicates_, setScreen_] := DynamicModule[
{
    (* Definisce una griglia di colori per ciascun disco in ogni riga della partita.
    Inizialmente tutti i dischi sono vuori; viene aggiornata quando l'utente seleziona un colore *)
	gridItemsColors = Table[Opacity[0.2, Black], {numeroTentativi}, {lunghezzaCombinazione}], 
	(* Memorizza lo storico dei feedback per ciascun tentativo. 
	Ogni elemento \[EGrave] una lista di coppie {coloreScleto, feedback}.
	Serve per vedere i pioli dei feedback *)
	hintFeedbackHistory = ConstantArray[{}, numeroTentativi],
	(* Numero del tentativo attuale, ovvero quale riga l'utente sta riempendo *)
	turn = 1, 
	(* Lista dei colori disponibili per la selezione dei dischi, definita come variabile globale *)
	colorsList = paletteColori, 
	(* Coordinate dell'elemento selezionato nella griglia (riga, colonna).
	Serve per gestire l'interazione coi dischi *)
	selectedItem = {1, 1}, 
	(* La combinazione segreta da indovinare, generata una volta all'inizio.
	In base a seed, pu\[OGrave] essere rigenerata identica.
	La funzione generaCodiceSegreto, che definisce la combinazione, \[EGrave] definita fuori da interfacciaGriglia *)
	soluzioneList = generaCodiceSegreto[seed, lunghezzaCombinazione, allowDuplicates], 
	(* Lista che rappresenta il tentativo attuale dell'utente. 
	Ogni elemento \[EGrave] None fino a che non viene scelto un colore *)
	tentativoList = ConstantArray[None, lunghezzaCombinazione],
	(* Memorizza il risultato dell'ultimo tentativo valutato. Tipicamente \[EGrave] una lista: 
	- esito: mastermindProsegui/mastermindVittoria/mastermindSconfitta,
	- feedbackList: simboli associati al tentativo *)
	valutazioneTentativo = {},
	(* Tiene conto di quante domande Trivia sono state poste.
	Aumenta ogni volta che l'utente clicca sul pulsante hint *)
	questionCounter = 0,
	(* Tiene traccia degli aiuti trivia usati. Ogni voce pu\[OGrave] essere:
	- {colore, infromazione}: se la domanda \[EGrave] stata risposta correttamente,
	- {}: se il giocare non ha richiesto un aiuto,
	- elemento Missing: se l'utente ha sbagliato *)
	correct = {}
},

	Framed[
        Column[{
            (* Contenuto della griglia *)
            Row[{
                Spacer[20],

                (* Il blocco crea una griglia interattiva di colori, che rappresenta i possibili
                colori utilizzati per formare una combinazione segreta.
                Ogni colore \[EGrave] rappresentato da un disco colorato, organizzato in una girglia a due colonne.
                L'utente pu\[OGrave] cliccare su uno di questi dischi per selezionarlo: il colore scelto verr\[AGrave]
                applicato alla posizione impostata sulla griglia di gioco, a condizione che la partita 
                sia ancora in corso *)
                Grid[
                    Partition[
                        Table[
                            (* ColorsCol \[EGrave] la lista dei colori disponibili. Si definisce la variabile
                            locale col che cattura il valore corrente per permette una sicura interazione
                            con l'utente *)
                            With[{col = colorsCol}, 
	                            EventHandler[ 
					                Dynamic @ Graphics[ (* Creazione dei dischi colorati*)
					                {
						                EdgeForm[Black],
						                (* Colora il disco utilizzando il colore attualmente assegnato a col *)
						                FaceForm[col], 
						                Disk[{0, 0}, 1]
					                },
					                    ImageSize->35 (* Dimensione del disco *)
					                ],
				                    {
						                "MouseClicked" :> ( 
						                    (* Il click \[EGrave] controllato da partitaInCorso, ovvero il pulsante \[EGrave] abilitato solo se la
						                    partita \[EGrave] ancora in corso. Questo permette di evitare modifiche dopo la fine del gioco.
						                    In questo modo, i tentativi effettuati e i colori selezionati rimangono visibili e invariati a fine partita. *)
							                If[partitaInCorso,                            
								                gridItemsColors[[Sequence @@ selectedItem]] = col; (* Aggiorna il colore visualizzato nella griglia di gioco *)
								                tentativoList[[selectedItem[[2]]]] = col; (* Tiene traccia delle scelte dell\[CloseCurlyQuote]utente nel tentativo corrente *)
								                
								                (* Dopo aver assegnato il colore selezionato, viene evidenziata
								                automaticamente la selezione sul prossimo "pallino grigio"
								                disponibile. Se tutti i pallini sono gi\[AGrave] stati colorati, la
								                selezione resta sulla posizione corrente *)	                
								                selectedItem = vaiAlProssimoPallinoVuoto[selectedItem, tentativoList, lunghezzaCombinazione];                                 
							                ]
						                )
					                }
				                ]
                            ],
                        (* Ogni elemento colorsCol prende i valori dalla lista colorsList,
                        in cui era stata memorizzara paletteColori *)
                        {colorsCol, colorsList} 
                        ],
                    2 (* visualizzati su due colonne, per rendere la palette pi\[UGrave] compatta *)
                    ],
                Spacings -> {1, 1},
                Alignment -> Center
                ],

                Spacer[80],

                (* Il contenuto di questo blocco \[EGrave] racchiuso nel riquadro di
                colore grigio, che contiene gli elementi per giocare la partita *)
                Grid[
                    Table[
                        With[{x=row},
                            Append[
                                (* Questo blocco crea una riga di dischi interattivi, che rappresentanto i 
                                tentativi del giocare all'interno della griglia del gioco Trivia Mastermind.
                                Ogni disco pu\[OGrave] essere colorato selezionando un colore dalla palette definita 
                                nel blocco precedente. La colorazione avviene dinamicamente al click su un colore,
                                e viene visualizzata in tempo reale sulla griglia.
                                Viene gestita:
                                - La selezione del disco corrente da colorare,
                                - La deselezione cliccando di nuovo su un disco gi\[AGrave] colorato,
                                - La visualizzazione dinamica della riga corrispondente al turno attivo.
                                Il numero di dischi per riga corrisponde alla lunghezza della combinazione segreta,
                                e il numero totale di righe \[EGrave] pari al numero massimo di tentativi scelto inizialmente *)
	                            Table[
	                                (* Viene calcolato un id univoco per ogni cella *)
		                            With[{y=col, id=lunghezzaCombinazione*(row-1)+col},
			                            EventHandler[ (* Gestisce gli elementi mouse per ogni cerchio *)
				                            Dynamic @ Graphics[
				                            {
					                            EdgeForm[ (* Bordo evidenziato se il pallino \[EGrave] quello selezionato *)
						                            If[{x, y} === selectedItem,
						                            Directive[Black, AbsoluteThickness[1]], (* Bordo sottile nero se l'elemento \[EGrave] selezionato *)
						                            None]
					                            ],
					                            (* Vengono colorate le righe fino al turno corrente. A livello visivo appaiono "abilitate",
					                            nonostante la colorazione di riempimento grigia e il contorno lievemente pi\[UGrave] scuro.
					                            Si riconosce la differenza rispetto alle righe non ancora accedibili *)
					                            If[x <= turn, gridItemsColors[[x, y]], Opacity[0.1, Black]],
					                            Disk[{0, 0}, 1]
				                            },
				                                ImageSize->{35, 35} (* Dimensione dei cerchi *)
				                            ],
				                            {
					                            "MouseClicked" :> ( 
					                                (* Consente l'interazione solo durante la partita e solo sulla riga corrente.
					                                Controlla la selezione dei pallini: se il pallino \[EGrave] gi\[AGrave] stato colorato, permette la 
					                                decolorazione per una nuova scelta da parte dell'utente *)
						                            If[partitaInCorso && x === turn,
							                            If[tentativoList[[y]] =!= None, 
								                            selectedItem={x, y};
								                            tentativoList[[y]]=None;
								                            gridItemsColors[[x, y]]=Opacity[0.2, Black]; (* Si riporta il pallino allo stato iniziale, di colore grigio *)
								                            ,
								                            (* Se invece \[EGrave] vuoto, viene semplicemente selezionato *)
								                            selectedItem={x, y};
							                            ]
						                            ]
					                            )
				                            }
			                            ]
		                            ],
		                        (* Viene iterata la variabile col (indice corrente del
		                        ciclo, in questo caso la colonna della griglia) da 1 a
		                        lunghezzaCombinazione, con un passo di 1 *)
	                            {col, 1, lunghezzaCombinazione} 
	                            ],
								
								(* La griglia di feedback e i tasti di azione sono posizionati sulla stessa riga *)
	                            Row[{
                                    Spacer[20],
                                    
                                    (* Viene generata una griglia di feedback colorati, usata nel gioco originale
                                    di Mastermind per indicare quanto \[EGrave] corretto un tentattivo rispetto alla
                                    combinazione segreta. Si avranno tanti pallini di feedback quanto \[EGrave] lunga la
                                    combinazione da indovinare per la partita iniziata. 
                                    Si hanno due tipi di feedback: 
                                     *)
                                    Dynamic @ Module[ (* Si usa una Dynamic per consentire un aggiornamento automatico dell'interfaccia *)
                                    {
	                                    feedbackSymbolsForDisplay,
	                                    feedbackColors
                                    },
                                        (* Recupera i simboli di feedback per la riga x:
                                        - Se esistono dati, (!= {}), viene estratto il secondo elemento di 
                                        ciascuna coppia nella lista (feedbackEsatto o feedbackParziale)
                                        - Altrimenti (ad esempio per un turno non ancora giocato)), genera
                                        un array con solo feedbackAssente, cio\[EGrave] simboli vuoti
                                        (nessun feedback ancora disponibile) *)
	                                    feedbackSymbolsForDisplay = If[hintFeedbackHistory[[x]] =!= {},
		                                    hintFeedbackHistory[[x]][[All, 2]], 
		                                    ConstantArray[feedbackAssente, lunghezzaCombinazione] 
	                                    ];
										
									   (* Sostituisce i simboli di feedback con colori:
									   - feedbackEsatto: verde chiaro
									   - feedbackParziale: giallo
									   - feedbackAssente: nessun colore (il piolo rimane trasparente) *)
	                                    feedbackColors = feedbackSymbolsForDisplay /. {
		                                    feedbackEsatto -> RGBColor[0.57, 1, 0.05],
		                                    feedbackParziale -> RGBColor[1, 0.85, 0],
		                                    feedbackAssente -> None 
	                                    };
	                                    
	                                    (* Costruisce una griglia centrata contenente i pioli di feedback: 
	                                    ogni disco \[EGrave] disegnato con un bordo griglio e un riempimento determinato 
	                                    da feedbackColors, definito sopra, in base alla valutazione del tentativo *)
	                                    Style[
		                                    Grid[{
			                                    Table[
				                                    Graphics[{EdgeForm[Gray], FaceForm[hint], Disk[{0, 0}, 1]}, ImageSize->15],
				                                    {hint, feedbackColors}
			                                    ]
		                                    },
		                                    Alignment->Center
		                                    ],
		                                (* Non \[EGrave] possibile la selezione o la modifica dell'interfaccia da parte dell'utente *)
		                                Selectable->False,
		                                Editable->False
	                                    ]
                                    ],

                                    Spacer[50],
                                    
                                    (* Definito il comportamento dinamico del pulsante di check, cio\[EGrave]
                                    il bottone per confermare il tentativo corrente nel gioco. Il bottone 
                                    appare e si comporta diversamente a seconda di:
                                    - Se \[EGrave] il turno attuale,
                                    - Se il tentativo \[EGrave] stato completato (tutti i dischi hanno un colore selezionato),
                                    - Se la partit\[AGrave] \[EGrave] ancora in corso.
                                     *)
                                    Dynamic[Module[
                                        {tentativoCompletoQ},
                                        
                                        (* Verifica che tutti gli slot dei tentativi siano pieni, cio\[EGrave] 
                                        che l'utente abbia scleto tutti i colori per la combinazione *)
                                        tentativoCompletoQ = AllTrue[tentativoList, # =!= None &];  (* Il tentativo \[EGrave] 'completo' se non ha None *)
                                        
                                        (* Vi \[EGrave] la scelta dell'interfaccia con Which: a seconda del 
                                        valore di x (la riga corrente della griglia) e dello stato del
                                        tentativo, si individuano 3 diversi casi *)
	                                    Which[
		                                    (* Caso 1: turno corrente e tentativo completo *)
		                                    x === turn && tentativoCompletoQ,
		                                    ClickPane[
			                                    Framed[
				                                    Grid[{{
					                                    Style["\|01f3ae", FontSize->10],
					                                    Style[labels["vai"], White, FontFamily->"Consolas", FontSize->12, Bold]
				                                    }}],
				                                Background->Orange, (* Viene mostrato un tasto arancione con scritto check *)
				                                FrameStyle->None,
				                                RoundingRadius->10,
				                                FrameMargins->{{10, 10}, {10, 10}},
				                                ImageSize->Automatic
			                                    ],
			                                    
			                                    Function[
				                                    If[partitaInCorso,
					                                    (* Prima di processare il tentativo, assicuriamoci che esista una entry per l'eventuale aiuto in questa riga. *)
					                                    (* Questo serve per mantenere l'allineamento della griglia degli aiuti, anche se l'aiuto non viene usato. *)
					                                    If[Length[correct] < x,
					                                        (* Se la lunghezza della lista correct \[EGrave] minore di x, viene aggiunto un elemento vuoto *)
					                                        AppendTo[correct, {}];
					                                    ];
					                                    (* Viene valutato il tentativo attuale *)
					                                    valutazioneTentativo=valutaTentativo[soluzioneList, tentativoList, numeroTentativi, turn];
					                                        (* Il risultato della valutazione (feedback) viene salvato nella variabile hintFeedbackHistory *)
					                                    hintFeedbackHistory[[turn]]=Table[{tentativoList[[i]], valutazioneTentativo[[2]][[i]]}, {i, Length[tentativoList]}];
					                                    
					                                    If[valutazioneTentativo[[1]] === mastermindProsegui, turn++];
					                                    
					                                    (* Se il feedback indica vittoria: si ferma la partita, si mostra un dialog
					                                    di vittoria e tutte le variabili principali vengono resettate per preparare
					                                    una nuova partita *)
					                                    If[Length[valutazioneTentativo] > 0 && valutazioneTentativo[[1]] === mastermindVittoria,
					                                    
						                                    partitaInCorso=False;
						                                    
						                                    MostraDialogFinePartita[True,
							                                    Function[{},
								                                    (* Reset delle variabili *)
								                                    gridItemsColors=Table[Opacity[0.2, Black], {numeroTentativi}, {lunghezzaCombinazione}];
								                                    hintFeedbackHistory=ConstantArray[{}, numeroTentativi];
								                                    turn=1;
								                                    colorsList=paletteColori;
								                                    selectedItem={1, 1};
								                                    soluzioneList=generaCodiceSegreto[seed, lunghezzaCombinazione, allowDuplicates];
								                                    tentativoList=ConstantArray[None, lunghezzaCombinazione];
								                                    valutazioneTentativo={};
								                                    partitaInCorso=True;
								                                    questionCounter=0;
								                                    correct={};
							                                    ],
							                                    setScreen
						                                    ];
					                                    ];
					                                    
                                                        (* Se il feedback indica sconfitta: si ferma la partita, si mostra il dialog
                                                        di sconfitta e, anche qui, tutto viene resettato *)
					                                    If[Length[valutazioneTentativo] > 0 && valutazioneTentativo[[1]] === mastermindSconfitta,
						                                    partitaInCorso=False;
						                                    
						                                    MostraDialogFinePartita[False,
							                                    Function[{},
								                                    (* Reset delle variabili *)
								                                    gridItemsColors=Table[Opacity[0.2, Black], {numeroTentativi}, {lunghezzaCombinazione}];
								                                    hintFeedbackHistory=ConstantArray[{}, numeroTentativi];
								                                    turn=1;
								                                    colorsList=paletteColori;
								                                    selectedItem={1, 1};
								                                    soluzioneList=generaCodiceSegreto[seed, lunghezzaCombinazione, allowDuplicates];
								                                    tentativoList=ConstantArray[None, lunghezzaCombinazione];
								                                    valutazioneTentativo={};
								                                    partitaInCorso=True;
								                                    questionCounter=0;
								                                    correct={};
							                                    ],
							                                    setScreen
						                                    ];
					                                    ];
					                                    
					                                    (* Al termine, il turn viene incrementato (se la partita continua)
					                                    e tentativoList azzerato per la nuova riga *)
					                                    selectedItem={turn, 1};
					                                    tentativoList=ConstantArray[None, lunghezzaCombinazione];
				                                    ]
			                                    ]   
		                                    ],
		                                    
		                                    (* Caso 2: turno corrente ma tentativo incompleto.
		                                    Indica che \[EGrave] il proprio turno, ma non si pu\[OGrave] ancora inviare il tentativo *)
		                                    x === turn && !tentativoCompletoQ,
		                                    Framed[
			                                    Grid[{{
			                                        Style["\|01f3ae", FontSize->10, FontColor->Gray],
			                                        Style[labels["vai"], FontFamily->"Consolas", FontSize->12, FontColor->Gray, Bold]
			                                    }}],
			                                Background->GrayLevel[0.8],  (* Mostra un pulsante grigio disattivato *)
			                                FrameStyle->None,
			                                RoundingRadius->10,
			                                FrameMargins->{{10, 10}, {10, 10}},
			                                ImageSize->Automatic
		                                    ],
		                                    
		                                    (* Caso 3 (default): non \[EGrave] il turno corrente *)
		                                    True,
		                                    Framed[
			                                    Grid[{{
				                                    Style["\|01f3ae", FontSize->10, FontColor->Directive[GrayLevel[0.9], Opacity[0]]],
				                                    Style[labels["vai"], FontFamily->"Consolas", FontSize->12, FontColor->Directive[GrayLevel[0.9], Opacity[0]], Bold]
			                                    }}],  
			                                (* Mostra un pulsante invisibile per mantenere l'allineamento della griglia.
			                                Il pulsante ha stile e testo trasparenti *)
			                                Background->GrayLevel[0.9], 
			                                FrameStyle->None,
			                                RoundingRadius->10,
			                                FrameMargins->{{10, 10}, {10, 10}},
			                                ImageSize->Automatic
		                                    ]
	                                    ] (* fine which*)
                                    ]],   (* fine dynamic[module]*)
                                    
                                    Spacer[50],
                                        
                                    (* emptyResultPlaceholder rappresenta uno stato iniziale o nullo per il 
                                    risultato di un aiuto non ancora disponibile. *)
                                    emptyResultPlaceholder = Missing["NoResultSetYet"];
                                    Dynamic[
									    pulsanteSuggerimenti[
									        correct,         
									        x,               (* La 'x' corrente dalla Table (riga) *)
									        turn,            
									        emptyResultPlaceholder, 
									        triviaData,      
									        partitaInCorso,  
									        Function[        (* Questa \[EGrave] la funzione di callback per il ClickPane *)
									            If[partitaInCorso,
									                AppendTo[correct, MostraDomandeTrivia[seed + questionCounter, DecidiSuggerimentoLogica[hintFeedbackHistory, soluzioneList]]];
									                questionCounter++;
									            ]
									        ]
									    ]
									]
								}] (* Fine Row *)
							] (* Fine Append *)
						], (* Fine With *)
					{row, 1, numeroTentativi}
					] (* Fine Table *)
				] (* Fine Grid*)
            },
            Alignment->{{Left, Center, Center, Right}}
            ], (* Fine Row *)
            
            Spacer[20]
        },
        Alignment->Center
        ], (* Fine Column *)
    Background->GrayLevel[0.9],
    FrameStyle->None,
    RoundingRadius->15,
    FrameMargins->{{15, 15}, {5, 5}},
    ImageSize->Automatic
	] (* Fine Framed *)
]	


pulsanteSuggerimenti[
    correct_List, (* Lista dei risultati dei tentativi di hint *)
    rigaCorrenteX_Integer, (* La riga (x) per cui stiamo generando questo UI *)
    turnoAttuale_Integer, (* Il turno di gioco corrente *)
    placeholderRisultatoVuoto_, (* Il tuo emptyResultPlaceholder *)
    datiTrivia_, (* triviaData *)
    partitaInCorso_, (* Flag booleano *)
    callbackClickHint_Function (* Una funzione per gestire il click del bottone HINT *)
] := Module[
    {
        currentValForRowX, 
        displayOutput
    },

    (* Determina il valore attuale per la riga x *)
    currentValForRowX = If[
        ListQ[correct] && Length[correct] >= rigaCorrenteX && rigaCorrenteX >= 1 &&
        correct[[rigaCorrenteX]] =!= Null && correct[[rigaCorrenteX]] =!= {} && correct[[rigaCorrenteX]] =!= placeholderRisultatoVuoto,
        correct[[rigaCorrenteX]],
        placeholderRisultatoVuoto
    ];

    (* Determina quale interfaccia utente visualizzare*)
    displayOutput = Which[
        currentValForRowX === Missing["WrongAnswer"],
        Framed[
            Style["\:274c", FontSize -> 18],
            Background -> GrayLevel[0.95], FrameStyle -> Red, RoundingRadius -> 10,
            FrameMargins -> {{10, 10}, {0, 0}}, ImageSize -> {80, 35}, Alignment -> Center
        ],

        MatchQ[currentValForRowX, {_?ColorQ, _}],
        With[
            {
                resultColor = First[currentValForRowX],
                resultValue = Last[currentValForRowX]
            },
            Framed[
                If[resultValue =!= Missing["NoSimpleHintAvailable"], 
                    If[resultValue =!= Missing["PositionNotApplicable"], 
                        Row[{Graphics[{EdgeForm[Gray], resultColor, Disk[]}, ImageSize -> {20, 20}], Spacer[5], Column[{Style[ToString[resultValue], 16, Bold, FontFamily -> "Arial"]}, Spacings -> 0]}], 
                        Graphics[{EdgeForm[Gray], resultColor, Disk[]}, ImageSize -> {20, 20}]], 
                    ""], 
                Background -> GrayLevel[0.95], FrameStyle -> Darker[Green], RoundingRadius -> 10, 
                FrameMargins -> {{30, 10}, {6, 6}}, ImageSize -> {80, 35}
            ]
        ],

        True, (* Caso di default: mostra il pulsante HINT o un placeholder *)
        If[rigaCorrenteX === turnoAttuale && datiTrivia =!= $Failed,
            ClickPane[
                Framed[ (* Aspetto del pulsante HINT attivo *)
                    Grid[{{Style["\|01f4a1", FontSize -> 10], Style["HINT", White, FontFamily -> "Consolas", FontSize -> 12, Bold]}}],
                    Background -> Blue, FrameStyle -> None, RoundingRadius -> 10,
                    FrameMargins -> {{10, 10}, {10, 10}}, ImageSize -> {80, 35}
                ],
                callbackClickHint, (* Usa la funzione di callback passata come argomento *)
                Method -> "Queued"
            ],
            Framed[ (* Pulsante HINT inattivo/placeholder *)
                Grid[{{Style["\|01f4a1", FontSize -> 10, FontColor -> Directive[GrayLevel[0.7], Opacity[0]]], Style["HINT", FontFamily -> "Consolas", FontSize -> 12, FontColor -> Directive[GrayLevel[0.7], Opacity[0]], Bold]}}],
                Background -> GrayLevel[0.9], FrameStyle -> None, RoundingRadius -> 10,
                FrameMargins -> {{10, 10}, {10, 10}}, ImageSize -> {80, 35}
            ]
        ]
    ];
    displayOutput
];


(* === Funzione che visualizza un dialogo di fine partita ===
In  aggiunta, si hanno opzioni per riavviare il gioco, tornare al menu e chiudere la finestra di dialogo.
Parametri:
- hasWon: True se il giocatore ha vinto, False altrimenti
- onRestartFunc: funzione opzionale da eseguire se si sceglie Restart *)
MostraDialogFinePartita[hasWon_, onRestartFunc_: Null, setScreen_] :=
    CreateDialog[
        DynamicModule[{}, (* Manteniamo questo DynamicModule per il contesto del Dialog *)
            Pane[
                Column[{
                    (* Titolo *)
                    Style[
                        Pane[If[hasWon, "You won!", "You lost!"], 
                            ImageSize -> {400, Automatic}, Alignment -> Center],
                        36, Bold, FontFamily -> "Arial",
                        If[hasWon, Darker[Green], Red]
                    ],

                    (* Bottone Restart *)
                    Pane[
                        ClickPane[
                            Framed[
                                Style[labels["restartButton"], White, FontFamily -> "Consolas", FontSize -> 18, Bold],
                                Background -> RGBColor[0, 0.5, 0], FrameStyle -> None, RoundingRadius -> 10,
                                FrameMargins -> {{30, 30}, {10, 10}}, ImageSize -> {Automatic, Automatic}
                            ],
                            Function[
                                If[onRestartFunc =!= Null, onRestartFunc[]];
                                NotebookClose[EvaluationNotebook[]];
                            ]
                        ],
                        Alignment -> Center
                    ],

                    (* Bottoni Esci e Chiudi Dialogo *)
                    Pane[
                        Row[{
                            (* Bottone Esci *)
                            ClickPane[
                                Framed[
                                    Style[labels["esci"], White, FontFamily -> "Consolas", FontSize -> 18, Bold],
                                    Background -> Red, FrameStyle -> None, RoundingRadius -> 10,
                                    FrameMargins -> {{15, 15}, {5, 5}}, ImageSize -> {Automatic, Automatic}
                                ],
                                Function[
                                    setScreen["menu"];
                                    NotebookClose[EvaluationNotebook[]];
                                    aggiornaDimensioniSchermo[]
                                ]
                            ],

                            Spacer[10],

                            (* Bottone Chiudi Dialogo *)
                            ClickPane[
                                Framed[
                                    Style[labels["closeDialog"], White, FontFamily -> "Consolas", FontSize -> 18, Bold],
                                    Background -> Gray, FrameStyle -> None, RoundingRadius -> 10,
                                    FrameMargins -> {{15, 15}, {5, 5}}, ImageSize -> {Automatic, Automatic}
                                ],
                                Function[NotebookClose[EvaluationNotebook[]]]
                            ]
                        }],
                        Alignment -> Center
                    ],

                    Spacer[10]
                },
                Spacings -> 5,
                Alignment -> Center
                ],
                ImageSize -> {450, 300},
                Alignment -> {Center, Top}
            ]
            (* Fine del corpo di DynamicModule *)
        ],
        WindowTitle -> If[hasWon, "WIN!", "LOSE!"],
        WindowSize -> {450, 300},
        Modal -> True,
        WindowElements -> {}, (* Utile per controllare esattamente cosa appare *)
        WindowFrame -> "ModalDialog"
    ];


(* Mostra una finestra di dialogo (dialog) con una domanda trivia e opzioni a scelta multipla. *)
(* Il dialogo \[EGrave] modale, quindi la funzione sospende l'esecuzione finch\[EAcute] il dialogo non viene chiuso. *)
(* @param seed: Numero intero usato per selezionare e mescolare la domanda tramite PreparaTriviaData. *)
(* @param hintToGive: La struttura dell'indizio (solitamente {colore, posizione_o_codice_mancante}) che verr\[AGrave] mostrata se la risposta \[EGrave] corretta. Questa stessa struttura sar\[AGrave] il valore restituito dalla funzione in caso di risposta corretta. *)
(* @return: La struttura 'hintToGive' se \[EGrave] stata selezionata la risposta corretta. *)
(* Restituisce Missing["WrongAnswer"] se \[EGrave] stata selezionata una risposta errata o se il dialogo \[EGrave] stato chiuso prematuramente (es. tramite il pulsante di chiusura della finestra del sistema operativo). *)
(* Restituisce $Failed se il dialogo \[EGrave] stato chiuso prima di qualsiasi interazione e il risultato non \[EGrave] stato impostato esplicitamente (raro, data la gestione con NotebookEventActions). *)
MostraDomandeTrivia[seed_Integer, hintToGive_] := Module[
    {
        questionWindow,
        result = $Failed,
        dialogOpen = True,
        initialQuestionData,
        initialOptionsData,
        initialCorrectIndexData,
        displayStateForWindowClose = "question", (* Per l'evento 'X' della finestra *)

        (* --- Definizioni delle funzioni helper locali --- *)
        internalPerformCloseLogic, (* helper per la logica di chiusura comune *)
        onOsWindowCloseHandler     (* helper per l'evento WindowClose *)
    },

    (* Definisci la logica di chiusura una sola volta *)
    internalPerformCloseLogic[valueToSetForResult_] := (
        result = valueToSetForResult;
        dialogOpen = False;
        NotebookClose[EvaluationNotebook[]];
    );

    (* handler per la chiusura della finestra del SO *)
    onOsWindowCloseHandler = Function[{},
        internalPerformCloseLogic[
            If[displayStateForWindowClose == "correct_show_hint",
                hintToGive,
                Missing["WrongAnswer"]
            ]
        ];
    , HoldAll]; 

    (* Prepara i dati della domanda *)
    {initialQuestionData, initialOptionsData, initialCorrectIndexData} = PreparaTriviaData[seed];

    questionWindow = CreateDialog[
        CreaDialogTriviaUI[
            initialQuestionData,
            initialOptionsData,
            initialCorrectIndexData,
            hintToGive,
            internalPerformCloseLogic, 
            Function[newState, (* callback anonima per aggiornare lo stato esterno *)
                displayStateForWindowClose = newState;
            ]
        ],
        
        WindowTitle -> "Trivia Mastermind Hint",
        WindowSize -> {520, 400}, 
        Modal -> True,
        WindowElements -> {},       
        WindowFrame -> "ModalDialog",
        Background -> White,
        NotebookEventActions -> {
            "WindowClose" :> onOsWindowCloseHandler[] 
        }
    ]; 
  
    While[dialogOpen, Pause[0.1]];
  
    result
];


CreaDialogTriviaUI[
    initialQuestionData_, 
    initialOptionsData_, 
    initialCorrectIndexData_,
    hintToGiveOuter_, 
    performCloseCallback_, (* Callback per l'azione di chiusura dei bottoni interni *)
    updateOuterDisplayStateCallback_ (* Callback per aggiornare lo stato esterno per WindowClose *)
] := DynamicModule[
    { 
        displayStateInternal = "question", (* Stato interno al DynamicModule *)
        localQuestion = initialQuestionData,
        localOptions = initialOptionsData,
        localCorrectIndex = initialCorrectIndexData
    },
    
    (* L'interfaccia utente principale del dialogo *)
    Dynamic@Refresh[
        Switch[displayStateInternal,
            "question",
            DomandeTriviaUI[
                localQuestion,
                localOptions,
                localCorrectIndex,
                (* Funzione anonima per aggiornare entrambi gli stati *)
                Function[newState,
                    displayStateInternal = newState; (* Aggiorna lo stato interno del DM *)
                    updateOuterDisplayStateCallback[newState]; (* Chiama la callback per aggiornare lo stato esterno *)
                ]
            ],
            
            "correct_show_hint",
            TriviaCorrettoUI[hintToGiveOuter, performCloseCallback],
            
            "incorrect_show_message",
            TriviaIncorrettoUI[localOptions, localCorrectIndex, performCloseCallback],
            
            _, 
            Style["Error: Dialog content issue.", Red, Bold]
        ], 
        TrackedSymbols :> {displayStateInternal}
    ]
]


DomandeTriviaUI[
    questionData_, (* contiene questionData["Question"] *)
    optionsList_, 
    correctOptionIndex_, 
    updateDisplayStateCallback_ (* Funzione per cambiare displayState*)
] :=
Column[
    {
    Pane[
        Style[questionData["Question"], 16, Bold, TextAlignment -> Center],
        ImageSize -> {500, 120},
        Scrollbars -> False, Alignment -> Center
    ],
    Spacer[20],
    Grid[
        Partition[
            MapIndexed[
                Function[{optionText, optionIdx}, 
                    (* Questo DynamicModule mantiene lo stato 'clicked' e 'isCorrect' locale al bottone. *)
                    DynamicModule[
                        {
                            clicked = False, 
                            isCorrect = Null, 
                            position = First[optionIdx] (* Estrae l'indice numerico effettivo *)
                        },
                        Button[
                            optionText,
                            (* Azione al click del bottone *)
                            isCorrect = (position == correctOptionIndex);
                            clicked = True;
                            If[isCorrect,
                                updateDisplayStateCallback["correct_show_hint"],
                                updateDisplayStateCallback["incorrect_show_message"]
                            ];,
                            Background -> Dynamic[If[clicked, If[isCorrect, Green, Red], White]],
                            ImageSize -> {200, 80},
                            BaseStyle -> {FontColor -> Black, FontWeight -> Bold, FontFamily -> "Arial", FontSize -> 14},
                            FrameMargins -> 12
                        ]
                    ]
                ],
                optionsList
            ],
            UpTo[Ceiling[Length[optionsList] / 2]]
        ],
        Spacings -> {1, 1}, Alignment -> Center
    ]
    }, 
    Alignment -> Center
]


TriviaCorrettoUI[hintToGiveLocal_List, closeActionCallback_] :=
    With[{theCol = hintToGiveLocal[[1]], posVal = hintToGiveLocal[[2]]}, 
        Column[
            {
            (* Titolo "Correct!" *)
            Style["Correct!", Bold, Green, FontFamily -> "Arial", FontSize -> 36, TextAlignment -> Center],

            (* Pannello per il contenuto dell'indizio *)
            Pane[
                Which[
                    posVal === Missing["PositionNotApplicable"],
                    Row[{
                        Style["This color is present in the combination:", Medium, FontFamily -> "Arial", FontSize -> 18],
                        Spacer[8],
                        Tooltip[Graphics[{EdgeForm[Gray], theCol, Disk[]}, ImageSize -> {25, 25}], ToString[theCol]]
                        }, Alignment -> Center],

                    posVal === Missing["NoSimpleHintAvailable"],
                    Row[{
                        Style["No clue available, you have all the information already", Medium, FontFamily -> "Arial", FontSize -> 18]
                        }, Alignment -> Center],

                    True, (* Caso di default: posVal \[EGrave] una posizione intera *)
                    Row[{
                        Style["Color: ", Medium, FontFamily -> "Arial", FontSize -> 18], Spacer[8],
                        Tooltip[Graphics[{EdgeForm[Gray], theCol, Disk[]}, ImageSize -> {25, 25}], ToString[theCol]], Spacer[8],
                        Style["is in position", Medium, FontFamily -> "Arial", FontSize -> 18], Spacer[8],
                        Style[ToString[posVal], Medium, Bold, FontFamily -> "Arial", FontSize -> 24]
                        }, Alignment -> Center]
                ],
                ImageSize -> {500, Automatic}, 
                Alignment -> Center
            ],

            (* Pulsante "Chiudi" *)
            Button[
                Style["Close", Bold, FontSize -> 24],
                closeActionCallback[hintToGiveLocal] (* Chiama la funzione di callback passata *)
            ]
            },
            Alignment -> Center,
            Spacings -> 15
        ]
    ];


TriviaIncorrettoUI[optionsList_List, correctOptionIndex_Integer, closeActionCallback_] :=
    Column[
        {
        (* Titolo "Incorrect!" *)
        Pane[
            Style["Incorrect!", 36, Bold, Red, FontFamily -> "Arial", TextAlignment -> Center],
            ImageSize -> {500, Automatic}, 
            Alignment -> Center
        ],

        Pane[
            Style["The correct answer is: ", Medium, FontFamily -> "Arial", FontSize -> 18],
            ImageSize -> {500, Automatic},
            Alignment -> Center
        ],

        (* Mostra la risposta corretta *)
        Pane[
            Style[optionsList[[correctOptionIndex]], Medium, Bold, FontFamily -> "Arial", FontSize -> 18],
            ImageSize -> {500, Automatic},
            Alignment -> Center
        ],

        (* Pulsante "Chiudi" *)
        Button[
            Style["Close", Bold, FontSize -> 24],
            closeActionCallback[Missing["WrongAnswer"]] (* Chiama la callback con Missing["WrongAnswer"] *)
        ]
        },
        Alignment -> Center,
        Spacings -> 8
    ]


(* Carica le domande da un file CSV, le elabora e le restituisce come un Dataset strutturato.
Parametri:
- path_String: Il percorso completo del file CSV da cui importare le domande.
Valore di ritorno:
- Un Dataset Mathematica in cui ogni riga \[EGrave] un'associazione (nome_colonna -> valore_dato),
oppure $Failed se si verifica un errore durante il caricamento del file o
l'interpretazione del suo contenuto come dati CSV *)
CaricaTriviaDaCSV[path_String] := Module[
	{csvText, data, headers, rows, dataset},
	
	(* Fase 1: Importa il contenuto grezzo del file CSV come testo. *)
	(* Si utilizza Quiet@Check per gestire errori di importazione senza interrompere bruscamente. *)
	csvText = Quiet @ Check[
	    Import[path, "Text"], (* Importa l'intero file come una singola stringa *)
	    (Print["\:274c Errore: Impossibile importare il testo dal CSV."]; Return[$Failed]) (* Gestione errore importazione testo *)
	];
	
	(* Fase 2: Se l'importazione del testo \[EGrave] riuscita, interpreta la stringa come dati CSV. *)
	data = Quiet @ Check[
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


(* Prepara i dati di una domanda: seleziona la domanda in base al seed, estrae le opzioni di risposta e l'indice della risposta corretta.
@param seed: Intero usato per la selezione (pseudo)casuale della domanda.
@return: Lista contenente {domandaSelezionata, opzioniDisponibili, indiceRispostaCorretta} *)
PreparaTriviaData[seed_Integer] := Module[
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


(* Calcola un indizio per un gioco tipo Mastermind, basato sulla cronologia dei tentativi e sulla soluzione.
   Usa Catch/Throw per gestire tutti i percorsi di uscita.

Priorit\[AGrave] degli Indizi (la funzione restituisce il primo indizio applicabile in questa lista):
1. Nessun Tentativo Giocato
2. Colore della Soluzione Non Confermato
3. Colore con Feedback Parziale (Mai Esatto), Posizionato
4. Fallback

@param hintFeedbackHistoryInput_List: Cronologia dei turni. Ogni turno \[EGrave] una lista di 
    {{colore_tentato, simbolo_feedback}, ...}. Turni vuoti {} sono ammessi.
@param soluzioneListInput_List: Lista dei colori che compongono la soluzione segreta.
@return (tramite Throw catturato): Uno dei seguenti:
- Missing["SolutionIsEmpty"]: Se 'soluzioneListInput' \[EGrave] vuota.
- {colore, Missing["PositionNotApplicable"]}: Indizio senza posizione specifica.
- {colore, posizione_Integer}: Indizio con posizione.
- {colore_placeholder, Missing["NoSimpleHintAvailable"]}: Indizio di fallback. *)
DecidiSuggerimentoLogica[hintFeedbackHistoryInput_List, soluzioneListInput_List] := Catch[
    Module[
        {
            soluzione = soluzioneListInput,
            hintFeedbackHistory = hintFeedbackHistoryInput,
            n,
            tentativiEffettuati
        },

        n = Length[soluzione];
        If[n == 0,
            Throw[Missing["SolutionIsEmpty"]]
        ];

        (* Filtra e conserva solo i turni con dati (non vuoti) una sola volta *)
        tentativiEffettuati = Select[hintFeedbackHistory, # =!= {} &];

        (* --- Priorit\[AGrave] 1: Gestione del caso "Nessun Tentativo Giocato" --- *)
        GestisciHintNessunTentativo[soluzione, tentativiEffettuati]; (* Pu\[OGrave] fare Throw *)

        (* --- Priorit\[AGrave] 2: Cerca colori della soluzione non ancora "confermati" --- *)
        GestisciHintColoreNonConfermato[soluzione, tentativiEffettuati, feedbackEsatto, feedbackParziale]; (* Pu\[OGrave] fare Throw *)
        
        (* --- Priorit\[AGrave] 3: Indizio per un colore con 'feedbackParziale' ma mai 'feedbackEsatto' --- *)
        GestisciHintConPosizione[soluzione, tentativiEffettuati, n, feedbackEsatto, feedbackParziale]; (* Pu\[OGrave] fare Throw *)

        (* --- Priorit\[AGrave] 4: Indizio di Fallback --- *)
        (* Se nessun altro indizio specifico \[EGrave] stato generato, fornisce un indizio generico. *)
        (* 'Green' \[EGrave] un placeholder; potrebbe essere un colore non usato o un simbolo specifico. *)
        Throw[{Green, Missing["NoSimpleHintAvailable"]}]
    ] (* Fine Module *)
]; (* Fine Catch *)


(* === Helper per Priorit\[AGrave] 1: Gestisce il caso di nessun tentativo giocato ===
Se non ci sono tentativi, lancia un indizio con un colore casuale dalla soluzione.
@param soluzione_List: La lista dei colori della soluzione segreta.
@param tentativiEffettuati_List: La lista dei tentativi che contengono dati (non vuoti).
@effect: Pu\[OGrave] lanciare (Throw) un indizio se non ci sono tentativi. *)
GestisciHintNessunTentativo[soluzione_List, tentativiEffettuati_List] :=
    If[Length[tentativiEffettuati] == 0,
        Throw[{RandomChoice[soluzione], Missing["PositionNotApplicable"]}]
    ];

(* === Helper per Priorit\[AGrave] 2: Trova un colore della soluzione non ancora confermato ===
Identifica i colori nella soluzione che non hanno mai ricevuto feedback (n\[EAcute] esatto n\[EAcute] parziale)
e, se ne trova uno, lancia un indizio con quel colore.
@param soluzione_List: La lista dei colori della soluzione segreta.
@param tentativiEffettuati_List: La lista dei tentativi che contengono dati (non vuoti). 
                                Ogni tentativo \[EGrave] una lista di {coloreTentato, simboloFeedback}.
@param feedbackEsattoSimbolo_: Il simbolo che rappresenta un feedback esatto.
@param feedbackParzialeSimbolo_: Il simbolo che rappresenta un feedback parziale.
@effect: Pu\[OGrave] lanciare (Throw) un indizio se trova un colore non confermato. *)
GestisciHintColoreNonConfermato[soluzione_List, tentativiEffettuati_List, feedbackEsattoSimbolo_, feedbackParzialeSimbolo_] :=
    Module[
        {
            coloriUniciSoluzione, 
            coloriConfermati, 
            coloreTentato, simboloFeedback (* Variabili per chiarezza dentro il Do *)
        },

        coloriUniciSoluzione = DeleteDuplicates[soluzione];
        coloriConfermati = Association[# -> False & /@ coloriUniciSoluzione];

        (* Itera su tutti i pioli di tutti i tentativi effettuati per marcare i colori confermati *)
        Do[
            coloreTentato = piolo[[1]];
            simboloFeedback = piolo[[2]];
            
            If[simboloFeedback === feedbackEsattoSimbolo || simboloFeedback === feedbackParzialeSimbolo,
                If[KeyExistsQ[coloriConfermati, coloreTentato],
                    coloriConfermati[coloreTentato] = True;
                ]
            ];
            ,
            {turno, tentativiEffettuati}, (* itera su ogni 'turno' nella lista 'tentativiEffettuati' *)
            {piolo, turno}              (* per ogni 'turno', itera su ogni 'piolo' in quel 'turno' *)
        ];

        (* Controlla se c'\[EGrave] un colore della soluzione non ancora confermato *)
        Do[
            If[Not[coloriConfermati[colore]], (* Se il colore NON \[EGrave] confermato *)
                Throw[{colore, Missing["PositionNotApplicable"]}]
            ];
            ,
            {colore, coloriUniciSoluzione} 
        ];
    ];
(* === Helper per Priorit\[AGrave] 3: Trova un indizio per un colore con feedback parziale ma mai esatto ===
   Questa versione replica pi\[UGrave] da vicino la logica originale con cicli For.
   Se tutti i colori della soluzione sono stati "confermati" (secondo la logica di Priorit\[AGrave] 2),
   questa funzione cerca un colore che abbia ricevuto 'feedbackParziale' ma mai 'feedbackEsatto'
   e ne suggerisce la posizione corretta nella soluzione.

@param soluzione_List: La lista dei colori della soluzione segreta.
@param tentativiEffettuati_List: La lista dei tentativi che contengono dati (non vuoti). 
                                Ogni tentativo \[EGrave] una lista di {coloreTentato, simboloFeedback}.
@param nPioli_Integer: La lunghezza della combinazione (numero di pioli per tentativo).
@param feedbackEsattoSimbolo_: Il simbolo che rappresenta un feedback esatto.
@param feedbackParzialeSimbolo_: Il simbolo che rappresenta un feedback parziale.
@effect: Pu\[OGrave] lanciare (Throw) un indizio {colore, posizione} se trova un candidato valido. *)

GestisciHintConPosizione[
    soluzione_List, 
    tentativiEffettuati_List, 
    nPioli_Integer, 
    feedbackEsattoSimbolo_, 
    feedbackParzialeSimbolo_
] := Module[
    {
        (* Lista che accumula i colori con feedbackParziale*)
        colorListCandidati = {}, 
        coloriConFeedbackEsattoUnici,
        targetHintColor,
        i, j, (* Indici per i cicli For *)
        turnoCorrente, pioloCorrente, coloreTentato, simboloFeedback (* Variabili di iterazione *)
    },

    (* --- Logica 3a: Raccogli tutti i colori che hanno ricevuto 'feedbackParziale'  *)
    (* Itera su ogni piolo di ogni tentativo *)
    For[i = 1, i <= Length[tentativiEffettuati], i++,
        turnoCorrente = tentativiEffettuati[[i]];
        For[j = 1, j <= nPioli, j++, (* Usa nPioli, che corrisponde a 'n' originale *)
            pioloCorrente = turnoCorrente[[j]];
            coloreTentato = pioloCorrente[[1]];
            simboloFeedback = pioloCorrente[[2]];
            If[simboloFeedback === feedbackParzialeSimbolo,
                AppendTo[colorListCandidati, coloreTentato];
            ];
        ];
    ];

    (* --- Logica 3b: Dalla lista appena creata, rimuovi i colori che hanno ricevuto anche solo una volta 'feedbackEsatto'  *)
    (* Prima, identifica quali colori unici hanno ricevuto feedbackEsatto *)
    coloriConFeedbackEsattoUnici = {};
    For[i = 1, i <= Length[tentativiEffettuati], i++,
        turnoCorrente = tentativiEffettuati[[i]];
        For[j = 1, j <= nPioli, j++,
            pioloCorrente = turnoCorrente[[j]];
            coloreTentato = pioloCorrente[[1]];
            simboloFeedback = pioloCorrente[[2]];
            If[simboloFeedback === feedbackEsattoSimbolo,
                AppendTo[coloriConFeedbackEsattoUnici, coloreTentato];
            ];
        ];
    ];
    coloriConFeedbackEsattoUnici = DeleteDuplicates[coloriConFeedbackEsattoUnici];

    (* Ora rimuovi questi colori da colorListCandidati *)
    For[i = 1, i <= Length[coloriConFeedbackEsattoUnici], i++,
        colorListCandidati = DeleteCases[colorListCandidati, coloriConFeedbackEsattoUnici[[i]]];
    ];
    (* --- Logica 3c: Se ci sono colori candidati, prendi il primo, verifica e lancia *)
	If[Length[colorListCandidati] > 0,
	    targetHintColor = First[colorListCandidati];
	    posizioniTrovate = Position[soluzione, targetHintColor, {1}, 1]; 
	    If[Length[posizioniTrovate] > 0,
	        Throw[{targetHintColor, First@First@posizioniTrovate}]
	    ]
	]
	(* Se nessun Throw \[EGrave] avvenuto, la funzione termina e il controllo passa alla Priorit\[AGrave] 4 in CalcolaHintSemplice *)
];


(* === Codice usato per il bottone di avvio nel notebook ===
	Button["Avvia Programma", FrontEndExecute[FrontEndToken[InputNotebook[], "EvaluateNotebook"]],
	BaseStyle -> {"GenericButton", 16, Bold}, ImageSize -> {175, 50}] *)


End[];
EndPackage[];
