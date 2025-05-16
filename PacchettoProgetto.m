(* ::Package:: *)

(* :Title:Trivia Mastermind*)
(* :Context:PacchettoProgetto`*)
(* :Author:Gruppo 10 - I Ludopatici*)
(* :Summary:Package per "Trivia Mastermind", progetto di MC Unibo anno 24/25*)
(* :Package Version:1.0*)
(* :History:last modified 16/5/2025*)
(* :Copyright:\[Copyright] 2025 Gruppo 10 - Trivia Mastermind*)
(* :License:MIT License*)

BeginPackage["PacchettoProgetto`"];
(*ClearAll["PacchettoProgetto`*"];*)

(* USAGE DI FUNZIONI CHIAMATE ESPLICITAMENTE NEL NOTEBOOK *)
avviaSchermataDiGioco::usage="Avvia l\[CloseCurlyQuote]interfaccia grafica principale, visualizzando una schermata iniziale da cui \[EGrave] possibile personalizzare i parametri del gioco e avviare una nuova partita.";


Begin["`Private`"];

(* Variabili Globali *)

(*
  Spiegazione del funzionamento per 'triviaData':
  Questa definizione impiega una tecnica nota come "caricamento differito" (o "lazy loading") con "memoizzazione".
  L'operatore ':=' fa s\[IGrave] che l'operazione specificata (in questo caso, LoadQuestionsFromCSV)
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
  testato aggiungendo un Print alla funzione di LoadQuestionsFromCSV
  Calcolando triviaData := LoadQuestions... nello stesso modo porta lo stesso problema, entrambi i casi portano a 
  perdite di prestazione, anche con cache automatiche per il risultato
  
  le parentesi chiuse non sono necessarie, ma lo trovo molto meno intuitivo scrivere:
  triviaData := triviaData = LoadQuestionsFromCSV["trivia.csv"];
  
  le giuro per\[OGrave] che \[EGrave] l'unica volta che uso le parentesi, le ho tolte in tutte le altri parti

  erano presenti un paio di cose extra che pensavo fossero necessario per le variabili globali fatte cos\[IGrave] 
  ma aveva ragione che potevano essere tolte.
*)
triviaData := (
    triviaData = LoadQuestionsFromCSV["trivia.csv"]
);


(* Lista dei colori usati da Mastermind *)
(* In particolare, \[EGrave] stato utilizzato RGBColor al posto di Red per ottenere una tonalit\[AGrave] di rosso pi\[UGrave] scura *)
paletteColori={RGBColor[0.9,0,0], Green, Yellow, Blue, Orange, Brown, Purple, Cyan, Magenta, White, Gray, Black};


(* Stato della partita *)
partitaInCorso=True


(* Libreria di etichette *)
labels=translations = <|
	"titoloGioco"->"TRIVIA MASTERMIND",
	"fattoDa"->"by Alessandro Modelli, Angelo Greco, Elia Friberg, Francesca Mazzetti, Gianpiero Tovo, Matteo Raggi",
	"inserisciSeed"->"Insert seed: ",
	"placeholderSeed"->"Write a numeric seed...",
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
{
    screenWidth,              (* Larghezza del display *)
    screenHeight,             (* Altezza del display *)
    titleFontScale,           (* Dimensione del testo nella schermata di menu, proporzionale allo schermo *)
    seedInserito="",          (* Variabile per la meomrizzazione temporanea del seed di gioco *)
    customSeed,               (* Variabile in cui \[EGrave] memorizzato il seed della partita *)
    customTurni=8,            (* Numero di tentativi scelti per indovinare il codice *)
    customLunghezzaCodice=4,  (* Lunghezza del codice segreto *)
    allowDuplicates=True,     (* Flag per la presenza di colori ripetuti nella combinazione *)
    currentScreen="menu",     (* Schermata attiva *)
    content,                  (* Contenuto mostrato nella finestra *)
    mainWindow                (* Finestra di visualizzazione *)
},

	
    (* Calcola una dimensione proporzionale alla risoluzione dello schermo, ottenuta tramite i valori di width e height.
    Il valore restituito \[EGrave] usato per scalare i titoli della schermata inziale.  
	La funzione non viene richiamata nella schermata successiva perch\[EGrave] l\[IGrave] le dimensioni degli elementi grafici sono fisse.
	Tuttavia, il risultato visivo finale anche in questa schermata rimane ben proporzionato.
	La risoluzione dello schermo viene ottenuta tramite il FrontEnd.
	In caso di errore, usa un valore di default (1920x1080) *)
    aggiornaDimensioniSchermo[] := ( 
        Quiet @ Check[ (* Quiet permette che eventuali messaggi di errore non siano visualizzati dall'utente *)
	        {screenWidth, screenHeight} = FrontEndExecute @ FrontEnd`Value[FE`getScreenSize[]],
			{screenWidth, screenHeight} = {1920, 1080}
		];
		(* Restituisce una dimensione proporzionale al lato pi\[UGrave] corto *)
        titleFontScale=Min[screenWidth, screenHeight]/15; (* Il divisore 15 \[EGrave] stato definito sperimentalmente *)
    );
    
    
    (* All'inizio del gioco, quando la schermata viene aperta per la prima volta, vengono
    calcolate le dimensioni di altezza e larghezza necessarie per visualizzare correttamente i titoli *)
    aggiornaDimensioniSchermo[];
    
    
    (* Funzione che aggiorna la variabile currentScreen, per modificare la schermata visualizzata dall'utente. 
    Il parametro passato in input newScreen pu\[OGrave] assumere i valori: "menu" e "gioco" *)
    cambiaSchermata[newScreen_] := ( 
        (* Utile nel caso in cui la nuova schermata sia "menu": ricalcola le dimensioni della
        finestra per addattarsi correttamente alla schermata che si sta per visualizzare *)
        aggiornaDimensioniSchermo[]; 
	    currentScreen=newScreen;
    );


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
		    (* Pulsante che se premuto genera un seed casuale tra 1 e 9999999999 *)
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
			        seedInserito=RandomInteger[{1, 9999999999}]; 
			    ]
			],
			    
		    Spacer[15],
	        
	        (* Campo di input per inserire o visualizzare il seed *)
		    Item[
			    Framed[
				    InputField[
					    Dynamic[seedInserito], (* Associa dinamicamente il campo al valore di seedInserito *)
					    (* Sono amessi solo numeri interi *)
					    (* Se l'utente tentasse di inserire da tastiera elementi diversi da quelli indicati, questi non sarebbero visualizzati *)
					    Number,
					    FieldHint->labels["placeholderSeed"], (* Suggerimento testuale se si lascia il campo vuoto *)
					    FieldHintStyle->{Italic},
					    ImageSize->{250, 21},
					    Appearance->"Frameless",
					    BaselinePosition->Center,
					    ContinuousAction->True (* Aggiornamento continuo: serve per attirare e disattivare il tasto Play *)
				    ],
				(* Stie della barra di inserimento *) 
			    Background->LightGray,
			    FrameStyle->None,
			    RoundingRadius->10,
			    FrameMargins->{{10, 10}, {5, 5}},
			    ImageSize->Automatic
			    ],
		    ItemSize->Automatic 
		    ],
	       
		    Spacer[15],
		    
		    (* Pulsante play, attivo solo se il seed inserito \[EGrave] valido (numero intero positivo).
		    L'aspetto del tasto varia visivamente in base al suo stato: se il seed non \[EGrave] valido, il 
		    tasto appare disattivato e non \[EGrave] cliccabile; se invece il seed \[EGrave] corretto, il tasto si
		    colora, consentendo l'avvio della partita *)
		    Dynamic[
			    If[IntegerQ[seedInserito] && seedInserito > 0,
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
						    cambiaSchermata["gioco"];
					    ]
				    ],
			        
				    (* Caso seed non valido - >tasto disabilitato  *)
				    Framed[
					    Style[labels["play"], FontSize->18, FontColor->GrayLevel[0.8]],
					    Background->RGBColor[0, 0.5, 0], (* Stesso sfondo verde, ma ingrigito per indicare la disattivazione *)
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
	
	    Spacer[{0, 125}],
	    
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
    ];


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
                customSeed, customTurni, customLunghezzaCodice, allowDuplicates]
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
	mainWindow=CreateDocument[
	{
        Cell[
            BoxData @ ToBoxes @ content, (* Converte "content" sopra creato in box per essere visualizzato dall'utente *)
            "Output",                    (* Specifica che \[EGrave] una cella di output *)
            ShowCellBracket->False,
            CellMargins->{{0, 0}, {0, 0}}
        ]
    },  
        (* Propriet\[AGrave] della finestra *)
        WindowSize->Full,
        WindowFrame->"Frameless",
        WindowElements->{},
        Background->White,
        Editable->False, (* Impedisce modifiche manuali al contenuto della finestra da parte dell'utente *)
        Deployed->True,  (* Disabilita interazinoi non previste, come la selezione di oggetti sulla schermata *)
        WindowMargins->{{0, 0}, {0, 0}},
        NotebookEventActions->{
            {"KeyDown", "Escape"} :> NotebookClose[EvaluationNotebook[]] (* Se l'utente preme QUIT, chiude la finestra *)
        }
    ];  
  
  
    mainWindow (* Restituisce la finestra appena creata e tutto il suo contenuto *)
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


(* Funzione di generazione dell'interfaccia della griglia di gioco dinamica, prende in input il seed,
il numero di colori che formano la combinazione segreta, il numero di tentativi e un flag per l'utilizzo di colori duplicati.
Genera: 
 -Griglia di tentativi e relative combinazioni
 -Griglia di feedback per ciascun tentativo
 -Bottone di check del tentativo
 -Bottone di Hint per l'avvio del Trivia
 -Funzioni di restart e termina partita
*)
interfacciaGriglia[seed_, lunghezzaCombinazione_, numeroTentativi_, allowDuplicates_] := DynamicModule[
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
					                                    Module[{currentTurnFeedbackSymbols=valutazioneTentativo[[2]], combinedTurnData},
					                                        combinedTurnData=Table[{tentativoList[[i]], currentTurnFeedbackSymbols[[i]]}, {i, Length[tentativoList]}];
					                                        (* Il risultato della valutazione (feedback) viene salvato nella variabile hintFeedbackHistory *)
					                                        hintFeedbackHistory[[turn]]=combinedTurnData;
					                                    ];
					                                    
					                                    If[valutazioneTentativo[[1]] === mastermindProsegui, turn++];
					                                    
					                                    (* Se il feedback indica vittoria: si ferma la partita, si mostra un dialog
					                                    di vittoria e tutte le variabili principali vengono resettate per preparare
					                                    una nuova partita *)
					                                    If[Length[valutazioneTentativo] > 0 && valutazioneTentativo[[1]] === mastermindVittoria,
					                                    
						                                    partitaInCorso=False;
						                                    
						                                    DisplayEndGameDialog[True,
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
							                                    ]
						                                    ];
					                                    ];
					                                    
                                                        (* Se il feedback indica sconfitta: si ferma la partita, si mostra il dialog
                                                        di sconfitta e, anche qui, tutto viene resettato *)
					                                    If[Length[valutazioneTentativo] > 0 && valutazioneTentativo[[1]] === mastermindSconfitta,
						                                    partitaInCorso=False;
						                                    
						                                    DisplayEndGameDialog[False,
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
							                                    ]
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
                                    ]] (* fine dynamic[module]*)
                                    
                                    Spacer[50],
                                        
                                    (* emptyResultPlaceholder rappresenta uno stato iniziale o nullo per il 
                                    risultato di un aiuto non ancora disponibile. *)
                                    emptyResultPlaceholder = Missing["NoResultSetYet"];
                                    Dynamic[ 
                                        (* Il modulo dinamico si aggiorna automaticamente ogni volta che 
                                        cambia lo stato della variabile correct *)
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
				                                    Style["\:274c", FontSize->18],
				                                    Background->GrayLevel[0.95], 
				                                    FrameStyle->Red, 
				                                    RoundingRadius->10,
				                                    FrameMargins->{{10, 10}, {0, 0}},
				                                    ImageSize->{80, 35},
				                                    Alignment->Center
			                                    ],
			
			                                    (* Caso 2: Il risultato \[EGrave] una risposta corretta alla domanda per l'aiuto (tipicamente un indizio del tipo {colore, valore/posizione}). *)
			                                    MatchQ[currentValForRowX, {_?ColorQ, _}], (* Controlla se currentValForRowX corrisponde al pattern {un Colore, un qualcheValore} *)
			                                    Module[ 
				                                {
				                                    resultColor=First[currentValForRowX],
				                                    resultValue=Last[currentValForRowX] 
				                                },
				                                    Framed[
					                                    (* Il contenuto del box verde "Indizio Corretto" dipende da 'resultValue'. *)
					                                    If[resultValue =!= Missing["NoSimpleHintAvailable"],
						                                    If[resultValue =!= Missing["PositionNotApplicable"],
							                                    (* Se l'indizio ha un valore di posizione specifico: mostra colore + valore *)
							                                    Row[{
								                                    Graphics[{EdgeForm[Gray], resultColor, Disk[]}, ImageSize->{20, 20}],
								                                    
								                                    Spacer[5],
								                                    
								                                    Column[{
																	    Style[ToString[resultValue], 16, Bold, FontFamily->"Arial"] (*mette apposto lo spazio verticale, 
																	    pensavo di dover metter Spacer ma Column lo sposta gi\[AGrave] abbastanza*)
																	},
																	Spacings->0
																	]
							                                     }],
							                                     (* Altrimenti (l'indizio indica che la posizione non \[EGrave] applicabile o non c'\[EGrave] un indizio semplice): mostra solo il colore *)
							                                     Graphics[{EdgeForm[Gray], resultColor, Disk[]}, ImageSize->{20, 20}]
						                                     ],
						                                     "" (* Se non ci sono altri suggerimenti da dare \[EGrave] vuoto *)
					                                     ],
					                                 Background->GrayLevel[0.95], 
					                                 FrameStyle->Darker[Green],
					                                 RoundingRadius->10,
					                                 FrameMargins->{{30, 10}, {6, 6}},
					                                 ImageSize->{80, 35}
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
						                                     Grid[{{
						                                         Style["\|01f4a1", FontSize->10],
						                                         Style["HINT", White, FontFamily->"Consolas", FontSize->12, Bold]
						                                     }},
						                                     Alignment->{Center, Center},
						                                     Spacings->{1, 0}
						                                     ],
					                                     Background->Blue,
					                                     FrameStyle->None,
					                                     RoundingRadius->10,
					                                     FrameMargins->{{10, 10}, {10, 10}},
					                                     ImageSize->{80, 35}
					                                     ],
					                                     Function[ 
						                                     (* 'AppendTo' aggiunge l'esito della domanda per l'aiuto. Questo \[EGrave] coerente con la logica del pulsante "VAI",
						                                     dove aggiungiamo {} per mantenere la struttura anche se l'aiuto non viene usato. *)
						                                     If[partitaInCorso,
							                                     AppendTo[correct, DisplayTriviaQuestion[seed + questionCounter, CalcolaHintSemplice[hintFeedbackHistory, soluzioneList]]];
							                                     questionCounter++; (* Incrementa un contatore, per avere seed unici per le domande trivia *)
						                                     ]
					                                     ],
					                                     Method->"Queued"
				                                     ],
				
				                                     (* --- Mostra un Pulsante "HINT" Inattivo/Segnaposto --- *)
				                                     (* Mostrato se le condizioni per un pulsante HINT attivo non sono soddisfatte, 
				                                     \[EGrave] un copia incolla di quello sopra per preservare interazione eventuale tra le cose, e per evitare problemi  *)
				                                     Framed[
					                                     Grid[{{
					                                         Style["\|01f4a1", FontSize->10, FontColor->Directive[GrayLevel[0.7], Opacity[0]]], (* Icona resa invisibile *)
					                                         Style["HINT", FontFamily->"Consolas", FontSize->12, FontColor->Directive[GrayLevel[0.7], Opacity[0]], Bold] (* Testo reso invisibile *)
					                                     }},
					                                     Alignment->{Center, Center},
					                                     Spacings->{1, 0}
					                                     ],
				                                     Background->GrayLevel[0.9],
				                                     FrameStyle->None,
				                                     RoundingRadius->10,
				                                     FrameMargins->{{10, 10}, {10, 10}},
				                                     ImageSize->{80, 35}
				                                     ]
			                                     ]
		                                     ];
		
		                                     displayOutput (* Dynamic si valuta a questo elemento UI *)
	                                     ] (* Fine Module *)
                                     ] (* Fine Dynamic *)
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
creaSchermataGioco[seed_, tentativi_, combinazione_, allowDuplicates_] := DynamicModule[
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
						Function[cambiaSchermata["menu"]] (* Cambio schermata al click, ritornando al menu principale *)
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
					interfacciaGriglia[seed, combinazione, tentativi, allowDuplicates], 
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
 
(* === Funzione che visualizza un dialogo di fine partita ===
In  aggiunta, si hanno opzioni per riavviare il gioco, tornare al menu e chiudere la finestra di dialogo.
Parametri:
- hasWon: True se il giocatore ha vinto, False altrimenti
- onRestartFunc: funzione opzionale da eseguire se si sceglie Restart *)
DisplayEndGameDialog[hasWon_, onRestartFunc_: Null] := Module[
    {},
	CreateDialog[ (* Gestisce tutta la funzione e permette la creazione di una finestra modale *)
		DynamicModule[{},
			Pane[
				Column[{ (* Si ha una colonna con tre elementi *)
				    (* Titolo mostrato \[EGrave] lo stato della partita: 
				    l'utente pu\[OGrave] avere perso o vinto giocando *)
					Style[
						Pane[If[hasWon, "You won!", "You lost!"], 
						ImageSize -> {400, Automatic}, Alignment -> Center],
						36, Bold, FontFamily -> "Arial",
						If[hasWon, Darker[Green], Red] (* Verde se vittoria, Rosso altrimenti *)
					],
					
					Pane[
					    (* Bottone restart *)
						ClickPane[
							Framed[
								Style[labels["restartButton"], White, FontFamily -> "Consolas", FontSize -> 18, Bold],
								Background -> RGBColor[0, 0.5, 0],
								FrameStyle -> None,
								RoundingRadius -> 10,
								FrameMargins -> {{30, 30}, {10, 10}},
								ImageSize -> {Automatic, Automatic}
							],
							(* Se cliccato su restart, si esegue la funzione onRestartFunc, se fornita.
							Viene poi chiusa la finestra di dialogo *)
							Function[
							    If[onRestartFunc =!= Null, onRestartFunc[]];
							    NotebookClose[EvaluationNotebook[]];
							]
						],
						Alignment -> Center
					],
					
					Pane[
						Row[{
						    (* Bottone esci e chiudi finestra di dialogo *)
							ClickPane[
								Framed[
									Style[labels["esci"], White, FontFamily -> "Consolas", FontSize -> 18, Bold],
									Background -> Red,
									FrameStyle -> None,
									RoundingRadius -> 10,
									FrameMargins -> {{15, 15}, {5, 5}},
									ImageSize -> {Automatic, Automatic}
								],
								(* Cliccando quit si torna al menu principale, richiamando
								la funzione cambiaSchermata *)
								Function[
								    cambiaSchermata["menu"];
								    NotebookClose[EvaluationNotebook[]];
								]   
							],
							
							Spacer[10],
							
							ClickPane[
								Framed[
									Style[labels["closeDialog"], White, FontFamily -> "Consolas", FontSize -> 18, Bold],
									Background -> Gray,
									FrameStyle -> None,
									RoundingRadius -> 10,
									FrameMargins -> {{15, 15}, {5, 5}},
									ImageSize -> {Automatic, Automatic}
								],
								(* Il tasto chiudi chiude solo la finestra di dialogo, senza altre azioni *)
								Function[NotebookClose[EvaluationNotebook[]]]
							]
						}
						],
						Alignment -> Center
					],
					
					Spacer[10]
				},
				Spacings-> 5,
				Alignment -> Center
				],
				ImageSize -> {450, 300},
				Alignment -> {Center, Top}
			]
		],
		WindowTitle -> If[hasWon, "WIN!", "LOSE!"], (* Titolo della finestra *)
		WindowSize -> {450, 300},
		Modal -> True, (* Indica che l'utente deve interagire con la finestra prima di tornare al resto del programma *)
		WindowElements -> {},
		WindowFrame -> "ModalDialog"
	]
]


(* Carica le domande da un file CSV, le elabora e le restituisce come un Dataset strutturato.
Parametri:
- path_String: Il percorso completo del file CSV da cui importare le domande.
Valore di ritorno:
- Un Dataset Mathematica in cui ogni riga \[EGrave] un'associazione (nome_colonna -> valore_dato),
oppure $Failed se si verifica un errore durante il caricamento del file o
l'interpretazione del suo contenuto come dati CSV *)
LoadQuestionsFromCSV[path_String] := Module[
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
	Assicura che il ciclo While principale in DisplayTriviaQuestion termini e imposti un risultato appropriato *)
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
											    isCorrect = (position == localCorrectIndex); 
											    clicked = True; (* Contrassegna questo pulsante come cliccato per il feedback visivo. *)
											                          
											    (* Passa alla vista di feedback appropriata. *)
											    If[isCorrect,
												    displayState = "correct_show_hint",
												    displayState = "incorrect_show_message"
											    ];,
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
					    Style["Correct!", Bold, Green, FontFamily -> "Arial", FontSize -> 36, TextAlignment -> Center],
					    Pane[
						    (* Questo Modulo interno serve solo per localizzare 'theCol' e 'posVal', se necessario, usato perch\[EGrave] ho avuto problemi di scoping *)
						    Module[{theCol, posVal},
							    {theCol, posVal} = hintToGive; (* Scompatta la struttura dell'indizio 'hintToGive'. *)
							                
							    (* Mostra l'indizio in base alla sua struttura. *)
							    Which[
								    posVal === Missing["PositionNotApplicable"],
								    Row[{
									    Style["This color is present in the combination:", Medium, FontFamily -> "Arial", FontSize -> 18],
									    Spacer[8],
									    Tooltip[Graphics[{EdgeForm[Gray], theCol, Disk[]}, ImageSize -> {25, 25}], ToString[theCol]]
								    }, Alignment -> Center],
								
								    posVal === Missing["NoSimpleHintAvailable"],
								    Row[{
								        Style["No clue available, you have all the information", Medium, FontFamily -> "Arial", FontSize -> 18]
								    }, Alignment -> Center],
								                  
								    True, (* Caso di default: si assume che posVal sia un intero che rappresenta una posizione. *)
								    Row[{
								        Style["Color: ", Medium, FontFamily -> "Arial", FontSize -> 18], Spacer[8],
								        Tooltip[Graphics[{EdgeForm[Gray], theCol, Disk[]}, ImageSize -> {25, 25}], ToString[theCol]], Spacer[8],
								        Style["is in position", Medium, FontFamily -> "Arial", FontSize -> 18], Spacer[8],
								        Style[ToString[posVal], Medium, Bold, FontFamily -> "Arial", FontSize -> 24]
								    }, Alignment -> Center]
							    ] 
						    ], 
						    {500, Automatic}, 
						    Alignment -> Center
					    ],
					    Button[ (* Pulsante "Chiudi" per "Corretto!" *)
					        Style["Close", Bold, FontSize -> 24],
					        performCloseAction[hintToGive] (* Restituisce la struttura originale hintToGive come risultato. *)
					    ]
				    }, Alignment -> Center, Spacings -> 15], 
				
				    "incorrect_show_message",
				    (*Mostrata quando l'Utente Risponde Incorrettamente  *)
				    Column[{
					    Pane[
						    Style["Incorrect!", 36, Bold, Red, FontFamily -> "Arial", TextAlignment -> Center],
						    {500, Automatic}, Alignment -> Center
					    ],
					    Pane[
						    Style["The correct answer is: ", Medium, FontFamily -> "Arial", FontSize -> 18],
						    {500, Automatic}, Alignment -> Center
					    ],
					    Pane[
						    Style[localOptions[[localCorrectIndex]], Medium, Bold, FontFamily -> "Arial", FontSize -> 18],
						    {500, Automatic}, Alignment -> Center
					    ],
					    Button[ (* Pulsante "Chiudi" per "Incorretto!" *)
					        Style["Close", Bold, FontSize -> 24],
					        performCloseAction[Missing["WrongAnswer"]] (* Restituisce Missing["WrongAnswer"] come risultato. *)
					    ]
				    }, Alignment -> Center, Spacings -> 8], 
				
				    _, (* Caso di default per Switch[displayState, ...]: gestisce qualsiasi stato imprevisto. *)
				    Style["Error: the dialog is not available. Please report the issue.", Red, Bold]
				          
			    ], 
			    TrackedSymbols :> {displayState} (* Assicura che il contenuto Dinamico si aggiorni solo quando 'displayState' cambia. *)
		    ] 
	    ], 
	    
	    WindowTitle -> "Trivia Mastermind Hint",
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


(* Prepara i dati di una domanda: seleziona la domanda in base al seed, estrae le opzioni di risposta e l'indice della risposta corretta.
@param seed: Intero usato per la selezione (pseudo)casuale della domanda.
@return: Lista contenente {domandaSelezionata, opzioniDisponibili, indiceRispostaCorretta} *)
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


(* Calcola un indizio per un gioco tipo Mastermind, basato sulla cronologia dei tentativi e sulla soluzione.
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
- {colore_placeholder, Missing["NoSimpleHintAvailable"]}: Indizio di fallback (Priorit\[AGrave] 4) *)
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
