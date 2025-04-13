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
     Panel[
		Column[{
			Style["Mastermind Game",Bold,18,FontFamily->"Arial"],
			Spacer[10],
			Column[{
				Row[{
					Style["number of items",10],
					Spacer[10],
					RadioButtonBar[Dynamic[numPegs],{2,3,4,5,6},Appearance->"Horizontal"]
					},
					Alignment->Center
				],
				Spacer[20],
				Row[{
					Style["number of colors",10],
					Spacer[10],
					RadioButtonBar[Dynamic[numColors],{2,3,4,5,6,7,8},Appearance->"Horizontal"]
				},
				Alignment->Center
				]
			}],
Spacer[10],
Row[{
Button["replay same game",Null,ImageSize->120],
Spacer[10],
Button["new game",Null,ImageSize->120],
Spacer[10],
Button["reveal answer",Null,ImageSize->120]
}]
}],ImageSize->400, Background->White]

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


End[];
EndPackage[];
