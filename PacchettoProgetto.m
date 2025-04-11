(* Content-type: application/vnd.wolfram.mathematica *)

(*** Wolfram Notebook File ***)
(* http://www.wolfram.com/nb *)

(* CreatedBy='Wolfram 14.2' *)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[       154,          7]
NotebookDataLength[     19384,        413]
NotebookOptionsPosition[     18560,        392]
NotebookOutlinePosition[     19054,        411]
CellTagsIndexPosition[     19011,        408]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{
Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{":", "Title", ":", 
    RowBox[{"Trivia", " ", "Mastermind"}]}], "*)"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Context", ":", "PacchettoProgetto`"}], "*)"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Author", ":", 
    RowBox[{
     RowBox[{"Gruppo", " ", "10"}], " ", "-", " ", 
     RowBox[{"I", " ", "Ludopatici"}]}]}], "*)"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{":", "Summary", ":", 
     RowBox[{"Package", " ", "per", " ", "\"\<Trivia Mastermind\>\""}]}], ",",
     " ", 
    RowBox[{"progetto", " ", "di", " ", "MC", " ", "Unibo", " ", "anno", " ", 
     RowBox[{"24", "/", "25"}]}]}], "*)"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{":", 
    RowBox[{"Package", " ", "Version"}], ":", "0.2"}], "*)"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{":", "History", ":", 
    RowBox[{"last", " ", "modified", " ", 
     RowBox[{
      RowBox[{"11", "/", "4"}], "/", "2025"}]}]}], "*)"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Copyright", ":", 
    RowBox[{
     RowBox[{"\[Copyright]", " ", "2025", " ", "Gruppo", " ", "10"}], " ", "-",
      " ", 
     RowBox[{"Trivia", " ", "Mastermind"}]}]}], "*)"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{":", "License", ":", 
    RowBox[{"MIT", " ", "License"}]}], "*)"}], "\n", 
  RowBox[{"(*", 
   RowBox[{
    RowBox[{
     RowBox[{" ", 
      RowBox[{":", "Discussion", ":", 
       RowBox[{"Funzionalit\[AGrave]", " ", 
        RowBox[{"obbligatorie", ":", "\n", "\t", 
         RowBox[{
          RowBox[{"-", " ", "Seed"}], " ", "da", " ", "chiedere", " ", 
          "all"}]}]}]}], "\[CloseCurlyQuote]"}], "utente", " ", "per", " ", 
     RowBox[{"(", "ri", ")"}], "generare", " ", "un", " ", "esercizio"}], 
    "\n", "\t", "-", " ", 
    RowBox[{"Genera", " ", "Esercizio"}], "\n", "\t", "-", " ", 
    RowBox[{"Verifica", " ", "Risultato"}], "\n", "\t", "-", " ", 
    RowBox[{"Mostra", " ", "Soluzione"}], "\n", "\t", "-", " ", "Pulisci"}], 
   "\n", "*)"}]}]], "Code",
 CellChangeTimes->{{3.952430943787079*^9, 3.9524309706555767`*^9}, {
  3.95243102476952*^9, 3.952431126402914*^9}, {3.9524311773127136`*^9, 
  3.952431192947651*^9}, {3.9524312602691555`*^9, 3.9524313363628387`*^9}, {
  3.9524313670669403`*^9, 3.952431468764738*^9}, {3.952435382438404*^9, 
  3.9524354094111385`*^9}, {3.952597541372204*^9, 3.952597643812336*^9}, {
  3.9525976762666073`*^9, 3.9525978352464848`*^9}, {3.95335173194874*^9, 
  3.9533518148572063`*^9}},
 CellLabel->"In[15]:=",ExpressionUUID->"54a13669-0a28-0749-ab78-85d9e09ebab3"],

Cell[BoxData[{
 RowBox[{
  RowBox[{"BeginPackage", "[", "\"\<PackProg`\>\"", "]"}], ";"}], "\n", 
 RowBox[{
  RowBox[{"ClearAll", "[", "\"\<PackProg`*\>\"", "]"}], ";"}]}], "Code",
 CellChangeTimes->{{3.9533521689546223`*^9, 3.9533521851605873`*^9}, {
  3.953352421105463*^9, 3.9533524451926785`*^9}, {3.9533530257335606`*^9, 
  3.9533530282654266`*^9}, {3.9533532232002354`*^9, 3.9533532310818577`*^9}},
 CellLabel->"In[16]:=",ExpressionUUID->"5c43ff9d-5c40-8345-a784-f8d0c429ec58"],

Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{
   "USAGES", " ", "DI", " ", "FUNZIONI", " ", "CHIAMATE", " ", 
    "ESPLICITAMENTE", " ", "NEL", " ", "NOTEBOOK"}], " ", "*)"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{
     RowBox[{"ES", ".", " ", 
      RowBox[{"f", "::", "usage"}]}], "=", " ", "\"\<text\>\""}], ";"}], " ", 
   "*)"}], "\n", 
  RowBox[{
   RowBox[{
    RowBox[{"avviaSchermataDiGioco", "::", "usage"}], "=", "\"\<aaaaaa\>\""}],
    ";"}]}]], "Code",
 CellChangeTimes->{{3.953352178751608*^9, 3.9533521809454403`*^9}, {
  3.9533524158330536`*^9, 3.953352418661087*^9}},
 CellLabel->"In[18]:=",ExpressionUUID->"4ad54fb1-2af9-2c45-9a5f-e7157c9f2b9d"],

Cell[BoxData[{
 RowBox[{
  RowBox[{
   RowBox[{"Begin", "[", "\"\<`Private`\>\"", "]"}], ";"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{
    "Ricorda", " ", "di", " ", "documentare", " ", "ogni", " ", "riga", " ", 
     "di", " ", 
     RowBox[{"codice", ":", " ", "funzionalit\[AGrave]"}]}], ",", "\n", 
    RowBox[{"variabili", " ", "di", " ", "input"}], ",", " ", 
    RowBox[{"variabili", " ", "di", " ", "lavoro"}], ",", " ", 
    RowBox[{"variabili", " ", "di", " ", "output"}], ",", " ", 
    RowBox[{"spiegazione", " ", "dei", " ", "singoli", " ", "passaggi"}]}], 
   " ", "*)"}], "\n", "\n", "\n", 
  RowBox[{"(*", " ", "aaaaaa", " ", "*)"}]}], "\n", 
 RowBox[{
  RowBox[{
   RowBox[{"avviaSchermataDiGioco", "[", "]"}], " ", ":=", " ", 
   RowBox[{"Module", "[", "\n", "  ", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{
       RowBox[{"screenWidth", " ", "=", " ", "1920"}], ",", " ", 
       RowBox[{"screenHeight", " ", "=", " ", "1080"}], ",", " ", 
       "mainWindow", ",", " ", "fontScale", ",", " ", "\n", "   ", 
       "content"}], "}"}], ",", "\n", "  ", "\n", "  ", 
     RowBox[{"(*", " ", 
      RowBox[{
      "1.", " ", "Metodo", " ", "affidabile", " ", "per", " ", "ottenere", " ",
        "le", " ", "dimensioni", " ", "dello", " ", "schermo"}], " ", "*)"}], 
     "\n", "  ", 
     RowBox[{
      RowBox[{"Quiet", " ", "@", " ", 
       RowBox[{"Check", "[", "\n", "    ", 
        RowBox[{
         RowBox[{
          RowBox[{"{", 
           RowBox[{"screenWidth", ",", " ", "screenHeight"}], "}"}], " ", "=",
           " ", "\n", "     ", 
          RowBox[{"FrontEndExecute", " ", "@", " ", 
           RowBox[{"FrontEnd`Value", "[", 
            RowBox[{"FE`getScreenSize", "[", "]"}], "]"}]}]}], ",", "\n", 
         "    ", 
         RowBox[{
          RowBox[{"{", 
           RowBox[{"screenWidth", ",", " ", "screenHeight"}], "}"}], " ", "=",
           " ", 
          RowBox[{"{", 
           RowBox[{"1920", ",", " ", "1080"}], "}"}]}]}], "\n", "  ", "]"}]}],
       ";", "\n", "  ", "\n", "  ", 
      RowBox[{"(*", " ", 
       RowBox[{
       "2.", " ", "Calcolo", " ", "dimensione", " ", "font", " ", 
        "adattiva"}], " ", "*)"}], "\n", "  ", 
      RowBox[{"fontScale", " ", "=", " ", 
       RowBox[{
        RowBox[{"Min", "[", 
         RowBox[{"screenWidth", ",", " ", "screenHeight"}], "]"}], "/", 
        "20"}]}], ";", "\n", "  ", "\n", "  ", 
      RowBox[{"(*", " ", 
       RowBox[{
       "3.", " ", "Creazione", " ", "contenuto", " ", "con", " ", "pulsante", 
        " ", "di", " ", "chiusura"}], " ", "*)"}], "\n", "  ", 
      RowBox[{"content", " ", "=", " ", 
       RowBox[{"Column", "[", 
        RowBox[{
         RowBox[{"{", "\n", "     ", 
          RowBox[{
           RowBox[{"Spacer", "[", "1", "]"}], ",", "\n", "     ", 
           RowBox[{"Style", "[", 
            RowBox[{"\"\<MASTERMIND GIOCO\>\"", ",", " ", "\n", "      ", 
             RowBox[{"FontSize", " ", "->", " ", "fontScale"}], ",", " ", 
             "\n", "      ", 
             RowBox[{"FontWeight", " ", "->", " ", "Bold"}], ",", " ", "\n", 
             "      ", 
             RowBox[{"FontColor", " ", "->", " ", "White"}], ",", "\n", 
             "      ", 
             RowBox[{"FontFamily", " ", "->", " ", "\"\<Impact\>\""}]}], 
            "]"}], ",", "\n", "     ", 
           RowBox[{"Spacer", "[", "1", "]"}], ",", "\n", "     ", 
           RowBox[{"Button", "[", 
            RowBox[{"\"\<ESCI\>\"", ",", " ", "\n", "      ", 
             RowBox[{"NotebookClose", "[", 
              RowBox[{"EvaluationNotebook", "[", "]"}], "]"}], ",", " ", "\n",
              "      ", 
             RowBox[{"Background", " ", "->", " ", "Red"}], ",", " ", "\n", 
             "      ", 
             RowBox[{"BaseStyle", " ", "->", " ", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{"FontSize", " ", "->", " ", 
                 RowBox[{"fontScale", "/", "3"}]}], ",", " ", 
                RowBox[{"FontColor", " ", "->", " ", "White"}], ",", " ", 
                "Bold"}], "}"}]}], ",", "\n", "      ", 
             RowBox[{"ImageSize", " ", "->", " ", 
              RowBox[{"{", 
               RowBox[{"Automatic", ",", " ", 
                RowBox[{"fontScale", "/", "2"}]}], "}"}]}]}], "\n", "     ", 
            "]"}], ",", "\n", "     ", 
           RowBox[{"Spacer", "[", "1", "]"}], ",", "\n", "     ", 
           RowBox[{"Panel", "[", "\[IndentingNewLine]", "\t\t", 
            RowBox[{
             RowBox[{"Column", "[", 
              RowBox[{"{", "\[IndentingNewLine]", "\t\t\t", 
               RowBox[{
                RowBox[{"Style", "[", 
                 RowBox[{"\"\<Mastermind Game\>\"", ",", "Bold", ",", "18", ",", 
                  RowBox[{"FontFamily", "->", "\"\<Arial\>\""}]}], "]"}], ",",
                 "\[IndentingNewLine]", "\t\t\t", 
                RowBox[{"Spacer", "[", "10", "]"}], ",", 
                "\[IndentingNewLine]", "\t\t\t", 
                RowBox[{"Column", "[", 
                 RowBox[{"{", "\[IndentingNewLine]", "\t\t\t\t", 
                  RowBox[{
                   RowBox[{"Row", "[", 
                    RowBox[{
                    RowBox[{"{", "\n", "\t\t\t\t\t", 
                    RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{"\"\<number of items\>\"", ",", "10"}], "]"}], ",",
                     "\[IndentingNewLine]", "\t\t\t\t\t", 
                    RowBox[{"Spacer", "[", "10", "]"}], ",", 
                    "\[IndentingNewLine]", "\t\t\t\t\t", 
                    RowBox[{"RadioButtonBar", "[", 
                    RowBox[{
                    RowBox[{"Dynamic", "[", "numPegs", "]"}], ",", 
                    RowBox[{"{", 
                    RowBox[{"2", ",", "3", ",", "4", ",", "5", ",", "6"}], 
                    "}"}], ",", 
                    RowBox[{"Appearance", "->", "\"\<Horizontal\>\""}]}], 
                    "]"}]}], "\[IndentingNewLine]", "\t\t\t\t\t", "}"}], ",", 
                    "\[IndentingNewLine]", "\t\t\t\t\t", 
                    RowBox[{"Alignment", "->", "Center"}]}], 
                    "\[IndentingNewLine]", "\t\t\t\t", "]"}], ",", 
                   "\[IndentingNewLine]", "\t\t\t\t", 
                   RowBox[{"Spacer", "[", "20", "]"}], ",", 
                   "\[IndentingNewLine]", "\t\t\t\t", 
                   RowBox[{"Row", "[", 
                    RowBox[{
                    RowBox[{"{", "\[IndentingNewLine]", "\t\t\t\t\t", 
                    RowBox[{
                    RowBox[{"Style", "[", 
                    RowBox[{"\"\<number of colors\>\"", ",", "10"}], "]"}], ",",
                     "\[IndentingNewLine]", "\t\t\t\t\t", 
                    RowBox[{"Spacer", "[", "10", "]"}], ",", 
                    "\[IndentingNewLine]", "\t\t\t\t\t", 
                    RowBox[{"RadioButtonBar", "[", 
                    RowBox[{
                    RowBox[{"Dynamic", "[", "numColors", "]"}], ",", 
                    RowBox[{"{", 
                    RowBox[{
                    "2", ",", "3", ",", "4", ",", "5", ",", "6", ",", "7", ",",
                     "8"}], "}"}], ",", 
                    RowBox[{"Appearance", "->", "\"\<Horizontal\>\""}]}], 
                    "]"}]}], "\[IndentingNewLine]", "\t\t\t\t", "}"}], ",", 
                    "\[IndentingNewLine]", "\t\t\t\t", 
                    RowBox[{"Alignment", "->", "Center"}]}], 
                    "\[IndentingNewLine]", "\t\t\t\t", "]"}]}], 
                  "\[IndentingNewLine]", "\t\t\t", "}"}], "]"}], ",", 
                "\[IndentingNewLine]", 
                RowBox[{"Spacer", "[", "10", "]"}], ",", 
                "\[IndentingNewLine]", 
                RowBox[{"Row", "[", 
                 RowBox[{"{", "\[IndentingNewLine]", 
                  RowBox[{
                   RowBox[{"Button", "[", 
                    RowBox[{"\"\<replay same game\>\"", ",", "Null", ",", 
                    RowBox[{"ImageSize", "->", "120"}]}], "]"}], ",", 
                   "\[IndentingNewLine]", 
                   RowBox[{"Spacer", "[", "10", "]"}], ",", 
                   "\[IndentingNewLine]", 
                   RowBox[{"Button", "[", 
                    RowBox[{"\"\<new game\>\"", ",", "Null", ",", 
                    RowBox[{"ImageSize", "->", "120"}]}], "]"}], ",", 
                   "\[IndentingNewLine]", 
                   RowBox[{"Spacer", "[", "10", "]"}], ",", 
                   "\[IndentingNewLine]", 
                   RowBox[{"Button", "[", 
                    RowBox[{"\"\<reveal answer\>\"", ",", "Null", ",", 
                    RowBox[{"ImageSize", "->", "120"}]}], "]"}]}], 
                  "\[IndentingNewLine]", "}"}], "]"}]}], 
               "\[IndentingNewLine]", "}"}], "]"}], ",", 
             RowBox[{"ImageSize", "->", "400"}], ",", " ", 
             RowBox[{"Background", "->", "White"}]}], "]"}]}], "\n", "\n", 
          "  ", "}"}], ",", " ", "Center"}], "]"}]}], ";", "\n", "  ", "\n", 
      "  ", 
      RowBox[{"(*", " ", 
       RowBox[{"4.", " ", "Creazione", " ", "finestra", " ", "principale"}], 
       " ", "*)"}], "\n", "  ", 
      RowBox[{"mainWindow", " ", "=", " ", 
       RowBox[{"NotebookPut", " ", "@", " ", 
        RowBox[{"Notebook", "[", 
         RowBox[{
          RowBox[{"{", "\n", "     ", 
           RowBox[{"Cell", "[", 
            RowBox[{"BoxData", " ", "@", " ", 
             RowBox[{"ToBoxes", " ", "@", " ", 
              RowBox[{"Panel", "[", "\n", "        ", 
               RowBox[{"content", ",", "\n", "        ", 
                RowBox[{"Background", " ", "->", " ", "Black"}], ",", "\n", 
                "        ", 
                RowBox[{"ImageSize", " ", "->", " ", 
                 RowBox[{"{", 
                  RowBox[{"screenWidth", ",", " ", "screenHeight"}], 
                  "}"}]}]}], "\n", "     ", "]"}]}]}], "]"}], "\n", "   ", 
           "}"}], ",", "\n", "   ", 
          RowBox[{"(*", " ", 
           RowBox[{"Propriet\[AGrave]", " ", "finestra"}], " ", "*)"}], "\n", 
          "   ", 
          RowBox[{"WindowSize", " ", "->", " ", "Full"}], ",", "\n", "   ", 
          RowBox[{"WindowFrame", " ", "->", " ", "\"\<Frameless\>\""}], ",", 
          "\n", "   ", 
          RowBox[{"WindowElements", " ", "->", " ", 
           RowBox[{"{", "}"}]}], ",", "\n", "   ", 
          RowBox[{"WindowTitle", " ", "->", " ", "None"}], ",", "\n", "   ", 
          RowBox[{"Background", " ", "->", " ", "Black"}], ",", "\n", "   ", 
          RowBox[{"Editable", " ", "->", " ", "False"}], ",", "\n", "   ", 
          "\n", "   ", 
          RowBox[{"(*", " ", 
           RowBox[{"Gestione", " ", "eventi"}], " ", "*)"}], "\n", "   ", 
          RowBox[{"NotebookEventActions", " ", "->", " ", 
           RowBox[{"{", "\n", "     ", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{"\"\<KeyDown\>\"", ",", " ", "\"\<Escape\>\""}], "}"}], 
             " ", ":>", " ", 
             RowBox[{"NotebookClose", "[", 
              RowBox[{"EvaluationNotebook", "[", "]"}], "]"}]}], "\n", "   ", 
            "}"}]}]}], "\n", "  ", "]"}]}]}], ";", "\n", "  ", "\n", "  ", 
      RowBox[{"(*", " ", 
       RowBox[{
       "5.", " ", "Forza", " ", "focus", " ", "sulla", " ", "finestra"}], " ",
        "*)"}], "\n", "  ", 
      RowBox[{"SelectionMove", "[", 
       RowBox[{"mainWindow", ",", " ", "All", ",", " ", "Notebook"}], "]"}], ";",
       "\n", "  ", 
      RowBox[{"FrontEndExecute", " ", "@", " ", 
       RowBox[{"FrontEndToken", "[", 
        RowBox[{"mainWindow", ",", " ", "\"\<MoveNext\>\""}], "]"}]}], ";", 
      "\n", "  ", "\n", "  ", "mainWindow"}]}], "\n", "]"}]}], "\n", "\n"}], "\n", 
 RowBox[{
  RowBox[{"End", "[", "]"}], ";"}]}], "Code",
 CellChangeTimes->{{3.9524309881178493`*^9, 3.952430994722597*^9}, {
   3.9524314860072803`*^9, 3.9524314871068764`*^9}, {3.9524315989477177`*^9, 
   3.9524315993728943`*^9}, {3.952435168839573*^9, 3.9524351753274345`*^9}, 
   3.952435215435707*^9, {3.952597056211479*^9, 3.952597094189913*^9}, {
   3.9525971427990475`*^9, 3.9525972140306053`*^9}, {3.95302932506353*^9, 
   3.953029326759178*^9}, 3.953033854132044*^9, {3.953034015498358*^9, 
   3.9530340160679245`*^9}, {3.9530341557690964`*^9, 
   3.9530341569264736`*^9}, {3.953034547429495*^9, 3.9530345486676044`*^9}, {
   3.9530346151569653`*^9, 3.9530346245775833`*^9}, {3.9530351766067085`*^9, 
   3.953035177720009*^9}, {3.9530353166143703`*^9, 3.9530353225987225`*^9}, 
   3.9530353543367615`*^9, {3.953035389741665*^9, 3.9530353903696537`*^9}, {
   3.953036171759634*^9, 3.953036173345991*^9}, 3.9530363661578617`*^9, 
   3.9530364896237984`*^9, {3.953183380289982*^9, 3.953183382338379*^9}, {
   3.953183824294302*^9, 3.953183848963827*^9}, 3.9531840193617115`*^9, 
   3.953184508530508*^9, 3.95318468969483*^9, 3.953185224727461*^9, {
   3.9531914259960003`*^9, 3.953191430520067*^9}, {3.9531915497547417`*^9, 
   3.953191556908045*^9}, {3.9531918016128902`*^9, 3.953191801904417*^9}, 
   3.9531923216693*^9, {3.9531924830083466`*^9, 3.9531924907822075`*^9}, {
   3.953192573624197*^9, 3.9531925888184643`*^9}, 3.9531929210439034`*^9, {
   3.9531930610498905`*^9, 3.9531930723900604`*^9}, 3.953194884434084*^9, 
   3.953194948349169*^9, 3.953195119283289*^9, 3.9531951843042107`*^9, {
   3.953195755687166*^9, 3.9531957720089817`*^9}, 3.953196118913931*^9, 
   3.9531964417730904`*^9, 3.9531967521258793`*^9, {3.953306543130043*^9, 
   3.953306544178831*^9}, {3.953306574331662*^9, 3.9533065971943283`*^9}, {
   3.953306998441433*^9, 3.953307001208956*^9}, {3.9533070720417223`*^9, 
   3.953307072041967*^9}, {3.9533071861492643`*^9, 3.9533071917005253`*^9}, {
   3.9533073021734858`*^9, 3.9533074574600563`*^9}, {3.9533518272593307`*^9, 
   3.9533520339104424`*^9}, {3.953352120726385*^9, 3.9533522128529224`*^9}, {
   3.953352400802471*^9, 3.9533524106284485`*^9}},
 CellLabel->"In[19]:=",ExpressionUUID->"1a5d3d6e-9ceb-ad4e-955e-f91ea67f9168"],

Cell[BoxData[
 RowBox[{
  RowBox[{"EndPackage", "[", "]"}], ";"}]], "Code",
 CellChangeTimes->{{3.9533521476135635`*^9, 3.9533521490761833`*^9}, 
   3.953352388834532*^9},
 CellLabel->"In[22]:=",ExpressionUUID->"b908606f-2af0-b140-9b2d-8556c64dc0f4"]
},
WindowSize->{1141.2, 568.8},
WindowMargins->{{0, Automatic}, {Automatic, 0}},
DockedCells->{},
TaggingRules-><|"TryRealOnly" -> False|>,
Magnification:>1.1 Inherited,
FrontEndVersion->"14.2 for Microsoft Windows (64-bit) (December 26, 2024)",
StyleDefinitions->"Default.nb",
ExpressionUUID->"eb79ee5e-9a6e-df47-9686-4b313a6d70bf"
]
(* End of Notebook Content *)

(* Internal cache information *)
(*CellTagsOutline
CellTagsIndex->{}
*)
(*CellTagsIndex
CellTagsIndex->{}
*)
(*NotebookFileOutline
Notebook[{
Cell[554, 20, 2564, 59, 325, "Code",ExpressionUUID->"54a13669-0a28-0749-ab78-85d9e09ebab3"],
Cell[3121, 81, 483, 8, 74, "Code",ExpressionUUID->"5c43ff9d-5c40-8345-a784-f8d0c429ec58"],
Cell[3607, 91, 690, 18, 93, "Code",ExpressionUUID->"4ad54fb1-2af9-2c45-9a5f-e7157c9f2b9d"],
Cell[4300, 111, 14003, 272, 1967, "Code",ExpressionUUID->"1a5d3d6e-9ceb-ad4e-955e-f91ea67f9168"],
Cell[18306, 385, 250, 5, 55, "Code",ExpressionUUID->"b908606f-2af0-b140-9b2d-8556c64dc0f4"]
}
]
*)

