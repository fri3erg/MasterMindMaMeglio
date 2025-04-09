(* Content-type: application/vnd.wolfram.mathematica *)

(*** Wolfram Notebook File ***)
(* http://www.wolfram.com/nb *)

(* CreatedBy='Wolfram 14.2' *)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[       154,          7]
NotebookDataLength[     12561,        278]
NotebookOptionsPosition[     11733,        257]
NotebookOutlinePosition[     12227,        276]
CellTagsIndexPosition[     12184,        273]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{
Cell[BoxData[
 RowBox[{
  RowBox[{"(*", " ", 
   RowBox[{":", "Title", ":", "NomeProgetto"}], "*)"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Context", ":", "NomeContesto`"}], "*)"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Author", ":", 
    RowBox[{
     RowBox[{"Gruppo", " ", "X"}], " ", "-", " ", "NomeGruppo"}]}], "*)"}], 
  "\n", 
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{":", "Summary", ":", 
     RowBox[{"Package", " ", "per", " ", "\"\<NomeProgetto\>\""}]}], ",", " ", 
    RowBox[{"progetto", " ", "su", " ", "aaaaaa"}]}], "*)"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{":", 
    RowBox[{"Package", " ", "Version"}], ":", "0.1"}], "*)"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{":", "History", ":", 
    RowBox[{"last", " ", "modified", " ", 
     RowBox[{
      RowBox[{"2", "/", "4"}], "/", "2025"}]}]}], "*)"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Copyright", ":", 
    RowBox[{
     RowBox[{"\[Copyright]", " ", "2025", " ", "Gruppo", " ", "X"}], " ", "-",
      " ", "NomeGruppo"}]}], "*)"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{":", "License", ":", 
    RowBox[{"MIT", " ", "License"}]}], "*)"}], "\n", 
  RowBox[{"(*", 
   RowBox[{
    RowBox[{
     RowBox[{" ", 
      RowBox[{":", "Discussion", ":", 
       RowBox[{"aaaaaa", "\n", "\t", "Funzionalit\[AGrave]", " ", 
        RowBox[{"obbligatorie", ":", "\n", "\t", 
         RowBox[{
          RowBox[{"-", " ", "Seed"}], " ", "da", " ", "chiedere", " ", 
          "all"}]}]}]}], "\[CloseCurlyQuote]"}], "utente", " ", "per", " ", 
     RowBox[{"(", "ri", ")"}], "generare", " ", "un", " ", "esercizio"}], 
    "\n", "\t", "-", " ", 
    RowBox[{"Genera", " ", "Esercizio"}], "\n", "\t", "-", " ", 
    RowBox[{"Verifica", " ", "Risultato"}], "\n", "\t", "-", " ", 
    RowBox[{"Mostra", " ", "Soluzione"}], "\n", "\t", "-", " ", "Pulisci"}], 
   "\n", "*)"}], "\n", 
  RowBox[{"(*", " ", 
   RowBox[{":", "Warning", ":", 
    RowBox[{"DOCUMENTATE", " ", "TUTTO", " ", "il", " ", 
     RowBox[{"codice", "!"}]}]}], "*)"}]}]], "Code",
 CellChangeTimes->{{3.952430943787079*^9, 3.9524309706555767`*^9}, {
  3.95243102476952*^9, 3.952431126402914*^9}, {3.9524311773127136`*^9, 
  3.952431192947651*^9}, {3.9524312602691555`*^9, 3.9524313363628387`*^9}, {
  3.9524313670669403`*^9, 3.952431468764738*^9}, {3.952435382438404*^9, 
  3.9524354094111385`*^9}, {3.952597541372204*^9, 3.952597643812336*^9}, {
  3.9525976762666073`*^9, 3.9525978352464848`*^9}},
 CellLabel->"In[1]:=",ExpressionUUID->"54a13669-0a28-0749-ab78-85d9e09ebab3"],

Cell[BoxData[
 RowBox[{
  RowBox[{"avviaSchermataDiGioco", "[", "]"}], " ", ":=", " ", 
  RowBox[{"Module", "[", "\n", "  ", 
   RowBox[{
    RowBox[{"{", 
     RowBox[{
      RowBox[{"screenWidth", " ", "=", " ", "1920"}], ",", " ", 
      RowBox[{"screenHeight", " ", "=", " ", "1080"}], ",", " ", "mainWindow",
       ",", " ", "fontScale", ",", " ", "\n", "   ", "content"}], "}"}], ",", 
    "\n", "  ", "\n", "  ", 
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
      "2.", " ", "Calcolo", " ", "dimensione", " ", "font", " ", "adattiva"}],
       " ", "*)"}], "\n", "  ", 
     RowBox[{"fontScale", " ", "=", " ", 
      RowBox[{
       RowBox[{"Min", "[", 
        RowBox[{"screenWidth", ",", " ", "screenHeight"}], "]"}], "/", 
       "20"}]}], ";", "\n", "  ", "\n", "  ", 
     RowBox[{"(*", " ", 
      RowBox[{
      "3.", " ", "Creazione", " ", "contenuto", " ", "con", " ", "pulsante", " ",
        "di", " ", "chiusura"}], " ", "*)"}], "\n", "  ", 
     RowBox[{"content", " ", "=", " ", 
      RowBox[{"Column", "[", 
       RowBox[{
        RowBox[{"{", "\n", "     ", 
         RowBox[{
          RowBox[{"Spacer", "[", "1", "]"}], ",", "\n", "     ", 
          RowBox[{"Style", "[", 
           RowBox[{"\"\<MASTERMIND GIOCO\>\"", ",", " ", "\n", "      ", 
            RowBox[{"FontSize", " ", "->", " ", "fontScale"}], ",", " ", "\n",
             "      ", 
            RowBox[{"FontWeight", " ", "->", " ", "Bold"}], ",", " ", "\n", 
            "      ", 
            RowBox[{"FontColor", " ", "->", " ", "White"}], ",", "\n", 
            "      ", 
            RowBox[{"FontFamily", " ", "->", " ", "\"\<Impact\>\""}]}], "]"}],
           ",", "\n", "     ", 
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
          RowBox[{"Spacer", "[", "1", "]"}]}], "\n", "  ", "}"}], ",", " ", 
        "Center"}], "]"}]}], ";", "\n", "  ", "\n", "  ", 
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
                 RowBox[{"screenWidth", ",", " ", "screenHeight"}], "}"}]}]}],
               "\n", "     ", "]"}]}]}], "]"}], "\n", "   ", "}"}], ",", "\n",
          "   ", 
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
     "\n", "  ", "\n", "  ", "mainWindow"}]}], "\n", "]"}]}]], "Code",
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
   3.9531964417730904`*^9, 3.9531967521258793`*^9},
 CellLabel->"In[3]:=",ExpressionUUID->"1a5d3d6e-9ceb-ad4e-955e-f91ea67f9168"],

Cell[BoxData[""], "Input",
 CellChangeTimes->{{3.95303434394112*^9, 3.953034344992651*^9}},
 CellLabel->"In[5]:=",ExpressionUUID->"a87addd3-e25c-f646-af02-0965616d4aa1"],

Cell[BoxData[""], "Input",
 CellChangeTimes->{{3.9530343373537827`*^9, 3.9530343386530857`*^9}},
 CellLabel->"In[6]:=",ExpressionUUID->"221ab672-534e-1e46-b4bb-1c643486835f"],

Cell[BoxData[""], "Input",
 CellChangeTimes->{{3.9530343333282413`*^9, 3.953034334847044*^9}},
 CellLabel->"In[7]:=",ExpressionUUID->"c738afb7-f3bc-f041-9de5-0bb99e11a23a"]
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
Cell[554, 20, 2536, 58, 364, "Code",ExpressionUUID->"54a13669-0a28-0749-ab78-85d9e09ebab3"],
Cell[3093, 80, 8112, 163, 1194, "Code",ExpressionUUID->"1a5d3d6e-9ceb-ad4e-955e-f91ea67f9168"],
Cell[11208, 245, 169, 2, 31, "Input",ExpressionUUID->"a87addd3-e25c-f646-af02-0965616d4aa1"],
Cell[11380, 249, 174, 2, 31, "Input",ExpressionUUID->"221ab672-534e-1e46-b4bb-1c643486835f"],
Cell[11557, 253, 172, 2, 31, "Input",ExpressionUUID->"c738afb7-f3bc-f041-9de5-0bb99e11a23a"]
}
]
*)

