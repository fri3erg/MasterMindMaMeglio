(* Content-type: application/vnd.wolfram.mathematica *)

(*** Wolfram Notebook File ***)
(* http://www.wolfram.com/nb *)

(* CreatedBy='Wolfram 14.2' *)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[       154,          7]
NotebookDataLength[      5549,        148]
NotebookOptionsPosition[      5052,        132]
NotebookOutlinePosition[      5499,        149]
CellTagsIndexPosition[      5456,        146]
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
  3.9525976762666073`*^9, 
  3.9525978352464848`*^9}},ExpressionUUID->"54a13669-0a28-0749-ab78-\
85d9e09ebab3"],

Cell[BoxData[{
 RowBox[{
  RowBox[{"BeginPackage", "[", 
   RowBox[{
   "\[OpenCurlyDoubleQuote]", "PacchettoProgetto`", 
    "\[CloseCurlyDoubleQuote]"}], "]"}], "\n"}], "\n", 
 RowBox[{
  RowBox[{
   RowBox[{"f", "::", "usage"}], "=", " ", 
   RowBox[{"\[OpenCurlyDoubleQuote]", "text", "\[CloseCurlyDoubleQuote]"}]}], 
  "\n", 
  RowBox[{"(*", 
   RowBox[{
    RowBox[{" ", 
     RowBox[{
     "messo", " ", "qui", " ", "solo", " ", "se", " ", "f", " ", "e"}], 
     "\[CloseCurlyQuote]"}], " ", "chiamata", " ", "esplicitamente", " ", 
    "in", " ", 
    RowBox[{"Tutorial", ".", "nb"}]}], " ", "*)"}], "\n"}], "\n", 
 RowBox[{
  RowBox[{"Begin", "[", 
   RowBox[{
   "\[OpenCurlyDoubleQuote]", "`Private`", "\[CloseCurlyDoubleQuote]"}], 
   "]"}], "\n", 
  RowBox[{"(*", " ", "Parameters", " ", "*)"}], "\n", "\n", "\n", 
  RowBox[{"(*", 
   RowBox[{
    RowBox[{" ", 
     RowBox[{
     "documentazione", " ", "di", " ", "ogni", " ", "riga", " ", "di", " ", 
      RowBox[{"codice", " ", ":", " ", "funzionalita"}]}], 
     "\[CloseCurlyQuote]"}], ",", "\n", 
    RowBox[{"variabili", " ", "di", " ", "input"}], ",", " ", 
    RowBox[{"variabili", " ", "di", " ", "lavoro"}], ",", " ", 
    RowBox[{"variabili", " ", "di", " ", "output"}], ",", " ", 
    RowBox[{"spiegazione", " ", "dei", " ", "singoli", " ", "passaggi"}]}], 
   " ", "*)"}]}], "\n", 
 RowBox[{
  RowBox[{
   RowBox[{"f", "[", " ", "args_", " ", "]"}], " ", ":=", " ", "value"}], 
  "\n"}], "\n", 
 RowBox[{"End", "[", " ", "]"}], "\n", 
 RowBox[{"EndPackage", "[", " ", "]"}]}], "Code",
 CellChangeTimes->{{3.9524309881178493`*^9, 3.952430994722597*^9}, {
   3.9524314860072803`*^9, 3.9524314871068764`*^9}, {3.9524315989477177`*^9, 
   3.9524315993728943`*^9}, {3.952435168839573*^9, 3.9524351753274345`*^9}, 
   3.952435215435707*^9, {3.952597056211479*^9, 3.952597094189913*^9}, {
   3.9525971427990475`*^9, 
   3.9525972140306053`*^9}},ExpressionUUID->"1a5d3d6e-9ceb-ad4e-955e-\
f91ea67f9168"]
},
WindowSize->{1141.2, 568.8},
WindowMargins->{{0, Automatic}, {Automatic, 0}},
TaggingRules-><|"TryRealOnly" -> False|>,
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
Cell[554, 20, 2518, 59, 331, "Code",ExpressionUUID->"54a13669-0a28-0749-ab78-85d9e09ebab3"],
Cell[3075, 81, 1973, 49, 295, "Code",ExpressionUUID->"1a5d3d6e-9ceb-ad4e-955e-f91ea67f9168"]
}
]
*)

