#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

SetDirectory[NotebookDirectory[]];

<<Sij`

triclinic = {{c11, c12, c13, c14, c15, c16}, {0, c22, c23, c24, c25, 
    c26}, {0, 0, c33, c34, c35, c36}, {0, 0, 0, c44, c45, c46}, {0, 0, 0,
     0, c55, c56}, {0, 0, 0, 0, 0, c66}} // Symmetrise

f = ElasticEnergy[triclinic]

cj = {c11, c12, c13, c14, c15, c16, c22, c23, c24, c25, c26, c33, c34,
     c35, c36, c44, c45, c46, c55, c56, c66};

(*Length[cj] == 21*)

s = S[f, cj];

s // MatrixForm


Examine[s, cj, f]
