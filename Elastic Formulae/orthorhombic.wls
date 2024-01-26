#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

SetDirectory[NotebookDirectory[]];

<<Sij`

orth = {{c11, c12, c13, 0, 0, 0}, {0, c22, c23, 0, 0, 0}, {0, 0, c33,
     0, 0, 0}, {0, 0, 0, c44, 0, 0}, {0, 0, 0, 0, c55, 0}, {0, 0, 0, 0, 0,
     c66}} // Symmetrise

cj = {c11, c22, c33, c12, c13, c23, c44, c55, c66};

f = ElasticEnergy[orth]

(*Length[cj] == 9*)

s = S[f, cj];

s // MatrixForm


Examine[s, cj, f]
