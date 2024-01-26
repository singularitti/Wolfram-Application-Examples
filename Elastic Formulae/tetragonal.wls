#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

SetDirectory[NotebookDirectory[]];

<<Sij`

tet = {{c11, c12, c13, 0, 0, 0}, {0, c11, c13, 0, 0, 0}, {0, 0, c33, 
        0, 0, 0}, {0, 0, 0, c44, 0, 0}, {0, 0, 0, 0, c44, 0}, {0, 0, 0, 0, 0,
         c66}} // Symmetrise

cj = {c11, c33, c12, c13, c44, c66};

f = ElasticEnergy[tet]

(*Length[cj] == 6*)

s = S[f, cj];

s // MatrixForm


Examine[s, cj, f]
