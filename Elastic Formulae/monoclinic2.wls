#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

SetDirectory[NotebookDirectory[]];

<<Sij`

mono = {{c11, c12, c13, 0, c15, 0}, {0, c22, c23, 0, c25, 0}, {0, 0, 
    c33, 0, c35, 0}, {0, 0, 0, c44, 0, c46}, {0, 0, 0, 0, c55, 0}, {0, 0,
     0, 0, 0, c66}} // Symmetrise

f = ElasticEnergy[mono]

cj = {c11, c22, c33, c12, c13, c23, c44, c55, c66, c15, c25, c35, c46
    };

(*Length[cj] == 13*)

s = S[f, cj];

s // MatrixForm


Examine[s, cj, f]
