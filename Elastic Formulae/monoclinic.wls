#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

SetDirectory[NotebookDirectory[]];

<<Sij`

mono = {{c11, c12, c13, 0, 0, c16}, {0, c22, c23, 0, 0, c26}, {0, 0, 
    c33, 0, 0, c36}, {0, 0, 0, c44, c45, 0}, {0, 0, 0, 0, c55, 0}, {0, 0,
     0, 0, 0, c66}} // Symmetrise

f = ElasticEnergy[mono]

cj = {c11, c22, c33, c12, c13, c23, c44, c55, c66, c16, c26, c36, c45
    };

(*Length[cj] == 13*)

s = S[f, cj];

s // MatrixForm


Examine[s, cj, f]
