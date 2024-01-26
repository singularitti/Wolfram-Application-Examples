#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

SetDirectory[NotebookDirectory[]];

<<Sij`

cubic = {{c11, c12, c12, 0, 0, 0}, {0, c11, c12, 0, 0, 0}, {0, 0, c11,
        0, 0, 0}, {0, 0, 0, c44, 0, 0}, {0, 0, 0, 0, c44, 0}, {0, 0, 0, 0, 0,
        c44}} // Symmetrise

f = ElasticEnergy[cubic]

cj = {c11, c12, c44};

(*Length[cj] == 3*)

s = S[f, cj];

s // MatrixForm


Examine[s, cj, f]
