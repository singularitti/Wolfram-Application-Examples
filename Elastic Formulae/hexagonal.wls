#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

SetDirectory[NotebookDirectory[]];

<<Sij`

hex = {{c11, c12, c13, 0, 0, 0}, {0, c11, c13, 0, 0, 0}, {0, 0, c33, 
      0, 0, 0}, {0, 0, 0, c44, 0, 0}, {0, 0, 0, 0, c44, 0}, {0, 0, 0, 0, 0,
       (c11 - c12) / 2}} // Symmetrise

f = ElasticEnergy[hex]

cj = {c11, c33, c12, c13, c44};

(*Length[cj] == 5*)

s = S[f, cj];

s // MatrixForm


Examine[s, cj, f]
