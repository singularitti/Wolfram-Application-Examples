#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

SetDirectory[NotebookDirectory[]];

<<Sij`

tri = {{c11, c12, c13, c14, 0, 0}, {0, c11, c13, -c14, 0, 0}, {0, 0, 
        c33, 0, 0, 0}, {0, 0, 0, c44, 0, 0}, {0, 0, 0, 0, c44, c14}, {0, 0, 0,
         0, 0, (c11 - c12) / 2}} // Symmetrise

(*tri // SymmetricMatrixQ*)

cj = {c11, c33, c12, c13, c44, c14};

f = ElasticEnergy[tri]

(*Length[cj] == 6*)

s = S[f, cj];

s // MatrixForm


Examine[s, cj, f]
