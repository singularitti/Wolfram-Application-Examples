#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

indices := {{1,1,1,1},{1,1,2,2},{1,1,3,3},{2,2,1,1},{2,2,2,2},{2,2,3,3},{3,3,1,1},{3,3,2,2},{3,3,3,3},
{2,3,2,3},{2,3,3,2},{3,2,3,2},{3,2,2,3},{1,3,1,3},{1,3,3,1},{3,1,3,1},{3,1,1,3},
{1,2,1,2},{1,2,2,1},{2,1,2,1},{2,1,1,2}};

dict = <|{1,1}->1,{2,2}->2,{3,3}->3,{2,3}->4,{3,2}->4,{1,3}->5,{3,1}->5,{1,2}->6,{2,1}->6|>;

c[i_,j_,k_,l_] := c[dict[[Key[{i,j}]]],dict[[Key[{k,l}]]]]

c[i_,j_] := {{c11,c12,c13,0,0,0},{c12,c22,c23,0,0,0},{c13,c23,c33,0,0,0},{0,0,0,c44,0,0},{0,0,0,0,c55,0},{0,0,0,0,0,c66}}[[i,j]];

qs = {qx,qy,qz};

GreenChristoffelEquations[qs_] := Table[Sum[c[\[Alpha],\[Gamma],\[Beta],\[Delta]]qs[[\[Gamma]]]qs[[\[Delta]]],{\[Gamma],1,3},{\[Delta],1,3}],{\[Alpha],1,3},{\[Beta],1,3}]


qVectorOnDirection[{ a_, b_, c_ }] := q / Norm[{ a, b, c }] { a, b, c }

generateEquationsOnDirection[{ a_, b_, c_ }] := GreenChristoffelEquations[qVectorOnDirection[{ a, b, c }]]

phaseVelocity[{ a_, b_, c_ }] := Simplify[Sqrt[Eigenvalues[generateEquationsOnDirection[{ a, b, c }]] / \[Rho]] / q, Assumptions-> q > 0]

particleDisplacementDirections[{ a_, b_, c_ }] := Eigenvectors[generateEquationsOnDirection[{ a, b, c }]]

takePositiveSolutions[{ a_, b_, c_ }] := Module[{ v, indices },
    v = phaseVelocity[{ a, b, c }];
    indices = Simplify[v // Sign, Assumptions -> c11 > 0 && c11 c22 > c12^2 && c11 c22 c33 + 2 c12 c13 c23 > c11 c23^2 + c22 c13^2 + c33 c12^2 && c44 > 0 && c55 > 0 && c66 > 0 && \[Rho] > 0 && q > 0] // Position[#, 1] & // Flatten;
    Map[Part[v, #] &, indices]
]


phaseVelocity[{0,1,1}]


Simplify[((generateEquationsOnDirection[{1,1,1}]//Eigenvalues) / \[Rho] // Sqrt) /q,Assumptions->q>0]
