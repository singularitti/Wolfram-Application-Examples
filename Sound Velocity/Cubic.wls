#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

GreenChristoffelEquations[{ qx_, qy_, qz_ }] := {{
        c11 qx ^ 2 + c44(qy ^ 2 + qz ^ 2), (c12 + c44) qx qy, (c12 + c44) qx qz
    }, {
        (c12 + c44) qx qy, c11 qy ^ 2 + c44(qz ^ 2 + qx ^ 2), (c12 + c44) qy qz
    }, {
        (c12 + c44) qx qz, (c12 + c44) qy qz, c11 qz ^ 2 + c44(qx ^ 2 + qy ^ 2)
}}

qVectorOnDirection[{ a_, b_, c_ }] := q / Norm[{ a, b, c }] { a, b, c }

generateEquationsOnDirection[{ a_, b_, c_ }] := GreenChristoffelEquations[qVectorOnDirection[{ a, b, c }]]

phaseVelocity[{ a_, b_, c_ }] := Simplify[Sqrt[Eigenvalues[generateEquationsOnDirection[{ a, b, c }]] / \[Rho]] / q, Assumptions-> q > 0]

particleDisplacementDirections[{ a_, b_, c_ }] := Eigenvectors[generateEquationsOnDirection[{ a, b, c }]]

takePositiveSolutions[{ a_, b_, c_ }] := Module[{ v, indices },
    v = phaseVelocity[{ a, b, c }];
    indices = Simplify[v // Sign, Assumptions -> c11 > c12 && c11 + 2 c12 > 0 && c44 > 0 && \[Rho] > 0 && q > 0] // Position[#, 1] & // Flatten;
    Map[Part[v, #] &, indices]
]


takePositiveSolutions[{1,1,1}]
particleDisplacementDirections[{1,1,1}]


Simplify[((generateEquationsOnDirection[{1,1,1}]//Eigenvalues) / \[Rho] // Sqrt) /q,Assumptions->q>0]
