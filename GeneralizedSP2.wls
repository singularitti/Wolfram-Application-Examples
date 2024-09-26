#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"];

(*Iterative function:Applies the iterative relation to generate the next y_i*)
iter[yi_, {theta1_, theta2_, theta3_}] :=
    theta1 yi^2 + theta2 yi + theta3;

(*Generate the sequence of y_i values,starting from y_0=x*)
g[\[Theta]_, x_] :=
    FoldList[iter, x, \[Theta]]

(*Weighted summation of the y_i sequence*)
Y[\[Theta]_, c_, x_] :=
    Dot[Append[c, 1], g[\[Theta], x]]

FermiDirac[\[Theta]_, c_, x_] :=
    1 - Y[\[Theta], c, x]


\[Theta]=Map[ToExpression,MapThread[StringJoin,{Table["\[Theta]",{i,1,3*3}],Flatten[Table[ToString[i]<>ToString[j],{i,1,3},{j,1,3}]]}]]//ArrayReshape[#,{3,3}]&


(MonomialList[#,x]&)/@g[\[Theta],x]//TableForm
