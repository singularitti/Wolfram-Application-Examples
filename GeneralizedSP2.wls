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
    
(*Target function*)
targetFermiDirac[\[Beta]_, \[Mu]_, x_] :=
    1 / (1 + Exp[\[Beta](x - \[Mu])]);


\[Theta] = {{3.4199, -0.916353, 0.638295}, {-0.877837, 4.54196, 1.50423
    }, {0.111267, 0.40718, 0.644496}, {-0.0703375, 2.35554, 0.981319}};

c = {0.181909, 0.047729, -2.71051, 0.355542};

Plot[{FermiDirac[\[Theta], c, x], targetFermiDirac[9.423, 0.568, x]}, {x, 0, 1}, PlotLabels ->
     {Callout["approximant", {Scaled[0.25], Above}], Callout["target", {Scaled[
    0.5], Below}]}]


\[Theta] = {{2.251, -0.977224, -0.58401}, {-60.9648, 30.0563, -27.4474
    }, {0.00133936, 0.85038, -0.118016}, {-0.0117643, 2.78806, -0.439733}
    };

c = {0.0602414, 0.0303926, -3.19167, 0.788064};

Plot[{FermiDirac[\[Theta], c, x], targetFermiDirac[20, 0.568, x]}, {x, 0, 1}, PlotLabels ->
     {Callout["approximant", {Scaled[0.25], Above}], Callout["target", {Scaled[
    0.5], Below}]}]
