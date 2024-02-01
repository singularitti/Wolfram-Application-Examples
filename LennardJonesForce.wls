#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear[xi, yi, zi, xj, yj, zj, epsilon, sigma, rij]


rij[xi_, yi_, zi_, xj_, yj_, zj_] :=
  Sqrt[(xi - xj) ^ 2 + (yi - yj) ^ 2 + (zi - zj) ^ 2]


u[xi_, yi_, zi_, xj_, yj_, zj_, epsilon_, sigma_] :=
  4 epsilon ((sigma / rij[xi, yi, zi, xj, yj, zj]) ^ 12 - (sigma / rij[
    xi, yi, zi, xj, yj, zj]) ^ 6)


Fx[xi_, yi_, zi_, xj_, yj_, zj_, epsilon_, sigma_] :=
  -D[u[xi, yi, zi, xj, yj, zj, epsilon, sigma], xi]

Fy[xi_, yi_, zi_, xj_, yj_, zj_, epsilon_, sigma_] :=
  -D[u[xi, yi, zi, xj, yj, zj, epsilon, sigma], yi]

Fz[xi_, yi_, zi_, xj_, yj_, zj_, epsilon_, sigma_] :=
  -D[u[xi, yi, zi, xj, yj, zj, epsilon, sigma], zi]


Simplify[Fx[xi, yi, zi, xj, yj, zj, epsilon, sigma]]

Simplify[Fy[xi, yi, zi, xj, yj, zj, epsilon, sigma]]

Simplify[Fz[xi, yi, zi, xj, yj, zj, epsilon, sigma]]


Ftest[xi_, yi_, zi_, xj_, yj_, zj_, epsilon_, sigma_] :=
  (48 epsilon / sigma^2) ({xi, yi, zi} - {xj, yj, zj}) * ((sigma / Sqrt[
    (xi - xj) ^ 2 + (yi - yj) ^ 2 + (zi - zj) ^ 2]) ^ 14 - 1/2 (sigma / Sqrt[
    (xi - xj) ^ 2 + (yi - yj) ^ 2 + (zi - zj) ^ 2]) ^ 8)


Simplify[Fx[xi, yi, zi, xj, yj, zj, epsilon, sigma] - Ftest[xi, yi,
  zi, xj, yj, zj, epsilon, sigma][[1]]]

Simplify[Fy[xi, yi, zi, xj, yj, zj, epsilon, sigma] - Ftest[xi, yi,
  zi, xj, yj, zj, epsilon, sigma][[2]]]

Simplify[Fz[xi, yi, zi, xj, yj, zj, epsilon, sigma] - Ftest[xi, yi,
  zi, xj, yj, zj, epsilon, sigma][[3]]]
