# Revision history for uniqueness-periods-vector-stats

## 0.1.0.0 -- 2020-09-19

* First version. Released on an unsuspecting world.

## 0.1.1.0 -- 2020-09-19

* First version revised A. Fixed issues with not importing from GHC.Prim fabsFloat# for GHC lower than 8.2* versions. Removed functions that used Neumaier summation because
they do not have the intended behaviour.

## 0.1.2.0 -- 2020-09-19

* First version revised B. Removed the functions that compute mean with dispersion using unlifted types, because of numeric inaccuracy.

## 0.2.0.0 -- 2020-12-05

* Second version. Some documentation improvements because of the similar code snippets found in the various
previous publications by the other authors. Added new functions and rewrite the existing ones.

## 0.2.1.0 -- 2020-12-05

* Second version revised A. Fixed issue with being wrongly defined for the meanD and meanF functions.

