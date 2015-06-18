## Test environments

* local OS X install, R 3.2.1
* ubuntu 12.04 (on travis-ci), R 3.2.0
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE regarding the MIT license:

* checking CRAN incoming feasibility ... NOTE

	Maintainer: ‘Herman De Beukelaer <Herman.DeBeukelaer@UGent.be>’

	License components with restrictions and base license permitting such:
  		MIT + file LICENSE
	File 'LICENSE':
  		YEAR: 2015
  		COPYRIGHT HOLDER: Ghent University

Only on win-builder the same NOTE also says:

* Possibly mis-spelled words in DESCRIPTION:
  metaheuristics (11:14)

This seems to be a false positive of the spell-checker on that
platform as the word 'metaheuristics' is correctly spelled.

## Downstream dependencies

There are currently no downstream dependencies for this package.
