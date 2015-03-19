## Test environments

* local OS X install, R 3.1.3
* ubuntu 12.04 (on travis-ci), R 3.1.3
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE (because this is the first submission):

* checking CRAN incoming feasibility ... NOTE

	Maintainer: ‘Herman De Beukelaer <Herman.DeBeukelaer@UGent.be>’
	New submission
	Components with restrictions and base license permitting such:
  		MIT + file LICENSE
	File 'LICENSE':
  		YEAR: 2015
  		COPYRIGHT HOLDER: Ghent University

Only on win-builder the same NOTE also says:

* Possibly mis-spelled words in DESCRIPTION:
  metaheuristics (11:14)
  
This seems to be a false positive of the spell-checker on that
platform as the word 'metaheuristics' is correctly spelled.