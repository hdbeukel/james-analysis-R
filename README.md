JAMES Analysis R Package
========================

[![Build Status](https://img.shields.io/travis/hdbeukel/james-analysis-R.svg?style=flat)](https://travis-ci.org/hdbeukel/james-analysis-R)

The JAMES analysis R package is part of the [JAMES framework][james-github]. This R package is used to analyze and visualize results obtained using the analysis tools from the [extensions module][james-extensions].

Install
=======

The package is considered for inclusion on CRAN. Currently, it can directly be installed from GitHub using the `devtools` library:

```
library(devtools)
install_github("hdbeukel/james-analysis-R")
```

Documentation
=============

Use `data <- readJAMES("path/to/file")` the load a JSON file in R that was created using the analysis tools from the JAMES extensions module (in Java). An object of class `james` is returned. Run `summary(data)` to summarize the results, or `mergeJAMES` and `reduceJAMES` to manipulate them.

To extract data, the following functions are provided:

 - `getProblems`
 - `getSearches`
 - `getSearchRuns`
 - `getNumSearchRuns`
 - `getBestSolutions`
 - `getBestSolutionValues`
 - `getConvergenceTimes`

To visualize the results, use:

 - `plotConvergence`
 - `boxplot`

Detailed documentation is provided for each function and can be accessed by typing `?function`, as usual. To view the help file for the box plots, type `?boxplot.james` (this is an implementation of the S3 method `boxplot` from the standard `graphics` package for class `james`).

Examples
=============

Some examples are included in the help files of the available functions. More extensive examples of how to analyze the results will soon be provided at the [website][james-website].

License and copyright
=====================

The JAMES analysis R package is licensed under the MIT License, see http://www.r-project.org/Licenses/MIT. Copyright information is stated in the LICENSE file.

User forum
==========

Users may post questions on the [forum][james-forum]. Instructions for participating without a Google account are available at the [website][james-contact].

Developers
==========

The JAMES framework is developed and maintained by

 - Herman De Beukelaer (Herman.DeBeukelaer@UGent.be)
 
Please use the forum instead of directly mailing the developers whenever possible, so that others may benefit from or contribute to the discussion as well.
 
Changes
=======

A list of changes is provided in the NEWS file.


[james-github]:     https://github.com/hdbeukel/james
[james-extensions]: https://github.com/hdbeukel/james-extensions
[james-website]:    http://www.jamesframework.org
[james-forum]:      https://groups.google.com/forum/#!forum/james-users
[james-contact]:    http://www.jamesframework.org/contact/
