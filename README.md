
<!-- README.md is generated from README.Rmd. Please edit that file -->
tst v0.1.0
==========

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/pmcharrison/tst?branch=master&svg=true)](https://ci.appveyor.com/project/pmcharrison/tst) [![Travis build status](https://travis-ci.org/pmcharrison/tst.svg?branch=master)](https://travis-ci.org/pmcharrison/tst) [![Coverage status](https://coveralls.io/repos/github/pmcharrison/tst/badge.svg)](https://coveralls.io/r/pmcharrison/tst?branch=master)

`tst` is an R package for building timestamped suffix trees (TSTs). TSTs are [suffix trees](https://en.wikipedia.org/wiki/Suffix_tree) that store timestamps for each state transition in the training sequence(s). Given a sequence of consecutive symbols, the TST can efficiently return the timepoints when this sequence occurred in the training data, the symbols that followed this sequence in the training data, and the timepoints when these continuation symbols were observed.

For an interactive demo, visit <http://shiny.pmcharrison.com/suffix-tree-demo>.

Installation
------------

    install.packages("devtools") # if you don't have devtools already installed
    devtools::install_github("pmcharrison/tst")

Example use
-----------

``` r
library(tst)

# Build and visualize a simple suffix tree
t <- new_tree()
add_seq(t, c("a", "b", "c", "a", "b"))
#> A suffix tree with 4 observed symbols (including terminals)
#>   - order bound = none
#>   - active order = 6
#>   - last symbol location = 5
plot(t)
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="100%" />

``` r

# Incrementally visualize the building of a suffix tree
t <- new_tree()
add_seq(t, c("a", "b", "c", "a", "b"), visual = TRUE)
#> Press enter to continue:
#> Press enter to continue:
#> Press enter to continue:
#> Press enter to continue:
#> Press enter to continue:
#> A suffix tree with 4 observed symbols (including terminals)
#>   - order bound = none
#>   - active order = 6
#>   - last symbol location = 5

# Search for n-grams
when_ngram(t, c("a", "b"))
#> [1] 2 5
count_ngram(t, c("a", "b"))
#> [1] 2
```
