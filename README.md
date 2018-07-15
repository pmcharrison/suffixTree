# suffixTree - an R package for building suffix trees

`suffixTree` is an R package for building suffix trees. For an interactive demo, visit http://shiny.pmcharrison.com/suffix-tree-demo.

## About

Suffix trees are a useful way to represent sequences of symbolic data (https://en.wikipedia.org/wiki/Suffix_tree).
They efficiently support a number of text-processing routines, such as substring or n-gram matching,
and form the basis of a number of several sequence prediction algorithms,
such as Prediction by Partial Matching (https://en.wikipedia.org/wiki/Prediction_by_partial_matching).

This package provides a naive suffix tree building algorithm,
which theoretically takes quadratic time with respect to the input sequence.
More efficient algorithms are possible, 
e.g. Ukkonen's (1995) algorithm (https://en.wikipedia.org/wiki/Ukkonen%27s_algorithm).
It is currently implemented in base R, so is relatively slow.
The code is therefore best suited to research purposes with short sequences/low order bounds.

## Installation

```
install.packages("devtools") # if you don't have devtools already installed
devtools::install_github("pmcharrison/suffixTree")
```

## Example use

```
library(suffixTree)

# Build and visualize a simple suffix tree
t <- new_tree()
add_seq(t, c("a", "b", "c", "a", "b"))
plot(t)

# Incrementally visualize the building of a suffix tree
t <- new_tree()
add_seq(t, c("a", "b", "c", "a", "b"), visual = TRUE)

# Search for n-grams
when_ngram(t, c("a", "b"))
count_ngram(t, c("a", "b"))
