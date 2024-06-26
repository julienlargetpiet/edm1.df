% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_fun.R
\name{intersect_mod}
\alias{intersect_mod}
\title{intersect_mod}
\usage{
intersect_mod(datf, inter_col, mod_col, n_min, descendly_ordered = NA)
}
\arguments{
\item{datf}{is the input dataframe}

\item{inter_col}{is the column name or the column number of the values that may be commun betwee the different mods}

\item{mod_col}{is the column name or the column number of the mods in the dataframe}

\item{n_min}{is the minimum elements in common a mod should have to be taken in count}

\item{ordered_descendly, }{in case that the elements in commun are numeric, this option can be enabled by giving a value of TRUE or FALSE see examples}
}
\description{
Returns the mods that have elements in common
}
\examples{

datf <- data.frame("col1"=c("oui", "oui", "oui", "oui", "oui", "oui", 
                     "non", "non", "non", "non", "ee", "ee", "ee"), "col2"=c(1:6, 2:5, 1:3))

print(intersect_mod(datf=datf, inter_col=2, mod_col=1, n_min=2))

   col1 col2
2   oui    2
3   oui    3
7   non    2
8   non    3
12   ee    2
13   ee    3

print(intersect_mod(datf=datf, inter_col=2, mod_col=1, n_min=3))

   col1 col2
2   oui    2
3   oui    3
4   oui    4
5   oui    5
7   non    2
8   non    3
9   non    4
10  non    5

print(intersect_mod(datf=datf, inter_col=2, mod_col=1, n_min=5))

  col1 col2
1  oui    1
2  oui    2
3  oui    3
4  oui    4
5  oui    5
6  oui    6

datf <- data.frame("col1"=c("non", "non", "oui", "oui", "oui", "oui", 
                      "non", "non", "non", "non", "ee", "ee", "ee"), "col2"=c(1:6, 2:5, 1:3))

print(intersect_mod(datf=datf, inter_col=2, mod_col=1, n_min=3))

   col1 col2
8   non    3
9   non    4
10  non    5
3   oui    3
4   oui    4
5   oui    5

}
