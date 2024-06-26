% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_fun.R
\name{cost_and_taxes}
\alias{cost_and_taxes}
\title{cost_and_taxes}
\usage{
cost_and_taxes(
  qte = NA,
  pu = NA,
  prix_ht = NA,
  tva = NA,
  prix_ttc = NA,
  prix_tva = NA,
  pu_ttc = NA,
  adjust = NA,
  prix_d_ht = NA,
  prix_d_ttc = NA,
  pu_d = NA,
  pu_d_ttc = NA
)
}
\arguments{
\item{qte}{is the quantity of elements}

\item{pu}{is the price of a single elements without taxes}

\item{prix_ht}{is the duty-free price of the whole set of elements}

\item{tva}{is the percentage of all taxes}

\item{prix_ttc}{is the price of all the elements with taxes}

\item{prix_tva}{is the cost of all the taxes}

\item{pu_ttc}{is the price of a single element taxes included}

\item{adjust}{is the discount percentage}

\item{prix_d_ht}{is the free-duty price of an element after discount}

\item{prix_d_ttc}{is the price with taxes of an element after discount}

\item{pu_d}{is the price of a single element after discount and without taxes}

\item{pu_d_ttc}{is the free-duty price of a single element after discount}
}
\description{
Allow to calculate basic variables related to cost and taxes from a bunch of products (elements). So put every variable you know in the following order:
}
\examples{

print(cost_and_taxes(pu=45, prix_ttc=21, qte=3423))

#[1]  3.423000e+03  4.500000e+01  4.500000e+01 -9.998637e-01  2.100000e+01
#[6] -1.540140e+05  4.500000e+01            NA            NA            NA
#[11]            NA            NA

}
