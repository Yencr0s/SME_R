% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metricas.R
\name{aucf}
\alias{aucf}
\title{Funcion para calcular el área bajo la curva ROC}
\usage{
aucf(df, target, bin_class)
}
\arguments{
\item{df:}{dataframe de datos}

\item{target:}{nombre de la columna objetivo}

\item{bin_class:}{nombre de la columna con la clasificacion binaria}
}
\value{
: vector con los valores de tpr, fpr y el area bajo la curva
}
\description{
Funcion para calcular el área bajo la curva ROC
}
