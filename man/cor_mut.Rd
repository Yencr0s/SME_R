% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corr.R
\name{cor_mut}
\alias{cor_mut}
\title{Funcion que calcula la matriz de correlacion y la matriz de informacion mutua de un dataframe entre todas las variables.}
\usage{
cor_mut(df)
}
\arguments{
\item{df:}{dataframe}
}
\value{
: lista con :
        - dataframe con los valores de correlacion de las variables numericas
        - dataframe con los valores de informacion mutua de las variables categoricas
        - df de correlacion e informacion mutua de todas las variables
}
\description{
Funcion que calcula la matriz de correlacion y la matriz de informacion mutua de un dataframe entre todas las variables.
}
