library(dplyr) #para hacer uso de los pipes %>%

#' Funcion para disctetizar un vector de datos
#' 
#' @param x: vector
#' @param bins: puntos de corte
#' @param labels: Booleano para indicar si se quiere que los intervalos se muestren en el array (True) o solo el indice del intervalo (False)
#' @return: vector de datos discretizado
#' @export
discretizar <- function(x, bins, labels = FALSE) {
  #Añadimos los puntos de corte -Inf e Inf
  bins <- c(-Inf, bins, Inf)

  #Si labels es TRUE, creamos un array con los intervalos
  if (labels) {
    labels <- c()
    for (i in seq_along(bins)) {
      if(i<length(bins)){
        labels[i] <- paste0("[", round(bins[i], 2), ", ", round(bins[i+1], 2), "]")
      }
    }
  }

  return(cut(x, breaks = bins, labels = labels, duplicates = "drop"))
}

#' Funcion para discretizar un vector con el metodo de equal width
#' 
#' @param x: vector
#' @param num_bins: número de intervalos que queremos obtener
#' @param labels: Booleano para indicar si se quiere que los intervalos se muestren en el array (True) o solo el indice del intervalo (False)
#' @return: vector discretizado
#' @export
v_discretizarEW <- function(x, num_bins, labels = FALSE) {
  #Comprobamos que x es un vector
  if (!is.vector(x)) {
    stop("x must be a vector")
  }

  #Obtenemos los puntos de corte
  min_val <- min(x)
  max_val <- max(x)
  bin_width <- (max_val - min_val) / (num_bins)
  cut_points <- rep(min_val,num_bins-1) + (1:(num_bins-1)) * bin_width
  
  return(discretizar(x, cut_points, labels))
}

#' Funcion para discretizar un dataframe con el metodo de equal width
#' 
#' @param x: dataframe
#' @param num_bins: número de intervalos que queremos obtener
#' @param labels: Booleano para indicar si se quiere que los intervalos se muestren en el array (True) o solo el indice del intervalo (False)
#' @return: dataframe discretizado
#' @export
t_discretizarEW <- function(x, num_bins, labels = FALSE){
  #Obtenemos las columnas numéricas
  numerics <- sapply(x, is.numeric)
  nums <-  x[numerics]

  #Discretizamos las columnas numéricas
  x[numerics] <- nums %>% apply(2, v_discretizarEW, num_bins = num_bins, labels = labels)
  return(x)
}

#' Funcion para discretizar un vector o dataframe con el metodo de equal width
#' 
#' @param x: dataframe o vector
#' @param num_bins: número de intervalos que queremos obtener
#' @param labels: Booleano para indicar si se quiere que los intervalos se muestren en el array (True) o solo el indice del intervalo (False)
#' @return: dataframe o vector discretizado
#' @export
discretizarEW <- function(x, num_bins, labels = FALSE){
  #Comprobamos si x es un dataframe o un vector
  if (is(x, 'data.frame')){
    return(t_discretizarEW(x, num_bins))
  } else {
    return(v_discretizarEW(x, num_bins, labels))
  }
}


#' Funcion para discretizar un vector con el metodo de equal frequency
#' 
#' @param x: vector
#' @param num_bins: número de intervalos que queremos obtener
#' @param labels: Booleano para indicar si se quiere que los intervalos se muestren en el array (True) o solo el indice del intervalo (False)
#' @return: vector discretizado
#' @export
v_discretizarEF <- function(x, num_bins, labels = FALSE) {
  #Ordenamos el vector y obtenemos los puntos de corte
  x_sorted <- sort(x)
  cut_points <- quantile(x, probs = seq(0, 1, 1/num_bins)[2:num_bins], names = FALSE)
  return(discretizar(x, unique(cut_points), labels))
}

#' Funcion para discretizar un dataframe con el metodo de equal frequency
#' 
#' @param x: dataframe
#' @param num_bins: número de intervalos que queremos obtener
#' @param labels: Booleano para indicar si se quiere que los intervalos se muestren en el array (True) o solo el indice del intervalo (False)
#' @return: dataframe discretizado
#' @export
t_discretizarEF <- function(x, num_bins, labels = FALSE) {
  #Obtenemos las columnas numéricas
  numerics <- sapply(x, is.numeric)
  nums <-  x[numerics]
  #Discretizamos las columnas numéricas
  x[numerics] <- nums %>% apply(2,v_discretizarEF, num_bins, labels)
  return(x)
}

#' Funcion para discretizar un vector o dataframe con el metodo de equal frequency
#' 
#' @param x: dataframe o vector
#' @param num_bins: número de intervalos que queremos obtener
#' @param labels: Booleano para indicar si se quiere que los intervalos se muestren en el array (True) o solo el indice del intervalo (False)
#' @return: dataframe o vector discretizado
#' @export
discretizarEF <- function(x, num_bins, labels = FALSE){
  #Comprobamos si x es un dataframe o un vector
  if (is.data.frame(x)){
    return(t_discretizarEF(x, num_bins))
  }else{
    return(v_discretizarEF(x, num_bins, labels))
  }
}