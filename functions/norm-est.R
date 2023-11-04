# '''
# Funcion para normalizar un vector
# 
# @param x: vector de datos
# @return: vector normalizado
# '''
v_normalizar <- function(x){
  #Comprobamos que el vector es numerico y devolvemos el vector normalizado
  if(!is.numeric(x)){
    stop("El vector debe ser numérico")
  }else{
    return((x-min(x))/(max(x)-min(x)))
  }
}

# '''
# Funcion para normalizar un dataframe
# 
# @param df: dataframe de datos
# @return: dataframe normalizado
# '''
t_normalizar <- function(x) {
  #Seleccionamos las columnas que son numericas y normalizamos cada una
  numerics <- sapply(x, is.numeric)
  nums <-  x[numerics]
  x[numerics] <- nums %>% apply(v_normalizar, MARGIN=2)
  return(x)
}

# '''
# Funcion que normaliza un dataframe o un vector.

# @param x: vector o dataframe de pandas
# @return: vector o dataframe normalizado
# '''
normalizar <- function(x) {
  #Comprobamos si x es un dataframe o un vector
  if (is.data.frame(x)) {
    return(t_normalizar(x))
  } else {
    return(v_normalizar(x))
  }
}


# '''
# Funcion para estandarizar un vector
# 
# @param x: vector de datos
# @return: vector estandarizado
# '''
v_estandarizar <- function(x){
  #Comprobamos que el vector es numerico y devolvemos el vector estandarizado
  if (is.numeric(x)){
    return((x-mean(x))/desviacion(x))
  }
  stop('El vector debe ser numérico')
}


# '''
# Funcion para estandarizar un dataframe
# 
# @param df: dataframe de datos
# @return: dataframe estandarizado
# '''
t_estandarizar <- function(x) {
  #Seleccionamos las columnas que son numericas y estandarizamos cada una
  numerics <- sapply(x, is.numeric)
  nums <-  x[numerics]
  x[numerics] <- nums %>% apply(v_estandarizar, MARGIN=2)
  return(x)
}

# '''
# Funcion que estandariza un dataframe o un vector.
# 
# @param x: vector o dataframe
# @return: vector o dataframe estandarizado
# '''
estandarizar <- function(df){
  #Comprobamos si x es un dataframe o un vector
  if (is.data.frame(df)){
    return (t_estandarizar(df))
  }else{
    return (v_estandarizar(df))
  }
}

