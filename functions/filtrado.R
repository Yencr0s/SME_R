
#' '''
#' Funcion para filtrar un dataframe por entropia o varianza. Se eliminan todas aquellas
#' columnas que no cumplan la condicion indicada. Si se quiere filtrar por entropia, se debe 
#' indicar el parametro Entropy como True, si se quiere filtrar por varianza, se debe indicar
#' como False.
#' 
#' @param df: dataframe
#' @param Entropy: booleano para filtrar por entropia o varianza (por defecto True)
#' @param condition: string con la condicion para filtrar (<, >, <=, >=, ==)
#' @param valueV: valor para filtrar
#' @return: dataframe filtrado
#' '''
filtrar <- function(df, Entropy=TRUE, condition="", value = 0){
  #Obtenemos los df con la varianza y la entropia
  aux <- metricas(df)
  var <- aux[[1]]
  ent <- aux[[2]]
  #Filtramos por entropia o varianza
  if (Entropy){
    if (condition == "<"){
      #Eliminamos las columnas que no cumplan la condicion
      df <- df[, !(names(df) %in% row.names(ent)[ent >= value])]
    }else if (condition == ">"){
      df <- df[, !(names(df) %in% row.names(ent)[ent <= value])]
    }else if (condition == "<="){
      df <- df[, !(names(df) %in% row.names(ent)[ent > value])]
    }else if (condition == ">="){
      df <- df[, !(names(df) %in% row.names(ent)[ent < value])]
    }else if (condition == "=="){
      df <- df[, !(names(df) %in% row.names(ent)[ent != value])]
    }
  }else{
    if (condition == "<"){
      df <- df[, !(names(df) %in% row.names(var)[var >= value])]
    }else if (condition == ">"){
      df <- df[, !(names(df) %in% row.names(var)[var <= value])]
    }else if (condition == "<="){
      df <- df[, !(names(df) %in% row.names(var)[var > value])]
    }else if (condition == ">="){
      df <- df[, !(names(df) %in% row.names(var)[var < value])]
    }else if (condition == "=="){
      df <- df[, !(names(df) %in% row.names(var)[var != value])]
    }
  }
  return (df)
}