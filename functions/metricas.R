

#' '''
#' Funcion para calcular la entropia de un vector
#' 
#' @param x: vector de datos
#' @return: entropia del vector
#' '''
v_entropia <- function(x) {
  #Obtenemos la frecuencia de cada valor, alculamos la probabilidad de cada valor y devolvemos la entropia
  frec <- table(x)
  p <- frec/length(x)
  return(-sum(p*log2(p)))
}

#' '''
#' Funcion para calcular la entropia de un dataframe
#' 
#' @param df: dataframe de datos
#' @return: vector con la entropia de cada columna
#' '''
t_entropia <- function(x){
  #Seleccionamos las columnas que no son numericas y calculamos la entropia de cada una
  nums <- x[!sapply(x, is.numeric)]
  return(apply(nums, 2, v_entropia))
}

#' '''
#' Funcion para calcular la entropia de un vector o dataframe
#' 
#' @param df: vector o dataframe de datos
#' @return: valor de la entropia o vector con la entropia de cada columna
#' '''
entropia <- function(x) {
  #Comprobamos si x es un dataframe o un vector
  if (is.data.frame(x)) {
    return (t_entropia(x))
  } else {
    return (v_entropia(x))
  }
}


#' '''
#' Función para calcular la varianza de un vector
#' 
#' @param x: vector de datos
#' @return: varianza del vector
#' '''
v_varianza <- function(x) {
  #Comprobamos que el vector es numerico y devolvemos la varianza
  if (is.numeric(x))
    return(sum((x-mean(x))^2)/(length(x)-1))
  else
    stop("El vector debe ser numérico")
  
}

#' '''
#' Funcion para calcular la varianza de un dataframe
#' 
#' @param df: dataframe de datos
#' @return: vector con la varianza de cada columna
#' '''
t_varianza <- function(x){
  #Seleccionamos las columnas que son numericas y calculamos la varianza de cada una
  nums <- x[sapply(x, is.numeric)]
  return(apply(nums, 2, v_varianza))
}


#' '''
#' Funcion para calcular la varianza de un vector o dataframe
#' 
#' @param df: vector o dataframe de datos
#' @return: valor de la varianza o vector con la varianza de cada columna
#' '''
varianza <- function(df){
    #Comprobamos si x es un dataframe o un vector
    if (is.data.frame(df)){
        return (t_varianza(df))
    }else{
        return (v_varianza(df))
    }
}

#' '''
#' Funcion para calcular la covarianza de dos vectores
#' 
#' @param x: primer vector de datos
#' @param y: segundo vector de datos
#' @return: covarianza de los vectores
#' '''
covarianza <- function(x,y){
    #Comprobamos que los vectores son numericos y devolvemos la covarianza
    if (is.numeric(x) && is.numeric(y)){
        return (sum((x-mean(x))*(y-mean(y)))/(length(x)-1))
    }
    stop('Los vectores deben ser numéricos')
}

#' '''
#' Funcion para calcular la desviacion estandar de un vector
#' 
#' @param x: vector de datos
#' @return: desviacion estandar del vector
#' '''
desviacion <- function(x) {
  return(sqrt(v_varianza(x)))
}

#' '''
#' Funcion para calcular el área bajo la curva ROC
#' 
#' @param df: dataframe de datos
#' @param target: nombre de la columna objetivo
#' @param bin_class: nombre de la columna con la clasificacion binaria
#' @return: vector con los valores de tpr, fpr y el area bajo la curva
#' '''
aucf <- function(df, target, bin_class){
  #Ordenamos el dataframe por la columna objetivo
  df <- df[order(df[,target], decreasing = T),]
  tprl <- c()
  fprl <- c()
  #obtenemos los puntos de corte
  cut_points <- unique(df[,target])
  #Calculamos los valores de tpr y fpr para cada punto de corte
  for (cut in cut_points){
    tp <- nrow(df[(df[,target] >= cut) & (df[,bin_class] == 1),])
    fp <- nrow(df[(df[,target] >= cut) & (df[,bin_class] == 0),])
    tn <- nrow(df[(df[,target] < cut) & (df[,bin_class] == 0),])
    fn <- nrow(df[(df[,target] < cut) & (df[,bin_class] == 1),])
    tpr <- tp/(tp+fn)
    fpr <- fp/(fp+tn)
    tprl <- c(tprl,tpr)
    fprl <- c(fprl,fpr)
  }
  #Calculamos el area bajo la curva
  auc <- sum(diff(tprl)*rowMeans(cbind(fprl[-1],fprl[-length(fprl)])))
  return(list(tprl=tprl,fprl=fprl, auc = auc))
}

#' '''
#' Funcion que realiza el cálculo de métricas para los atributos de un dataset: varianza y AUC para las variables contínuas y entropía para las discretas.
#' Esta funcion reconoce automaticmente el tipo de variable de cada columna del dataframe y calcula la métrica correspondiente.
#' 
#' @param df: dataframe de datos
#' @param bin_class: nombre de la columna con la clasificacion binaria
#' @param Roc_curve: booleano que indica si se desea graficar la curva ROC
#' @return: lista de 2 dataframes, el primero con las metricas de las variables contínuas y el segundo con las metricas de las variables discretas
#' '''
metricas <- function(df, bin_class=NULL, Roc_curve=FALSE){
  
  #Seleccionamos las variables continuas
  contcols <- names(df)[sapply(df, is.numeric)] 

  #calculamos las varianzas de las columnas de las variables continuas del dataframe
  var <- varianza(df[contcols])
  res <- data.frame(var)
  
  #calcular el AUC de las columnas de las variables continuas del dataframe y representar la curva ROC si se quiere
  if (!is.null(bin_class)){
    aucl <- c()
    for (col in contcols){
      if (col != bin_class){
        auc <- aucf(df, col, bin_class)
        aucl <- c(aucl, auc[3])
        if (Roc_curve == TRUE){
          plot_roc_curve(auc[1], auc[2])
        }
      }
    }
    res$AUC <- unlist(aucl)
  }
  
  #Seleccionar las variables categoricas
  catcols <- names(df)[!sapply(df, is.numeric)]
  
  #calcular la entropía de las columnas de las variables discretas del dataframe
  ent <- entropia(df[catcols])
  res2 <- data.frame(ent)
  
  return (list(res, res2))
}