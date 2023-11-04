
# '''
# Funcion que calcula la correlacion de dos vectores.

# @param x: vector
# @param y: vector
# @return: correlacion de los vectores
# '''
v_cor <- function(x,y){
    if (is.numeric(x) && is.numeric(y)){
        return (covarianza(x,y)/(desviacion(x)*desviacion(y)))
    }
    stop('Los vectores deben ser numéricos')
}

# '''
# Funcion que calcula la matriz de correlacion de un dataframe entre todas las variables numéricas.

# @param df: dataframe
# @return: lista con el df de correlacion entre las variables numericas y la matriz de correlacion
# '''
correlacion <- function(df){
  #Obtenemos las columnas numéricas
  numeric<- sapply(df, is.numeric)
  num <- df[numeric]
  #Generamos las posibles combinaciones con dichas columnas
  comb <- combn(names(num), 2)
  
  corr <- c()
  names <- c()
  corr_mat <- matrix(0, nrow = ncol(num), ncol = ncol(num))
  rownames(corr_mat) <- names(num)
  colnames(corr_mat) <- names(num)
  
  #Calculamos la correlación para cada combinación
  for(i in 1:ncol(comb)){
    aux_c <- v_cor(num[,comb[1,i]], num[,comb[2,i]])
    corr <- c(corr, aux_c)
    names <- c(names, paste(comb[1,i], comb[2,i], sep = '-'))
    corr_mat[comb[1,i],comb[2,i]] <- aux_c
    corr_mat[comb[2,i],comb[1,i]] <- aux_c
  }
  #Rellenamos la correlacion de las variables consigo mismas
  for (i in 1:ncol(num)){
    corr_mat[i,i] <- 1
  }
  return (list(data.frame(corr, row.names = names),data.frame(corr_mat)))
}

# '''
# Funcion que calcula la informacion mutua de dos vectores.

# @param x: vector
# @param y: vector
# @return: informacion mutua de los vectores
# '''
v_mutua <- function(x,y){
  #Calculamos la proporcion de cada valor
  px <- table(x)/length(x)
  py <- table(y)/length(y)
  #Calculamos la proporcion de cada combinacion de valores
  pxy <- data.frame(table(x,y)/length(paste(x,y)))
  #Eliminamos las combinaciones que no se dan
  pxy <- pxy[pxy$Freq != 0,]
  mut <- 0
  #Calculamos la informacion mutua
  for (i in 1:nrow(pxy)){
    mut <- mut + pxy[i,'Freq']*log2(pxy[i,'Freq']/(px[[pxy[i,'x']]]*py[[pxy[i,'y']]]))
  }
  return(mut)
}

# '''
# Funcion que calcula la matriz de informacion mutua de un dataframe entre las variables categóricas.

# @param df: dataframe
# @return: lista con el df de informacion mutua entre las variables categoricas y la matriz de informacion mutua
# '''
informacion_mutua <- function(df){
    #Obtenemos las columnas categoricas
    cols <- sapply(df, is.numeric)
    cat <- df[!cols]
    #Generamos las posibles combinaciones con dichas columnas
    comb <- combn(names(cat), 2)
    
    mut <- c()
    names <- c()

    mut_mat <- matrix(0, nrow = ncol(cat), ncol = ncol(cat))
    rownames(mut_mat) <- names(cat)
    colnames(mut_mat) <- names(cat)
    
    #Calculamos la informacion mutua para cada combinacion
    for(i in 1:ncol(comb)){
        aux_m <- v_mutua(cat[,comb[1,i]], cat[,comb[2,i]])
        mut <- c(mut, aux_m)
        names <-  c(names, paste(comb[1,i], comb[2,i], sep = '-'))
        mut_mat[comb[1,i],comb[2,i]] <- aux_m
        mut_mat[comb[2,i],comb[1,i]] <- aux_m
    }
    
    #Rellenamos la informacion mutua de las variables consigo mismas
    for (i in 1:ncol(cat)){
      mut_mat[i,i] <- entropia(cat[i])
    }
    
    return(list(data.frame(mut, row.names = names),data.frame(mut_mat)))
}

# '''
# Funcion que calcula la matriz de correlacion y la matriz de informacion mutua de un dataframe entre todas las variables.

# @param df: dataframe
# @return: lista con :
#         - dataframe con los valores de correlacion de las variables numericas
#         - dataframe con los valores de informacion mutua de las variables categoricas
#         - df de correlacion e informacion mutua de todas las variables
# '''
cor_mut <- function(df){
  #Creamos el dataframe vacio del resultado
  m <- matrix(NA, nrow = ncol(df), ncol = ncol(df),)
  m <- data.frame(m)
  rownames(m) <- names(df)
  colnames(m) <- names(df)
  
  #Calculamos la correlacion y la informacion mutua
  aux1 <- correlacion(df)
  aux2 <- informacion_mutua(df)

  #Guardamos los resultados en el dataframe
  cor <- aux1[[1]]
  m_cor <- aux1[[2]]
  inf <- aux2[[1]]
  m_inf <- aux2[[2]]
  
  m[rownames(m_inf),rownames(m_inf)]<-m_inf[rownames(m_inf)]
  m[rownames(m_cor),rownames(m_cor)]<-m_cor[rownames(m_cor)]
  
  return(list(cor,inf,m))}