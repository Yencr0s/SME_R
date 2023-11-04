#activar los paquetes
library(ggplot2)
library(plotly)
library(reshape2)



#' Función para representar la curva ROC.
#' 
#' @param tprl: lista de valores de verdaderos positivos
#' @param fprl: lista de valores de falsos positivos
#' @param zoom: booleano para indicar si se quiere hacer zoom en la gráfica
#' @export
plot_roc_curve <- function(tprl, fprl, zoom = FALSE) {
  #Definimos el maximo y el minimo de los ejes dependiendo de si se quiere hacer zoom o no
  if (zoom){
    plot(fprl, tprl, type = "l", col = "orange", xlim = c(min(fprl)-0.01,max(fprl)+0.01), ylim = c(min(tprl)-0.01,max(tprl)+0.01), xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curve")
  } else{
    plot(fprl, tprl, type = "l", col = "orange", xlim = c(0,1), ylim = c(0,1), xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curve")
  }
  abline(0,1,col="blue",lty=2)
}

#' Funcion para graficar la matriz de correlaciones e informacion mutua
#'
#' @param df: df de correlaciones e informacion mutua
#' @export
plot_mat_heatmap <- function(df){
  m <- melt(as.matrix(mat))
  ggplot(data = m, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "#94ffff", high = "#ff90ff", mid = "white", midpoint = 0,space = "Lab") +
    labs(title = "Matriz de Correlaciones e informaciones mutuas") +
    geom_text(aes(Var2, Var1, label = round(value, digits=2)), color = "black", size = 4) +
    theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_text(vjust = 1, size = 12, hjust = 1),
      axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))
}
  