#' Gráfico de correlação
#' @import ggplot2
#' @import reshape2
#' @export

eda_visual_dataset_correlacao = function(dados, metodo = "pearson"){
dados = dplyr::select_if(dados, is.numeric)
saida = melt(round(cor(dados, method = metodo), 3)) |>
  ggplot(aes(x = Var1, y = Var2, fill = value, label = value)) +
  geom_tile() +
  geom_text(color = "black", size = 2.4) +
  scale_fill_gradient2(low = "#0E79B2", mid = "#f5f0f6", high = "#da2c38", midpoint = 0,
                                limit = c(-1,1), space = "Lab",
                                name = paste("Correlação\nde" , metodo)) +
  labs(x = element_blank(), y = element_blank()) +
  theme(text = element_text(size = 8))
return(saida)
}
