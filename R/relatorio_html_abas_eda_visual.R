#' Gráficos em abas (HTML)
#' @export

relatorio_html_abas_eda_visual = function(dados){
  dados = data.frame(dados)
  vars = names(dados)
  classe   = NULL
  plots = list()
  cat('## Visualização das variáveis {.tabset}   \n\n')
  for(i in 1:length(vars)){

    classe[i] = class(dados[,vars[i]])
    d = data.frame(var = dados[,vars[i]])

    if(classe[i] %in% c("numeric","double","integer")){
      breaks = pretty(range(d$var,na.rm = TRUE), n = nclass.Sturges(d$var), min.n = 1)
      plots[[i]] =
        ggplot2::ggplot(d, aes(x = var)) + labs(var = vars[i]) +
        ggplot2::labs(x = vars[i], y = "contagem") +
        ggplot2::geom_histogram(color  = "black", fill = "#028090", breaks = breaks) +
        ggplot2::theme(text = element_text(size = 8))
    }
    else{
      if(nrow(count(d, var)) <= 100){
      plots[[i]] =
        ggplot2::ggplot(d, aes(x = var)) +
        ggplot2::geom_bar(aes(fill = var))  +
        ggplot2::theme(text = element_text(size = 8), legend.position = "none") +
        ggplot2::labs(x = vars[i], y = "contagem")

      if(nrow(count(d, var)) >=10){ plots[[i]] = plots[[i]] + coord_flip()}
      }
      else{plots[[i]] = "Muitos niveis, o gráfico não foi computado"}}
    cat(paste0("### ", vars[i],"\n\n"))
    print(plots[[i]])
    cat('\n\n')
  }
}
