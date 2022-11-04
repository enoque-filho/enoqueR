#' Visualização gráfica das variáveis
#'
#' Retorna plot com histogramas para variaveis numéricas e barras para categoricas
#'
#' @param dados conjunto de dados
#' @import ggplot2
#' @import dplyr
#' @export

visual = function(dados){

  dados  = data.frame(relocate(dados, where(purrr::negate(is.numeric)), where(is.numeric)))
  vars   = names(dados)
  classe = NULL
  plots  = list()

  for(i in 1:length(vars)){

    classe = class(dados[,vars[i]])
    d      = data.frame(var = dados[,vars[i]])
    nn     = sum(!is.na(data.frame(var = dados[,vars[i]])$var))

    if(classe %in% c("numeric","double","integer")){

      breaks = pretty(range(d$var,na.rm = TRUE), n = nclass.Sturges(d$var), min.n = 1)
      plots[[i]] =
        ggplot(d, aes(x = var)) + labs(var = vars[i]) +
        labs(x = vars[i], y = "Contagem", title = paste0(vars[i], " (n=",nn,")")) +
        geom_histogram(color  = "black", fill = "#028090", breaks = breaks) +
        theme(text = element_text(size = 8))
    }
    else{

     d2 = mutate(summarise(group_by(d, var), n = n()), pct = paste0("(",round(n/sum(n)*100,1),"%)"))
     plots[[i]] =
       ggplot(d2, aes(x = var, y = n)) +
       geom_bar(aes(fill = var), stat = "identity")  +
       geom_text(aes(label =  paste(n, pct)), vjust = -0.3) +
       theme(text = element_text(size = 8), legend.position = "none") +
       labs(x = vars[i], y = "Contagem", title = paste0(vars[i], " (n=",nn,")")) +
       scale_y_continuous(limits = c(0 ,nn))
     if(nrow(count(d2, var)) >=10){ plots[[i]] = plots[[i]] + coord_flip()}
    }
  }
  names(plots) = vars
  return(plots)
}





