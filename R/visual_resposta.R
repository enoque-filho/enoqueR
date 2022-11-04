#' @import dplyr
#' @import ggplot2
#' @export

visual_resposta = function(dados, resposta, ncols){
   dados = data.frame(dados)
   classr = class(dados[,resposta])  # classe da var resposa
   vars =  names(dados)[names(dados)!= resposta] # variaveis
   graficos = list()

  for(i in 1:length(vars)){
    # Caso: Resposta Qualitativa:
    if(classr %in% c("factor","character")){ # qualitativa vs
      if( class(dados[,vars[i]]) %in% c("factor","character")){# qualitativa
        d = dplyr::select(dados, resposta = all_of(resposta), var = all_of(vars[i]))
        graficos[[i]] =
          ggplot(d, aes(x = var, fill = resposta)) +
          geom_bar(color = "black") +
          labs(x = vars[i], y = resposta)
      }
      else{ # qualitativa vs quantitativa
        d = dplyr::select(dados, resposta = resposta, var = vars[i])
        graficos[[i]] =
          ggplot(d, aes(x = resposta, y = var, fill = resposta)) +
          geom_boxplot(color = "black") +
          labs(x = vars[i], y = resposta) +
          theme(legend.position = "none", text = element_text(size = 8))
      }
    }
    # Caso: Resposta Quantitativa:
    else{
      if(class(dados[,vars[i]]) %in% c("factor","character")){
        # quantitativa vs quantitativa}
        d = dplyr::select(dados, resposta = resposta, var = vars[i])
        graficos[[i]] = ggplot(d, aes(x = var, y = resposta, fill = var)) +
          geom_boxplot(color = "black") +
          labs(x = vars[i], y = resposta) +
          theme(legend.position = "none", text = element_text(size = 8))
      }
      else{
        d = dplyr::select(dados, resposta = resposta, var = vars[i])
        graficos[[i]] =
          ggplot(d, aes(x = var, y = resposta)) +
          geom_point(color = "black") +
          labs(x = vars[i], y = resposta)
      }
    }
  }
   return(gridExtra::grid.arrange(grobs = graficos, ncols))
}
