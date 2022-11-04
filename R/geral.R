#' Visão geral de banco de dados
#' Retorna as seguintes informações de um banco de dados:**
#' - Nome das variáveis
#' - Classe das variáveis
#' - Normalidade (caso numeric)
#' - Niveis e frequencia (casso character ou factor)
#' - Numéro de observações não ausentes em cada variável

#'Pacotes Exigidos:
#' - dplyr
#' Atualizado em: 23/09/2022
#' Mudança: Adicionado teste de normalidade KS (caso n>=5000)
#' @param dados banco de dados
#' @export

geral = function(dados){
  dados = data.frame(dados)
  vars = names(dados)
  var_info = NULL
  classe = NULL
  Obs = NULL


  for(i in 1:length(vars)){
    classe[i] = class(dados[,vars[i]])
    if(classe[i] %in% c("numeric","double","integer")){
      if(nrow(dados)<5000){
        W = shapiro.test(dados[,vars[i]])$statistic["W"]
        p = round(shapiro.test(dados[,vars[i]])$p.value,4)
        var_info[i] = paste0(ifelse(p>=0.05,"Normal","Não Normal"), " (W: ",round(W,5),", p: ",p,")")
      }
      else{
        ks = ks.test(dados[,vars[i]], pnorm, mean(dados[,vars[i]]), sd(dados[,vars[i]]))$statistic["D"]
        p = round(ks.test(dados[,vars[i]], pnorm, mean(dados[,vars[i]]), sd(dados[,vars[i]]))$p.value,4)
        var_info[i] = paste0(ifelse(p>=0.05,"Normal","Não Normal"), " (KS: ",round(ks,5),", p: ",p,")")
      }

    }
    else{
      if( nrow(table(dados[,vars[i]])) <= 10){
      tabela = NULL
      tabela = as.list(table(as.factor(dados[,vars[i]])))
      nv = NULL
      for( j in 1:length(names(tabela))){
        names(tabela[[j]]) = j
        nv[j] = paste0(names(tabela)[j],": ",tabela[[j]])
      }
      var_info[i] = paste0(nv,collapse = ", ")
      }
      else{
        total = nrow(table(as.factor(dados[,vars[i]])))
        paste0("Muitos: ", total, " níveis")
        var_info[i] = paste0("Muitos: ", total, " níveis")
        }
    }
    Obs[i] = length(na.omit(dados[,vars[i]]))
  }

  saida = dplyr::tibble(Variável = vars, Tipo = classe, Informação = var_info, Obs)
  return(saida)
}

