#' Gerar tabela de testes usando o gtsummary
#' @param dados objeto de classe data.frame() ou tibble()
#' @param resposta variável
#' @param separado determina se será uma tabela unica ou tabekas separadas
#' @export

testes_tabelas = function(dados, resposta, separado = TRUE){
  dados = data.frame(dados)
  classr = class(dados[,resposta])  # classe da var resposa
  vars =  names(dados)[names(dados)!= resposta] # variaveis
 # pvalores = data.frame()
  if(separado == TRUE){
    tabelas = list()
    for(i in 1:length(vars)){
      tabelas[[i]] =
        dplyr::select(dados, sym(resposta), sym(vars[i])) %>%
        gtsummary::tbl_summary(by = sym(resposta)) %>%
        gtsummary::bold_labels() %>%
        gtsummary::add_p()
    }
  names(tabelas) = vars
  }
  else{
    tabelas =
      gtsummary::tbl_summary(dados, by = sym(resposta)) %>%
      gtsummary::bold_labels() %>%
      gtsummary::add_p()
  }
  return(tabelas)
}





