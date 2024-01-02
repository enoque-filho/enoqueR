#' Testes qui-quadrado para todas as variáveis catégoricas
#'
#' @export


eda_tabela_dataset_teste_qui_quadrado = function(dados, resposta = NULL){
  dados = dplyr::select_if(dados, purrr::negate(is.numeric)) |> dplyr::mutate_all(factor)
  chi = list(); var1 = NULL; var2 = NULL; assump = NULL
  if(is.null(resposta) == TRUE){
    # SE NÃO HÁ VARIÁVEL RESPOSTA
    vars = names(dados)
    cb = expand.grid(vars, vars)
    comb = list(); n = 1; i = 1; nrow(cb)
    for(i in 1:nrow(cb)){
      if( cb[i,1] != cb[i,2]){
        comb[[n]] = sort(c(cb[i,1], cb[i,2]))
        n = n+1}}
    comb = unique(comb)
    i = 1
    for(i in 1:length(comb)){
      var1[i] = as.character(comb[[i]][1])
      var2[i] = as.character(comb[[i]][2])
      chi[[i]] = rstatix::chisq_test(dados[,var1[i]], dados[,var2[i]])
      assump[i] = ifelse(TRUE %in% (data.frame(dados[,var1[i]], dados[,var2[i]]) %>%
                                      table() %>% c() <5), "Não valido", "Valido")}}
  else{
    vars = names(dados)[names(dados)!=resposta]
    for(i in 1:length(vars)){
      var1[i] = resposta; var2[i] = vars[i]
      chi[[i]] = rstatix::chisq_test(dados[,resposta], dados[,vars[i]])
      assump[i] = ifelse(TRUE %in% (data.frame(dados[,var1[i]], dados[,var2[i]]) %>%
                                      table() %>% c() <5), "Não valido", "Valido")}}
  saida = data.frame(cbind(data.frame(var1, var2), dplyr::bind_rows(chi),data.frame(assump)))
  return(saida)}
