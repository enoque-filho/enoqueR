#' Informações relevantes das variáveis para testes estatísticos
#'
#'@export

dataset_var_info = function(dados, label = "na"){
  dados    = data.frame(dados)
  # Informações
  variavel = names(dados)
  rotulo   = sapply(dados, labelled::var_label)
  classe   = sapply(dados, class)
  N        = sapply(dados, function(x){length(na.omit(x))})
  n_dist   = sapply(dados, dplyr::n_distinct)
  n_niveis = sapply(dados, nlevels)
  N_miss   = sapply(dados, function(x){sum(is.na(x))})
  est_norm = sapply(dados, function(x)round(as.numeric(enoqueR::teste_normalidade(vetor = x, "var")[2]),5))
  p_norm   = sapply(dados, function(x)round(as.numeric(enoqueR::teste_normalidade(vetor = x, "var")[3]),5))
  if(label %in% c("sim","s","Sim", "SIM", "S", "y", "Y", "yes", "Yes", "YES")){
    saida = data.frame(variavel, rotulo, classe, N, n_dist, n_niveis, N_miss, est_norm, p_norm)}
  else{saida = data.frame(variavel, classe, N, n_dist, n_niveis, N_miss, est_norm, p_norm)}
  return(saida)
}

