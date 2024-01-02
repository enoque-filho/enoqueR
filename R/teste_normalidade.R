#' Teste de normalidade
#' @export
#'
teste_normalidade = function(vetor, alpha = 0.05){

  if(is.numeric(vetor) == TRUE){

  vetor = na.omit(vetor)

  if(length(vetor) < 5000){
    teste = "Shapiro-Wilk"
    estatistica = shapiro.test(vetor)$statistic["W"]
    p = round(shapiro.test(vetor)$p.value, 4)
  }
  else{
    vetor = unique(vetor)
    teste = "Kolmogorov-Smirnov"
    estatistica = ks.test(vetor, pnorm, mean(vetor), sd(vetor))$statistic["D"]
    p = ks.test(vetor, pnorm, mean(vetor), sd(vetor))$p.value
  }

  txt = glue::glue("O teste de {teste}, com significância de {alpha}%, foi realizado e {resultado} ({nome_estatistica} = {estatistica}, p = {p})",
           teste = teste,
           resultado = ifelse(p >= alpha,
                              "não mostrou evidências para rejeitar a hipótese de normalidade dos dados",
                              "mostrou evidências para rejeitar a hipótese de normalidade dos dados"),
           nome_estatistica = names(estatistica),
           estatistica = round(estatistica, 3),
           p = scales::pvalue(p))
  saida = c(teste = teste, estatistica, p.valor = p, txt)
  }
  else{saida = NA}
  return(saida)
}


