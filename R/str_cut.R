#' Cortar strings e pegar primeira parte
#'
#' @export
str_cut = function(vetor, sep){
  s = str_split(vetor, sep); x = NULL;
  for(i in 1:length(vetor)){x[i] = s[[i]][1]}
  return(x)}
