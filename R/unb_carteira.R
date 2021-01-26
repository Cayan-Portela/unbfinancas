#' Retorno esperado da carteira.
#'
#' Recebe vetores de peso e retorno esperado e calcula o retorno esperado da carteira.
#'
#' @param peso vetor de números.
#' @param retorno_esp vetor de números.
#'
#' @export

unb_retorno_esp_c <- function(peso, retorno_esp){
  sum(peso * retorno_esp)
}


#' Matriz de variância-covariância dos ativos.
#'
#' Recebe vetores de desvio padrão e matriz de correlação e calcula a matriz de variância-covariância.
#'
#' @param diag_dp vetor de números.
#' @param correlacao matriz de correlação.
#'
#' @export

unb_matriz_covar <- function(desviopadrao, correlacao){
  diag_dp <- diag(desviopadrao, nrow = length(desviopadrao))
  t(diag_dp) %*% correlacao %*% diag_dp
}


#' Variância do retorno da carteira.
#'
#' Recebe vetores de peso, risco total e matriz de correlação e calcula a variância do retorno da carteira.
#'
#' @param peso vetor de números.
#' @param riscototal vetor de números.
#' @param correlacao matriz de correlação.
#'
#' @export

unb_variancia_c <- function(peso, riscototal, correlacao){
  t(peso) %*% unb_matriz_covar(riscototal,correlacao) %*% peso
}


#' Risco total da carteira.
#'
#' Recebe vetores de peso, risco total e matriz de correlação e calcula o risco total da carteira.
#'
#' @param peso vetor de números.
#' @param riscototal vetor de números.
#' @param correlacao matriz de correlação.
#'
#' @export

unb_risco_tot_c <- function(peso, riscototal, correlacao){
  (unb_variancia_c(peso, riscototal, correlacao)) ^ (1/2)
}
