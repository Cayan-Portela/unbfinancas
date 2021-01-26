#' Esperança.
#'
#' Calcula o valor esperado com base em um vetor numérico com respectivas probabilidades associadas.
#'
#' @param prob vetor de números.
#' @param valor vetor de números.
#'
#' @export

unb_esperanca <- function(prob, valor){
  sum(prob * valor)
}

#' Variância.
#'
#' Calcula a variância com base em um vetor numérico com respectivas probabilidades associadas.
#'
#' @param prob vetor de números.
#' @param valor vetor de números.
#'
#' @export

unb_variancia <- function(prob, valor){
  mu <- unb_esperanca(prob, valor)
  sum(prob * (valor - mu) ^ 2)
}


#' Desvio-padrão.
#'
#' Calcula o desvio-padrão com base em um vetor numérico com respectivas probabilidades associadas.
#'
#' @param prob vetor de números.
#' @param valor vetor de números.
#'
#' @export

unb_desvio_padrao <- function(prob, valor){
  unb_variancia(prob, valor) ^ (1/2)
}

#' Covariância
#'
#' Calcula a covariância de dois vetores.
#'
#' @param prob vetor de números.
#' @param valor1 vetor de números.
#' @param valor2 vetor de números.
#'
#' @export

unb_covariancia <- function(prob, valor1, valor2){
  mu_1 <- unb_esperanca(prob, valor1)
  mu_2 <- unb_esperanca(prob, valor2)
  sum(prob * (valor1 - mu_1) * (valor2 - mu_2))
}


#' Correlação.
#'
#' Calcula a correlação entre dois vetores.
#'
#' @param prob vetor de números.
#' @param valor1 vetor de números.
#' @param valor2 vetor de números.
#'
#' @export

unb_correlacao <- function(prob, valor1, valor2){
  dp_1 <- unb_desvio_padrao(prob, valor1)
  dp_2 <- unb_desvio_padrao(prob, valor2)
  cov <- unb_covariancia(prob, valor1, valor2)
  cov / (dp_1 * dp_2)
}
