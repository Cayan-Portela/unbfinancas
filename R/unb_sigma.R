#' Matriz Sigma
#'
#' Recebe vetores de volatilidade e correlação e retorna matriz de variância covariância
#'
#' @param volatilidade vetor de números.
#' @param correlacao vetor de números.
#'
#' @export

unb_sigma <- function(volatilidade, correlacao){
  diag_dp <- diag(volatilidade, nrow = length(volatilidade))
  t(diag_dp) %*% correlacao %*% diag_dp
}


#' Matriz Q
#'
#' Recebe vetores de volatilidade e correlação e nível de significância alpha e retorna matriz Q
#'
#' @param volatilidade vetor de números.
#' @param correlacao vetor de números.
#' @param alpha escalar entre 0 e 1.
#'
#' @export

unb_q_corr  <- function(volatilidade, correlacao, alpha){
  unb_sigma(volatilidade, correlacao) * (qnorm(1-alpha)^2)
}


#' Matriz Q
#'
#' Recebe matriz de variância-covariância nível de significância alpha e retorna matriz Q
#'
#' @param sigma matriz de variância-covariância.
#' @param alpha escalar entre 0 e 1.
#'
#' @export

unb_q_cov  <- function(sigma, alpha){
  sigma * (qnorm(1-alpha)^2)
}
