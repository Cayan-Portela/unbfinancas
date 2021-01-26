#' Value at Risk não diversificado.
#'
#' Recebe vetores de mapeamento, volatilidadee nível de significância alpha retorna value at risk.
#'
#' @param mapeamento vetor de números.
#' @param volatilidade vetor de números.
#' @param alpha escalar entre 0 e 1.
#'
#' @export

unb_var_u <- function(mapeamento, volatilidade, alpha){
  z <- qnorm(1-alpha)
  aux <- mapeamento * z * volatilidade
  dim(aux) <- c(length(mapeamento), 1)
  aux
}


#' Value at Risk
#'
#' Recebe vetores de mapeamento, volatilidade, correlacao e nível de significância alpha retorna value at risk.
#'
#' @param mapeamento vetor de números.
#' @param volatilidade vetor de números.
#' @param correlacao vetor de números.
#' @param alpha escalar entre 0 e 1.
#'
#' @export

unb_var <- function(mapeamento, volatilidade, correlacao, alpha){
  varu <- unb_var_u(mapeamento, volatilidade, alpha)
  sqrt(t(varu) %*% correlacao %*% varu)
}


#' Delta VaR
#'
#' Recebe vetores de mapeamento, volatilidade, correlacao e nível de significância alpha retorna vetor de tamanho n.
#'
#' @param mapeamento vetor de números.
#' @param volatilidade vetor de números.
#' @param correlacao vetor de números.
#' @param alpha escalar entre 0 e 1.
#'
#' @export

unb_delta_var <- function(mapeamento, volatilidade, correlacao, alpha){
  sigma <- unb_sigma(volatilidade, correlacao)
  var <- unb_var(mapeamento, volatilidade, correlacao, alpha)
  var<-as.numeric(var)
  qe <- unb_q_cov(sigma, alpha) %*% mapeamento
  qe / var
}


#' Component VaR
#'
#' Recebe vetores de mapeamento, volatilidade, correlacao e nível de significância alpha retorna vetor de tamanho n.
#'
#' @param mapeamento vetor de números.
#' @param volatilidade vetor de números.
#' @param correlacao vetor de números.
#' @param alpha escalar entre 0 e 1.
#'
#' @export

unb_component_var <- function(mapeamento, volatilidade, correlacao, alpha){
  deltavar <- unb_delta_var(mapeamento, volatilidade, correlacao, alpha)
  mapeamento * deltavar
}


#' Incremental VaR
#'
#' Recebe vetores de mapeamento, volatilidade, correlacao, nível de significância alpha e vetor de incremento e retorna escalar.
#'
#' @param mapeamento vetor de números.
#' @param volatilidade vetor de números.
#' @param correlacao vetor de números.
#' @param correlacao vetor de números
#' @param alpha escalar entre 0 e 1.
#'
#' @export

unb_incremental_var <- function(mapeamento, volatilidade, correlacao, alpha, incremento){
  deltaVaR <- unb_delta_var(mapeamento = fator_risco$Mapeamento, volatilidade = fator_risco$Volatilidade,
                            correlacao = mat_corr, alpha = alpha1)
  t(deltaVaR) %*% incremento
}

