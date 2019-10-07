#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function(){
  print('Hello World!')
}

#' @title import
#'
#' @description This function allows importation of files in your directory into your R file
#'
#' @param file the file to import
#'
#' @return None
#'
#' @examples import('test.csv')
#' @importFrom rio import
#' @export wam

import <- function(file) {
  rio::import(file)
}

#' @title export
#'
#' @description This function allows users to export objects into a desired file in their working directory
#'
#' @param file the file to deposit the object into
#' @param x the object itself
#'
#' @return None
#'
#' @examples export(df, 'test.csv')
#' @importFrom rio export
#' @export wam

export <- function(x, file) {
  rio::export(x, file)
}

#' @title bptest
#'
#' @description This function allows users to apply the Breusch-Pagan test for heteroskedasticity on their regression. The Breusch-Pagan test fits a linear regression model to the residuals of a linear regression model (by default the same explanatory variables are taken as in the main regression model) and rejects if too much of the variance is explained by the additional explanatory variables.
#'
#' @param formula a symbolic description for the model to be tested (or a fitted "lm" object)
#' @param varformula a formula describing only the potential explanatory variables for the variance (no dependent variable needed). By default the same explanatory variables are taken as in the main regression model.
#' @param studentize logical. If set to TRUE Koenker's studentized version of the test statistic will be used.
#' @param data an optional data frame containing the variables in the model. By default the variables are taken from the environment which bptest is called from.
#'
#' @return A list with class "htest" containing a statistic (the value of the test statistic), p.value (the p-value of the test), parameter (degrees of freedom), method (a character string indicating what type of test was performed), and data.name(a character string giving the name(s) of the data)
#'
#' @examples generate a regressor
#' x <- rep(c(-1,1), 50)
#' generate heteroskedastic and homoskedastic disturbances
#' err1 <- rnorm(100, sd=rep(c(1,2), 50))
#' err2 <- rnorm(100)
#' generate a linear relationship
#' y1 <- 1 + x + err1
#' y2 <- 1 + x + err2
#' perform Breusch-Pagan test
#' bptest(y1 ~ x)
#' bptest(y2 ~ x)
#'
#' @importFrom lmtest bptest
#' @export wam

bptest <- function(formula, varformula = NULL, studentize = TRUE, data = list()) {
  lmtest::bptest(formula, varformula = NULL, studentize = TRUE, data = list())
}

#' @title bptest
#'
#' @description This function allows users to apply the Breusch-Pagan test for heteroskedasticity on their regression. The Breusch-Pagan test fits a linear regression model to the residuals of a linear regression model (by default the same explanatory variables are taken as in the main regression model) and rejects if too much of the variance is explained by the additional explanatory variables.
#'
#' @param formula a symbolic description for the model to be tested (or a fitted "lm" object)
#' @param varformula a formula describing only the potential explanatory variables for the variance (no dependent variable needed). By default the same explanatory variables are taken as in the main regression model.
#' @param studentize logical. If set to TRUE Koenker's studentized version of the test statistic will be used.
#' @param data an optional data frame containing the variables in the model. By default the variables are taken from the environment which bptest is called from.
#'
#' @return A list with class "htest" containing a statistic (the value of the test statistic), p.value (the p-value of the test), parameter (degrees of freedom), method (a character string indicating what type of test was performed), and data.name(a character string giving the name(s) of the data)
#'
#' @examples generate a regressor
#' x <- rep(c(-1,1), 50)
#' generate heteroskedastic and homoskedastic disturbances
#' err1 <- rnorm(100, sd=rep(c(1,2), 50))
#' err2 <- rnorm(100)
#' generate a linear relationship
#' y1 <- 1 + x + err1
#' y2 <- 1 + x + err2
#' perform Breusch-Pagan test
#' bptest(y1 ~ x)
#' bptest(y2 ~ x)
#'
#' @importFrom lmtest bptest
#' @export wam
coeftest <- function(x, vcov. = NULL, df = NULL, ...) {
  lmtest::coeftest(x, vcov. = NULL, df = NULL, ...)
}

#' @title Centralizes many econometric techniques
#'
#' @description This package gives access to useful econometric tools
#'
#' @param
#'
#' @return NULL
#'
#' @examples
#'
#' @export wam
ivreg <- function(formula, instruments, data, subset, na.action, weights, offset,
                  contrasts = NULL, model = TRUE, y = TRUE, x = FALSE, ...){
  AER::ivreg(formula, instruments, data, subset, na.action, weights, offset,
             contrasts = NULL, model = TRUE, y = TRUE, x = FALSE, ...)
}

#' @title Centralizes many econometric techniques
#'
#' @description This package gives access to useful econometric tools
#'
#' @param
#'
#' @return NULL
#'
#' @examples
#'
#' @export wam
margins <- function(model, ...)
{
  margins::margins(model, ...)
}

#' @title Centralizes many econometric techniques
#'
#' @description This package gives access to useful econometric tools
#'
#' @param
#'
#' @return NULL
#'
#' @examples
#'
#' @export wam
PseudoR2 <- function(x, which = NULL) {
  DescTools::PseudoR2(x, which = NULL)
}

#' @title Centralizes many econometric techniques
#'
#' @description This package gives access to useful econometric tools
#'
#' @param
#'
#' @return NULL
#'
#' @examples
#'
#' @export wam
heckit <- function(selection, outcome, data = sys.frame(sys.parent()),
                   method = "2step", ... )
{
  sampleSelection::heckit(selection, outcome, data = sys.frame(sys.parent()),
                          method = "2step", ... )
}


