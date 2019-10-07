#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

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

#' @title coeftest
#'
#' @description This function allows users to apply t- and z-Wald tests on their coefficients
#'
#' @param x an object
#' @param vcov specification of covariance matrix of estimated coefficients
#' @param df degrees of freedom: iff it's a finite positive number a t test with df degrees of freedom is performed, otherwise a z test is used
#' @param ... further arguments passed to methods and vcov
#' @param parm specification of which parameters are to be given confidence intervals, if missing all params are considered
#' @param level confidence level required
#'
#' @return An object with class "coeftest" which is a coefficient matrix with columns containing estimates, standard errors, test statistics, and p values.
#'
#' @examples generate a regressor
#' ## a different covariance matrix can be also used:
#'(ct <- coeftest(fm, df = Inf, vcov = vcovHC))
#'
#' @importFrom lmtest coeftest
#' @export wam
coeftest <- function(x, vcov. = NULL, df = NULL, ...) {
  lmtest::coeftest(x, vcov. = NULL, df = NULL, ...)
}

#' @title ivreg
#'
#' @description This function allows users to fit instrumental-variable regression through two-stage least squares. Regressors and instruments for ivreg are most easily specified in a formula with two parts on the right-hand side, e.g., y ~ x1 + x2 | z1 + z2 + z3, where x1 and x2 are the regressors and z1, z2, and z3 are the instruments. Note that exogenous regressors have to be included as instruments for themselves.
#'
#' @param formula formula specification of both formulas with instrument included in the second formula
#' @param instruments read above
#' @param data optional data frame containing variables in the model
#' @param subset optional vector specifying subset of observations to be used in fitting the model
#' @param na.action function indicating what should be done with NAs in data
#' @param weights optional vector of weights to be used in fitting process
#' @param offset optional offset
#' @param contrasts optional list
#' @param model logical, if true model frame is returned
#' @param x logical, if true model matrices are returned
#' @param y logical, if true response is returned
#' @param ... further arguments
#'
#' @return ivreg returns an object of class "ivreg", with econometric model with intstrumental variable used
#'
#' @examples modeling
#' fm <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
#' data = CigarettesSW, subset = year == "1995")
#' summary(fm)
#' summary(fm, vcov = sandwich, df = Inf, diagnostics = TRUE)
#'
#' @importFrom AER ivreg
#' @export wam
ivreg <- function(formula, instruments, data, subset, na.action, weights, offset,
                  contrasts = NULL, model = TRUE, y = TRUE, x = FALSE, ...){
  AER::ivreg(formula, instruments, data, subset, na.action, weights, offset,
             contrasts = NULL, model = TRUE, y = TRUE, x = FALSE, ...)
}

#' @title margins
#'
#' @description This function allows users to access more descriptive model margins (like Stata's margins command)
#'
#' @param model Desired model to marginify
#' @param ... arguments passed to methods
#'
#' @return dataframe of class "margins" returned containing margins
#'
#' @examples using a linear model
#' basic example using linear model
#' require("datasets")
#' x <- lm(mpg ~ cyl * hp + wt, data = head(mtcars))
#' margins(x)
#'
#' @importFrom margins margins
#' @export wam
margins <- function(model, ...)
{
  margins::margins(model, ...)
}

#' @title PseudoR2
#'
#' @description This function allows users to access more R2 values (ex. McFadden's)
#'
#' @param x glm, polr, or multinom model object to be evaluated
#' @param which character, out of "McFadden","AldrichNelson", "McFaddenAdj", "Nagelkerke", "CoxSnell", "Effron", "McKelveyZavoina", "Tjur", and "all".
#'
#' @return value of desired statistic
#'
#' @examples usage
#' PseudoR2(x, which = NULL)
#'
#' @importFrom DescTools PseudoR2
#' @export wam
PseudoR2 <- function(x, which = NULL) {
  DescTools::PseudoR2(x, which = NULL)
}

#' @title PseudoR2
#'
#' @description This function allows users to execute Heckman selection models (tobits) with one or two outcomes
#'
#' @param selection formula
#' @param outcome outcome equation, either single equation or list of two equations
#' @param data optional dataframe list or environment
#' @param weights optional vector of prior weights
#' @param method how to estimate the model - either ml for max likelihood or 2step for 2-step estimation, or model.frame for returning the model frame only
#' @param type model type
#'
#' @return value of desired statistic
#'
#' @examples usage
#' heckit( selection, outcome, data = sys.frame(sys.parent()),
#' method = "2step", ... )
#'
#' @importFrom sampleSelection heckit
#' @export wam
heckit <- function(selection, outcome, data = sys.frame(sys.parent()),
                   method = "2step", ... )
{
  sampleSelection::heckit(selection, outcome, data = sys.frame(sys.parent()),
                          method = "2step", ... )
}


