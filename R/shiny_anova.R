#' Shiny App to Demonstrate Analysis of Variance
#'
#' @name shiny_anova
#' @aliases shiny_anova
#' @description An interactive Shiny app to demonstrate Analysis of Variance.
#' @usage shiny_anova()
#'
#' @details The interactive Shiny app demonstrates the principles of Analysis of Variance.
#'     The true parameter values are provided by the user.
#'     The user changes sample characteristics, distribution function and simulation features
#'     and explores the influence of these changes on the hypothesis testing using principles of analysis of variance.
#'
#'     The left panel includes the user inputs for \strong{Simulation Features}, \strong{Population Parameters},
#'     \strong{Sample Characteristics}, and \strong{Distribution Function}.
#'     To use the app at first instance, just click the \code{Update} button.
#'     To alter the input values, edit the text box or move the point on the slider and
#'     explore the changes in different tabs (see below).
#'
#'     To obtain identical outcomes in a separate run of the app,
#'     set a common seed value at the bottom of the left panel and click \code{Update}.
#'     All subsequent updates will produce identical results provided other inputs are identical.
#'     The seed value is ignored when the option \code{check the box to update instantly} is selected.
#'
#' @return The outcomes are presented in several tabs.
#'     \item{Population}{contains the density plots of three populations and
#'     rug plots of the sample units randomly drawn from these populations.
#'     It also shows the population parameter values chosen by the user.}
#'     \item{Sample}{contains the dot plots and box plots of three samples drawn
#'     randomly from the three populations and rug plots of the sample units.
#'     It also includes the estimates of mean and standard deviation of three samples.}
#'     \item{SS & MS}{contains the bar plots showing the between and within sum of squares (SS)
#'     and mean squares (MS) as well as the proportion of between and within SS over total SS.}
#'     \item{Test Statistic}{contains the plots showing the mean difference between groups.
#'     and corresponding 95\% confidence intervals (CI).
#'     The tab also contains the distribution of the test statistic \code{F},
#'     the observed value of the test statistic and probabilities
#'     under the given value of the Type 1 error}
#'     \item{Summary}{includes the summary of the sampled data and outcomes
#'     from the one-way analysis of variance. Different sections are:
#'     (1) Hypothesis, highlighting the null and alternative hypothesis;
#'     (2) Sample, tabulating the full sampled data;
#'     (3) Summary Statistics, summarising the summary information of three samples;
#'     (4) Model Outputs, the outputs from fitting the analysis of variance model.
#'     The section also present the multiple comparision of means using
#'     Tukey's Honest Significant Differences test.
#'     This section represents standard R outputs based on fitting an \code{\link{lm}} function.}
#'
#' @note \url{https://shiny.abdn.ac.uk/Stats/apps/}
#'
#' @author Mintu Nath
#'
#' @seealso Function in base R for normal distribution, F distribution and fitting linear model including
#'          \code{\link{dnorm}}, \code{\link{pnorm}}, \code{\link{qnorm}}, \code{\link{rnorm}},
#'          \code{\link{df}}, \code{\link{pf}}, \code{\link{qf}}, \code{\link{rf}},
#'          \code{\link{lm}}, \code{\link{aov}},
#'          \code{\link{anova.lm}}, \code{\link{summary.lm}}
#'          \code{\link{summary.aov}}, \code{\link{model.tables}}
#'
#' @examples
#' if(interactive()){
#'     library(ggplot2)
#'     library(shiny)
#'     library(ABACUS)
#'     # Run shiny app
#'     shiny_anova()
#' }
#'
#' @import shiny
#' @import ggplot2
#' @export

# Function

shiny_anova <- function() {

  shiny::runApp(appDir = system.file("app_anova", package = "ABACUS"), launch.browser = TRUE)
  Sys.setenv("shiny_anova" = "")

}

