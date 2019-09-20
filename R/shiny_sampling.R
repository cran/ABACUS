#' Shiny App to Explore Properties of Sampling Distributions
#'
#' @name shiny_sampling
#' @aliases shiny_sampling
#' @description An interactive Shiny app to demonstrate properties of the sampling distributions.
#' @usage shiny_sampling()
#'
#' @details The interactive Shiny app demonstrates the properties of the sampling distribution.
#'     The true population parameter values of the Normal distribution are provided by the user.
#'     The user draws many samples from the population with the given sample characteristics
#'     and explore the variability of sample means.
#'     The app also includes the construction of 95\% confidence interval for all samples.
#'     Altering the population and sample characteristics, the user can explore
#'     the influence of these changes on the sampling distribution.
#'
#'     The left panel includes the user inputs for \strong{Simulation Features}, \strong{Population Parameters},
#'     \strong{Sample Characteristics} and \strong{Distribution Function}.
#'     To use the app at first instance, just click the \code{Update} button.
#'     To alter the input values, edit the text box or move the point on the slider and
#'     explores the changes in different tabs (see below).
#'
#'     To obtain identical outcomes in a separate run of the app,
#'     set a common seed value at the bottom of the left panel and click \code{Update}.
#'     All subsequent updates will produce identical results provided other inputs are identical.
#'     The seed value is ignored when the option \code{check the box to update instantly} is selected.
#'
#' @return The outcomes are presented in several tabs.
#'     \item{Population & Sample}{contains the density plots of the population and
#'     dot plot of the sample units for the first sample randomly drawn from the population.
#'     It also includes the population parameter values are chosen by the user as well as
#'     estimates of sample mean and standard deviation based on the first sample.}
#'     \item{Sampling Distribution}{contains a panel of 8 dot plots based on the sample drawn
#'     randomly from the population with given parameters.
#'     Each plot depicts the mean and standard deviation of the random sample.}
#'     \item{Sample Estimators}{contains the histogram of the observed sample means and
#'     the empirical distribution of sample means. It also includes the rug plot of all sample means.}
#'     \item{Confidence Interval}{contains the plot showing the 95\% confidence intervals (CI) of all samples.
#'     The plot shows the true population mean as a red horizontal line.
#'     It also provides the exact number of these estimated CI that include the true population mean.}
#'     \item{Summary}{includes the summary of the sampled data and outcomes
#'     from the one-sample z-test. Different sections are:
#'     (1) Sample, tabulating the full sampled data;
#'     (2) Sample Distribution, highlighting the expection of sample mean and sample standard deviation
#'     as well as standard error of mean;
#'     (3) Confidence Interval, showing the concept of 95\% confidence intervals (CI) of mean.}
#'
#' @note \url{https://shiny.abdn.ac.uk/Stats/apps/}
#'
#'     Also note that under the central limit theorem, the distribution of the sample means will follow normal distribution
#'     whatever the distribution of the variable in the population.
#'
#' @author Mintu Nath

#' @seealso Function in base R for normal distribution including
#'          \code{\link{dnorm}}, \code{\link{pnorm}}, \code{\link{qnorm}}, \code{\link{rnorm}}, \code{\link{sample}}.
#'
#' @examples
#' if(interactive()){
#'     library(ggplot2)
#'     library(shiny)
#'     library(ABACUS)
#'     # Run shiny app
#'     shiny_sampling()
#' }
#'
#' @import shiny
#' @import ggplot2
#' @export


# Funcition

shiny_sampling <- function() {

  shiny::runApp(appDir = system.file("app_sampling", package = "ABACUS"), launch.browser = TRUE)
  Sys.setenv("shiny_sampling" = "")

}

