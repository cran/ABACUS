
# Global code for Sampling


library(shiny)
library(ggplot2)

#_________________________________________________________________________________________
# Function to fit Model


fn_InputData <- function(pmean, psd, n, k){

  pop <- rnorm(n = 1e05, mean = pmean, sd = psd)

  xrs <- replicate(k, sample(pop, n, replace = TRUE))

  sDF <- data.frame(xrs = xrs)  # density not required
  row.names(sDF) <- paste0('ID', 1:nrow(sDF))
  colnames(sDF) <- paste0('Sample ', 1:ncol(sDF))

  pstat <- data.frame(n = n, k = k, pmean = pmean, psd = psd)

  smean <- apply(sDF, MARGIN = 2, FUN = 'mean', na.rm = TRUE)
  ssd <- apply(sDF, MARGIN = 2, FUN = 'sd', na.rm = TRUE)

  sstat <- data.frame(n = n, smean = round(smean, 2), ssd = round(ssd, 2))

  sstat$Sample <- 1:nrow(sstat)
  sstat$se <- round(sstat$ssd / sqrt(n), 2)
  tcal <- qt(p = 0.025, df = (n-1), lower.tail = FALSE)
  sstat$lcl <- round(sstat$smean - tcal * sstat$se, 2)
  sstat$ucl <- round(sstat$smean + tcal * sstat$se, 2)

  xmin <- pmean - 3.5*psd
  xmax <- pmean + 3.5*psd
  norm_xlim <- c(xmin, xmax)

  dTitle <- paste0( 'Population: Mean = ', round(pmean,2), ', SD = ', round(psd,2) )

  sTitle1 <- paste0( 'Distribution of ', k, ' samples each with ', n, ' observations')
  sTitle2 <- paste0( 'Distribution of means of ', k, ' samples each with ', n, ' observations')

  txtTitle <- c(dTitle = dTitle, sTitle1 = sTitle1, sTitle2 = sTitle2)

  out <- list(sDF = sDF, tail = tail,
              pstat = pstat, sstat = sstat,
              norm_xlim = norm_xlim,
              txtTitle = txtTitle)

  return(out)


}


#_________________________________________________________________________________________
# Normal distribution: Density plot

fn_dnorm <- function(inputData){


  list2env(inputData, envir = environment())
  rm(inputData)

  pmean <- pstat$pmean[1]
  psd <- pstat$psd[1]


  dTitle <- bquote( 'Normal Distribution: ' ~
                      mu == .(pmean) ~ ', ' ~
                      sigma == .(psd))


  g <- ggplot(data = NULL, mapping = aes(norm_xlim))

  g <- g + geom_area(stat = 'function', fun = dnorm,
                     args = list(mean = pmean, sd = psd),
                     xlim = norm_xlim, fill = '#ffff00', alpha = 0.3)

  g <- g + geom_vline(xintercept = pmean, size = 1, linetype = 2, colour = 'darkred')


  g <- g + labs(title = dTitle, x = 'Variable X (unit)', y = 'Density')


  xscale <- seq(from = norm_xlim[1], to = norm_xlim[2], length.out = 21)
  xscale <- round(xscale, digits = 1)
  g <- g + scale_x_continuous(breaks = xscale)

  g <- g + theme_bw()

  g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 0),
                 axis.text.y = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 90, vjust = 0.5),
                 axis.title.x = element_text(size = 16, colour = 'purple'),
                 axis.title.y = element_text(size = 16, colour = 'purple'),
                 title = element_text(face = 'plain', color = 'blue',
                                      size = 16, angle = 0))


  print(g)

}


#_________________________________________________________________________________________
# Sample distribution (Single Sample): dotplot

fn_dotplot1 <- function(inputData){

  list2env(inputData, envir = environment())
  rm(inputData)

  n <- pstat$n[1]

  xmean <- sstat$smean[1]
  xsd <- sstat$ssd[1]

  sDF <- sDF[,1, drop=FALSE]
  names(sDF) <- 'Sample'


  scale_factor <- (norm_xlim[2] - norm_xlim[1])/100

  sTitle <- bquote('Sample 1: ' ~
                     bar(x[1]) == .(round(xmean,2)) ~ ', ' ~
                     s[1] == .(round(xsd,2)) )


  g <- ggplot(data = sDF[,1, drop=FALSE], aes_string(x = 'Sample'))


  g <- g + geom_dotplot(fill = 'cyan', method = 'dotdensity',
                        binwidth = scale_factor, # dotsize = xdotsize,
                        stackratio = 0.7, alpha = 0.3)

  g <- g + scale_y_continuous(NULL, breaks = NULL)

  g <- g + geom_rug(colour = 'blue')

  g <- g + geom_vline(xintercept = xmean, size = 1, linetype = 1, colour = 'purple')

  g <- g + labs(title = sTitle, x = 'Variable X (unit)', y = 'Density')


  g <- g + theme_bw()

  g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 0),
                 axis.text.y = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 90, vjust = 0.5),
                 axis.title.x = element_text(size = 16, colour = 'purple'),
                 axis.title.y = element_text(size = 16, colour = 'purple'),
                 title = element_text(face = 'plain', color = 'blue',
                                      size = 16, angle = 0))
  print(g)


}


#_________________________________________________________________________________________
# Sample distribution (8 Samples): dotplot

fn_dotplot2 <- function(inputData){

  list2env(inputData, envir = environment())
  rm(inputData)

  n <- pstat$n[1]
  sDF <- sDF[,1:8]

  sstat <- sstat[1:8,]
  sstat$Sample <- row.names(sstat)

  sampID <- names(sDF)

  sTitle <- unname(txtTitle['sTitle1'])

  DF <- reshape(data = sDF,
                varying = sampID,
                v.names = 'x',
                times = sampID,
                timevar = 'Sample',
                idvar = c('ID'),
                drop = NULL,
                direction = 'long',
                new.row.names = 1:(length(sampID)*nrow(sDF)))


  xmean <- sstat$smean
  xmin <- min(DF$x)

  txt <- paste0('mean = ', sstat$smean, '\nsd = ', sstat$ssd)
  aDF <- data.frame(
    Sample = sampID,
    x = xmin, y = Inf,
    aText = txt,
    hjustvar = 0 ,
    vjustvar = 2
  )

  scale_factor <- (norm_xlim[2] - norm_xlim[1])/50

  g <- ggplot(data = DF, aes(x = x))


  g <- g + geom_dotplot(fill = 'cyan', method = 'dotdensity',
                        binwidth = scale_factor, # dotsize = 0.8,
                        stackratio = 0.7, alpha = 0.3)

  g <- g + scale_y_continuous(NULL, breaks = NULL)

  g <- g + geom_rug(colour = 'blue')

  g <- g + facet_wrap( ~ Sample, nrow = 2)


  g <- g + geom_text(data = aDF,
                     aes(x = x, y = y,
                         hjust = hjustvar, vjust = vjustvar,
                         label = aText),
                     size = 4, colour = 'blue')

  g <- g + geom_vline(data = sstat,
                      mapping = aes(xintercept = smean),
                      size = 1, linetype = 1, colour = 'purple')


  g <- g + labs(title = sTitle, x = 'Variable X (unit)', y = 'Density')


  g <- g + theme_bw()

  g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 0),
                 axis.text.y = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 90, vjust = 0.5),
                 axis.title.x = element_text(size = 16, colour = 'purple'),
                 axis.title.y = element_text(size = 16, colour = 'purple'),
                 title = element_text(face = 'plain', color = 'blue',
                                      size = 16, angle = 0))

  print(g)


}


#_________________________________________________________________________________________
# Distribution of Sample Mean: Histogram and Density plot


fn_distn_mean <- function(inputData){

  list2env(inputData, envir = environment())
  rm(inputData)

  n <- pstat$n[1]
  bins <- ceiling(1 + 3.322*log(n))
  sstat$xrs <- sstat$smean

  mean_xbar <- round(mean(sstat$smean), 2)
  sd_xbar <- round(sd(sstat$smean), 2)
  mean_ssd <- round(mean(sstat$ssd), 2)

  s_xlim <- c(floor(min(sstat$smean)), ceiling(max(sstat$smean)))

  rTitle <- bquote( 'Mean of Sample Mean (' ~
                      bar(x) ~')' == .(mean_xbar) ~ ', ' ~
                      'SD of ' ~
                      bar(x) == .(sd_xbar) ~ ', ' ~
                      'Mean of Sample SD (s)'
                    == .(mean_ssd) )



  g <- ggplot(data = sstat, aes(x = xrs))

  g <- g + geom_histogram(mapping = aes(x = xrs, y =..density..),
                          bins = bins, colour = 'purple', fill = 'orange')

  g <- g + geom_density(mapping = aes(x = xrs,
                                      y =..density..),
                        n = 1000, size = 1, colour = 'red')

  g <- g + geom_rug(colour = 'blue')

  g <- g + labs(title = rTitle, x = 'Sample Mean (unit)', y = 'Density')

  xscale <- seq(from = s_xlim[1], to = s_xlim[2], length.out = 21)
  xscale <- round(xscale, digits = 1)
  g <- g + scale_x_continuous(breaks = xscale, limits = s_xlim)

  g <- g + theme_bw()

  g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 0),
                 axis.text.y = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 90, vjust = 0.5),
                 axis.title.x = element_text(size = 16, colour = 'purple'),
                 axis.title.y = element_text(size = 16, colour = 'purple'))

  g <- g + theme(legend.position = 'bottom')

  print(g)

}


#_________________________________________________________________________________________
# Sample distribution: Confidenc Interval

fn_CI_1 <- function(inputData){

  list2env(inputData, envir = environment())
  rm(inputData)

  pmean <- pstat$pmean[1]

  n <- pstat$n[1]
  k <- pstat$k[1]

  index <- which(sstat$lcl > pmean | sstat$ucl < pmean)
  pCI <- round(100*(k - length(index))/k, digits = 2)


  rTitle <- paste0(pCI, '% of estimated 95%CI of means obtained from ', k,
                   ' random samples of size ', n,
                   ' include the True Population Mean ', pmean)


  g <- ggplot(data = sstat, aes(x=Sample, y=smean))

  g <- g + geom_point(shape=16, size=3, colour='blue')

  g <- g + geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.5, colour='purple')

  g <- g + geom_hline(yintercept = pmean, colour='red', size=1.5)

  xscale <- seq(from = 1, to = nrow(sstat), by = 10)
  g <- g + scale_x_continuous(breaks = xscale)

  g <- g + labs(title = rTitle, x = 'Sample', y = 'Mean')

  g <- g + theme_bw()

  g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 90, hjust = 1),
                 axis.text.y = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 90, vjust = 0.5),
                 axis.title.x = element_text(size = 16, colour = 'purple'),
                 axis.title.y = element_text(size = 16, colour = 'purple'))

  g <- g + theme(legend.position = 'bottom')

  print(g)

}



#_________________________________________________________________________________________
# Sample distribution: ordered Confidenc Interval

fn_CI_2 <- function(inputData){

  list2env(inputData, envir = environment())
  rm(inputData)

  pmean <- pstat$pmean[1]

  n <- pstat$n[1]
  k <- pstat$k[1]

  index <- order(sstat$smean, decreasing = FALSE)
  sstat <- sstat[index,]

  sstat$Sample <- 1:nrow(sstat)

  index <- which(sstat$lcl > pmean | sstat$ucl < pmean)
  pCI <- round(100*(k - length(index))/k, digits = 2)


  rTitle <- paste0(pCI, '% of estimated 95%CI of means obtained from ', k,
                   ' random samples of size ', n,
                   ' include the True Population Mean ', pmean)


  g <- ggplot(data = sstat, aes(x=Sample, y=smean))

  g <- g + geom_point(shape=16, size=3, colour='blue')

  g <- g + geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.5, colour='purple')

  g <- g + geom_hline(yintercept = pmean, colour='red', size=1.5)

  xscale <- seq(from = 1, to = nrow(sstat), by = 10)
  g <- g + scale_x_continuous(breaks = xscale)

  g <- g + labs(title = rTitle, x = 'Ordered Sample', y = 'Mean')

  g <- g + theme_bw()

  g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 90, hjust = 1),
                 axis.text.y = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 90, vjust = 0.5),
                 axis.title.x = element_text(size = 16, colour = 'purple'),
                 axis.title.y = element_text(size = 16, colour = 'purple'))

  g <- g + theme(legend.position = 'bottom')

  print(g)

}



#_________________________________________________________________________________________
# Report preparation


fn_Report <- function(inputData){


  list2env(inputData, envir = environment())
  rm(inputData)


  sstat <- sstat[, c('Sample','n','smean','ssd','se','lcl','ucl')]
  sstat$Sample <- paste0('Sample ', sstat$Sample)


  mean_xbar <- round(mean(sstat$smean), 2)
  sd_xbar <- round(sd(sstat$smean), 2)
  mean_ssd <- round(mean(sstat$ssd), 2)

  txtMean <- paste0("Mean of Sample Mean (x&#772;): &nbsp;", mean_xbar,
                    "; &nbsp; &nbsp; SD of x&#772;: &nbsp;", sd_xbar,
                    "; &nbsp; &nbsp; Mean of Sample SD (s): &nbsp;", mean_ssd)

  txtMean <- h4(HTML(txtMean), style="color:blue")


  pmean <- pstat$pmean[1]
  n <- pstat$n[1]
  k <- pstat$k[1]
  index <- which(sstat$lcl > pmean | sstat$ucl < pmean)
  pCI <- round(100*(k - length(index))/k, digits = 2)


  txtCI <- paste0(pCI, '% of estimated 95%CI of means obtained from ', k,
                  ' random samples of size ', n,
                  ' include the True Population Mean ', pmean)
  txtCI <- h4(HTML(txtCI), style="color:blue")


  names(sstat) <- c('Sample ID', 'Sample Size',
                    'Sample Mean', 'Sample SD',
                    'SE', '95% LCL',  '95% UCL')


  rpt <- list(sstat = sstat, txtMean = txtMean, txtCI = txtCI)


}




#_________________________________________________________________________________________




#_________________________________________________________________________________________


