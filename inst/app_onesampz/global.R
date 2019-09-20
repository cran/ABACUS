
# Shiny global function for One-sample z-test


library(shiny)
library(ggplot2)

#_________________________________________________________________________________________
# Function to fit Model

fn_InputData <- function(pmean, hpmean, psd, n,
                         p, p_tail){

  xrs <- rnorm(n = n, mean = pmean, sd = psd)
  smean <- round(mean(xrs, na.rm = TRUE), 2)
  ssd <- round(sd(xrs, na.rm = TRUE), 2)
  sse <- round(ssd / sqrt(n), 2)

  sDF <- data.frame(Group = 'Group 1', xrs = xrs)  # density not required

  sstat <- data.frame(Group = 'Group 1',
                      n = n,
                      pmean = pmean,
                      hpmean = hpmean,
                      psd = psd,
                      smean = smean,
                      ssd = ssd,
                      sse = sse)

  sstat$lower <- sstat$smean - 1.96*sstat$sse
  sstat$upper <- sstat$smean + 1.96*sstat$sse


  xmin <- pmean - 3.5*psd
  xmax <- pmean + 3.5*psd
  norm_xlim <- c(xmin, xmax)

  se <- psd / sqrt(n)
  zcal <- (smean - hpmean)/se

  if(p_tail == 'lower' | p_tail == 'upper'){
    pcal <- pnorm(q = abs(zcal), mean = 0, sd = 1, lower.tail = FALSE)
    z <- qnorm(p = 0.05, mean = 0, sd = 1, lower.tail = FALSE)
  } else {
    pcal <- pnorm(q = abs(zcal), mean = 0, sd = 1, lower.tail = FALSE) * 2
    z <- qnorm(p = 0.025, mean = 0, sd = 1, lower.tail = FALSE)
  }

  mean_diff <- round((smean - hpmean), digits = 2)
  ci_mean_diff <- round(c(mean_diff - z*se, mean_diff + z*se), 2)

  q_out <- switch(EXPR = p_tail,
                  lower = qnorm(p = p, mean = 0, sd = 1, lower.tail = TRUE),
                  upper = qnorm(p = p, mean = 0, sd = 1, lower.tail = FALSE),
                  both = c(qnorm(p = p/2, mean = 0, sd = 1, lower.tail = TRUE),
                           qnorm(p = p/2, mean = 0, sd = 1, lower.tail = FALSE)))

  zstat <- c(zcal = round(zcal, 4),
             pcal = round(pcal, 4),
             p = round(p, 4),
             q_out = round(q_out, 2))

  tail <- c(p_tail = p_tail)

  if(p_tail == 'lower' | p_tail == 'upper'){
    q_out_txt <- paste0('  p = ', zstat['p'], '; q = ', zstat['q_out'] )
    xpos1 <- zstat['q_out']
  } else {
    q_out_txt <- paste0('  p = ', zstat['p'],
                        ';  q = ', round(q_out[1], 2), ', ', round(q_out[2], 2) )
    xpos1 <- zstat['q_out2']
  }


  qText = q_out_txt
  annotateText <- c('pText')
  annotateDF <- data.frame(
    xpos = c(xpos1),
    ypos =  c(Inf),
    annotateText = c(qText),
    hjustvar = c(0) ,
    vjustvar = c(2)) #<- adjust

  dTitle <- paste0( 'True Population: Mean = ', round(pmean,2), ', SD = ', round(psd,2) )

  rTitle <- paste0( 'Sample: Mean = ', round(smean,2), ', SD = ', round(ssd,2) )

  zTitle1 <- paste0( 'Calculated z-statistic: ',
                     round(zcal, 4), '; p-value = ', round(pcal, 4) )

  zTitle2 <- paste0('Difference = ', mean_diff, '; 95% CI = ', ci_mean_diff[1], ', ', ci_mean_diff[2])

  txtTitle <- c(dTitle = dTitle, rTitle = rTitle,
                zTitle1 = zTitle1, zTitle2 = zTitle2)

  out <- list(sDF = sDF,
              sstat = sstat, zstat = zstat, tail = tail,
              mean_diff = mean_diff, ci_mean_diff = ci_mean_diff,
              norm_xlim = norm_xlim,
              annotateDF = annotateDF,
              txtTitle = txtTitle)


  return(out)

}



#_________________________________________________________________________________________
# Population density


fn_dnorm1 <- function(inputData){


  list2env(inputData, envir = environment())
  rm(inputData)

  pmean <- sstat$pmean[1]
  psd <- sstat$psd[1]

  smean <- sstat$smean[1]
  ssd <- sstat$ssd[1]

  p <- unname(zstat['p'])
  p_tail <- unname(tail['p_tail'])

  if(p_tail == 'both'){
    q_out <- unname(c(zstat['q_out1'], zstat['q_out2']))
  } else {
    q_out <- unname(zstat['q_out'])
  }

  dTitle1 <- bquote( 'True Population Mean & SD: ' ~
                       mu == .(pmean) ~ ', ' ~
                       sigma == .(psd) )


  dTitle2 <- 'Rugplot represents the random samples drawn from the population'

  g <- ggplot(data = NULL, mapping = aes(norm_xlim))


  g <- g + geom_area(stat = 'function', fun = dnorm,
                     args = list(mean = pmean, sd = psd), colour = 'darkred',
                     xlim = norm_xlim, fill = '#ffffff', alpha = 0.5)


  g <- g + geom_rug(data = sDF, mapping = aes(x = xrs),
                    colour = 'blue', sides = 'b')



  g <- g + geom_vline(xintercept = pmean, size = 1, linetype = 2, colour = 'blue')

  g <- g + labs(title = dTitle1, subtitle = dTitle2,
                x = 'Populations: X (unit)', y = 'Density')


  xscale <- seq(from = norm_xlim[1], to = norm_xlim[2], length.out = 21)
  xscale <- round(xscale, digits = 1)
  g <- g + scale_x_continuous(breaks = xscale, limits = norm_xlim)


  g <- g + theme_bw()

  g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 0),
                 axis.text.y = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 90, vjust = 0.5),
                 axis.title.x = element_text(size = 16, colour = 'purple'),
                 axis.title.y = element_text(size = 16, colour = 'purple'))


  print(g)

}



#_________________________________________________________________________________________
# Sample distribution: dotplot

fn_dotplot <- function(inputData){

  list2env(inputData, envir = environment())
  rm(inputData)


  # type <- 'density'


  xmean <- sstat$smean[1]
  xsd <- sstat$ssd[1]


  rTitle <- bquote( 'Sample Mean & SD: ' ~
                      bar(x[1]) == .(round(xmean,2)) ~ ', ' ~
                      s[1] == .(round(xsd,2)) )


  # scale_factor <- max(0.5, xmean/40)
  scale_factor <- (norm_xlim[2] - norm_xlim[1])/100

  # browser()

  g <- ggplot(data = sDF, aes(x = xrs))


  g <- g + geom_dotplot(fill = 'cyan', method = 'dotdensity',
                        binwidth = scale_factor, # dotsize = 0.4,
                        stackdir = 'center', stackratio = 0.9, alpha = 0.7)

  g <- g + scale_y_continuous(NULL, breaks = NULL)

  g <- g + geom_rug(colour = 'blue')

  g <- g + geom_vline(xintercept = xmean, size = 1, linetype = 1, colour = 'purple')

  g <- g + labs(title = rTitle, x = 'Variable (unit)', y = 'Density')


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
# Sample distribution: boxplot

fn_boxplot <- function(inputData){

  list2env(inputData, envir = environment())
  rm(inputData)

  xmean <- sstat$smean[1]
  xsd <- sstat$ssd[1]


  rTitle <- bquote( 'Sample Mean & SD: ' ~
                      bar(x[1]) == .(round(xmean,2)) ~ ', ' ~
                      s[1] == .(round(xsd,2)) )

  g <- ggplot(data = sDF, aes(y = xrs, x = 1))

  g <- g + geom_boxplot(alpha = 0.4, size = 1.0, colour = '#ff9966', varwidth = TRUE)

  g <- g + geom_jitter(fill = 'cyan', width = 0.25, height = 0.001,
                       shape = 21, size = 10, alpha = 0.7)

  g <- g + geom_hline(yintercept = xmean, size = 1, linetype = 1, colour = 'purple')

  g <- g + labs(title = rTitle, x = '', y = 'Variable (unit)')


  yscale <- seq(from = norm_xlim[1], to = norm_xlim[2], length.out = 21)
  yscale <- round(yscale, digits = 1)
  g <- g + scale_y_continuous(breaks = yscale, limits = norm_xlim)

  g <- g + coord_flip()

  g <- g + geom_rug(colour = 'blue', sides = 'b')

  g <- g + theme_bw()

  g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 0),
                 axis.text.y = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 90, vjust = 0.5),
                 axis.title.x = element_text(size = 16, colour = 'purple'),
                 axis.title.y = element_text(size = 16, colour = 'purple'),
                 title = element_text(face = 'plain', color = 'blue',
                                      size = 16, angle = 0))

  g <- g + theme(legend.position = 'none')

  print(g)


}




#_________________________________________________________________________________________
# Standard Normal Density: With Type1 error


fn_dnorm_z_plot1 <- function(inputData){


  list2env(inputData, envir = environment())
  rm(inputData)

  hpmean <- unname(sstat$hpmean[1])

  p <- unname(zstat['p'])
  p_tail <- unname(tail['p_tail'])

  zcal <- unname(zstat['zcal'])
  pcal <- unname(zstat['pcal'])

  if(p_tail == 'both'){
    q_out <- unname(c(zstat['q_out1'], zstat['q_out2']))
  } else {
    q_out <- unname(zstat['q_out'])
  }

  hTitle <- bquote(H[0] ~ ':' ~ mu == .(hpmean) ~ ';  ' ~ H[A] ~ ':' ~ mu != .(hpmean))
  zTitle1 <- unname(txtTitle['zTitle1'])

  z_xlim <- c(-3.5, 3.5)

  g <- ggplot(data = NULL, mapping = aes(z_xlim))


  if(p_tail == 'lower'){
    z_xlim1 <- c(z_xlim[1], q_out)
    z_xlim2 <- c(q_out, z_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = 0, sd = 1), colour = 'darkred',
                       xlim = z_xlim1, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = 0, sd = 1), colour = 'darkred',
                       xlim = z_xlim2, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
  }


  if(p_tail == 'upper'){
    z_xlim1 <- c(z_xlim[1], q_out)
    z_xlim2 <- c(q_out, z_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = 0, sd = 1), colour = 'darkred',
                       xlim = z_xlim1, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = 0, sd = 1), colour = 'darkred',
                       xlim = z_xlim2, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
  }


  if(p_tail == 'both'){
    z_xlim1 <- c(z_xlim[1], q_out[1])
    z_xlim2 <- c(q_out[1], q_out[2])
    z_xlim3 <- c(q_out[2], z_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = 0, sd = 1), colour = 'darkred',
                       xlim = z_xlim1, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = 0, sd = 1), colour = 'darkred',
                       xlim = z_xlim2, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = 0, sd = 1), colour = 'darkred',
                       xlim = z_xlim3, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_vline(xintercept = q_out[1], size = 1, linetype = 2, colour = 'orange')
    g <- g + geom_vline(xintercept = q_out[2], size = 1, linetype = 2, colour = 'orange')
  }


  g <- g + geom_vline(xintercept = zcal, size = 2, linetype = 1, colour = 'red')

  g <- g + labs(title = hTitle, subtitle = zTitle1, x = 'Test Statistic: z', y = 'Density')


  g <- g + geom_text(data = annotateDF[1,],
                     aes(x = xpos, y = ypos,
                         hjust = hjustvar, vjust = vjustvar,
                         label = annotateText),
                     colour = c('blue'), size = 4)


  xscale <- seq(from = -3.5, to = 3.5, by = 0.5)
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
# Mean & CI

fn_mean_diff <- function(inputData){


  list2env(inputData, envir = environment())
  rm(inputData)

  zTitle2 <- unname(txtTitle['zTitle2'])

  mDF <- data.frame(mean_diff = mean_diff, lower = ci_mean_diff[1], upper = ci_mean_diff[2], y = 0)

  g <- ggplot(data = mDF, mapping=aes(x = mean_diff, y = y))

  g <- g + geom_errorbarh(aes(xmin = lower, xmax = upper), size = 1.5, colour = '#0000cc')

  g <- g + geom_point(size = 20, shape = 15, colour = '#ff9966')

  g <- g + labs(title = '', subtitle = zTitle2,
                x = 'Difference between Sample Mean & Hypothesised Mean with 95% CI', y = NULL)

  g <- g + scale_y_continuous(expand = c(0,0))

  g <- g + geom_vline(xintercept = 0, size = 1.5, linetype = 2, colour = 'purple')

  g <- g + theme_bw()

  g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 0),
                 axis.title.x = element_text(size = 16, colour = 'purple'),
                 title = element_text(face = 'plain', color = 'blue',
                                      size = 16, angle = 0),
                 axis.title.y = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 axis.ticks.length = unit(0, "pt"),
                 axis.line = element_blank(),
                 panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  print(g)

}



#_________________________________________________________________________________________
# Report preparation

fn_Report <- function(inputData){


  list2env(inputData, envir = environment())
  rm(inputData)

  p_tail <- unname(tail['p_tail'])

  txtH <- switch(EXPR = p_tail,
                 lower = paste0("Hypothesis: &nbsp;  &nbsp;
                                H<sub>0</sub>: &mu; = ", sstat$hpmean[1], "&nbsp; &nbsp;
                                H<sub>1</sub>: &mu; < ", sstat$hpmean[1]),
                 upper = paste0("Hypothesis: &nbsp;  &nbsp;
                                H<sub>0</sub>: &mu; = ", sstat$hpmean[1], "&nbsp; &nbsp;
                                H<sub>1</sub>: &mu; > ", sstat$hpmean[1]),
                 both = paste0("Hypothesis: &nbsp;  &nbsp;
                               H<sub>0</sub>: &mu; = ", sstat$hpmean[1], "&nbsp; &nbsp;
                               H<sub>1</sub>: &mu; &ne; ", sstat$hpmean[1]) )

    H <- tags$h3(HTML(txtH), style="color:blue")

    pval <- paste0('Probability = ', zstat['p'], '; Tail: ', unname(tail))

    names(sDF) <- c('Group', 'X')
    sDF$SampleID <- 1:nrow(sDF)
    sDF <- sDF[, c('SampleID', 'Group', 'X')]

    sstat <- sstat[, 1:7]
    names(sstat) <- c('Group', 'N', 'Population Mean', 'Population SD', 'Sample Mean',  'Sample SD',  'SE')

    zstat <- as.data.frame(t(zstat))
    zstat <- zstat[,1:4]
    zstat[,4] <- abs(zstat[,4]) # only take absolute Tabulated z

    zstat$zcal = sprintf('%.4f', zstat$zcal)
    zstat$pcal= sprintf('%1.4f', zstat$pcal)

    names(zstat) <- c('Cal z', 'Pr(>|z|)', 'Type 1 Error', 'Tabulated |z|')


    txtCI <- paste0('Mean difference & 95% CI:  ', mean_diff,
                    '    (', ci_mean_diff[1], ', ', ci_mean_diff[2], ')')
    txtCI <- tags$h4(HTML(txtCI), style="color:blue")

    rpt <- list(H = H, sDF = sDF, sstat = sstat, zstat = zstat, txtCI = txtCI)


}



#_________________________________________________________________________________________



