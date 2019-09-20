
# Shiny global function for One-sample t-test


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

  se <- psd / sqrt(n)
  alt <- switch(p_tail,
                'lower' = 'less',
                'upper' = 'greater',
                'both' = 'two.sided')


  fm <- t.test(x = sDF$xrs,
               alternative = alt,
               mu = hpmean, paired = FALSE,
               var.equal = TRUE,
               conf.level = 0.95)

  tcal <- unname(round(fm$statistic, digits = 4))
  df1 <- unname(fm$parameter)
  pcal <- round(fm$p.value, digits = 4)
  sed <- round(fm$stderr, digits = 4)

  if(p_tail == 'lower' | p_tail == 'upper'){
    pcal <- pt(q = abs(tcal), df = df1, lower.tail = FALSE)
    ttab <- qt(p = 0.05, df = df1, lower.tail = FALSE)
  } else {
    pcal <- pt(q = abs(tcal), df = df1, lower.tail = FALSE) * 2
    ttab <- qt(p = 0.025, df = df1, lower.tail = FALSE)
  }

  mean_diff <- round((smean - hpmean), digits = 2)
  ci_mean_diff <- round(c(mean_diff - ttab*se, mean_diff + ttab*se), 2)

  xmin <- pmean - 3.5*psd
  xmax <- pmean + 3.5*psd
  norm_xlim <- c(xmin, xmax)

  tr <- rt(n = 10000, df = df1)
  t_xlim <- c(min(tr), max(tr))
  rm(tr)

  q_out <- switch(EXPR = p_tail,
                  lower = qt(p = p, df = df1, lower.tail = TRUE),
                  upper = qt(p = p, df = df1, lower.tail = FALSE),
                  both = c(qt(p = p/2, df = df1, lower.tail = TRUE),
                           qt(p = p/2, df = df1, lower.tail = FALSE)))

  tstat <- c(tcal = round(tcal, 4),
             df1 = round(df1, 4),
             pcal = round(pcal, 4),
             p = round(p, 4),
             q_out = round(q_out, 4))


  tail <- c(p_tail = p_tail)


  if(p_tail == 'lower' | p_tail == 'upper'){
    q_out_txt <- paste0('p = ', tstat['p'], '; q = ', tstat['q_out'] )
    xpos1 <- tstat['q_out']
  } else {
    q_out_txt <- paste0('p = ', tstat['p'], ';
                        q = ', round(q_out[1], 2), ', ',
                        round(q_out[2], 2) )
    xpos1 <- tstat['q_out2']
  }



  qText = q_out_txt
  annotateText <- c('pText')

  annotateDF <- data.frame(
    xpos = c(xpos1),
    ypos =  c(Inf),
    annotateText = c(qText),
    hjustvar = c(0) ,
    vjustvar = c(2)) #<- adjust


  hTitle <- paste0('H0: mu = ', hpmean, '; H1: mu <> ', hpmean)

  dTitle <- paste0( 'True Population: dnorm(x = X', ', mean = ', pmean, ', sd = ', psd, ')' )

  rTitle <- paste0( 'Sample: Mean = ', round(smean,2), ', SD = ', round(ssd,2) )

  tTitle1 <- paste0( 'Calculated t-statistic: ', round(tcal, 4),
                     ', df = ', df1, ', p-value = ', round(pcal, 4) )

  tTitle2 <- paste0('Difference = ', round(mean_diff,2),
                    '; 95% CI = ', ci_mean_diff[1], ', ', ci_mean_diff[2])


  txtTitle <- c(hTitle = hTitle, dTitle = dTitle, rTitle = rTitle,
                tTitle1 = tTitle1, tTitle2 = tTitle2)

  out <- list(sDF = sDF,
              sstat = sstat, tstat = tstat, tail = tail,
              mean_diff = mean_diff, ci_mean_diff = ci_mean_diff,
              norm_xlim = norm_xlim, t_xlim = t_xlim,
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


  dTitle1 <- bquote( 'True Population Mean & SD: ' ~
                       mu == .(pmean) ~ ', ' ~
                       sigma == .(psd) )


  dTitle2 <- 'Rugplot represents the random samples drawn from the population'

  g <- ggplot(data = NULL, mapping = aes(norm_xlim))

  g <- g + geom_area(stat = 'function', fun = dnorm,
                     args = list(mean = pmean, sd = psd), colour = 'darkred',
                     xlim = norm_xlim, fill = '#ffffff', alpha = 0.0)

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

  xmean <- sstat$smean[1]
  xsd <- sstat$ssd[1]


  rTitle <- bquote( 'Sample Mean & SD: ' ~
                      bar(x[1]) == .(round(xmean,2)) ~ ', ' ~
                      s[1] == .(round(xsd,2)) )


  scale_factor <- (norm_xlim[2] - norm_xlim[1])/100

  g <- ggplot(data = sDF, aes(x = xrs))


  g <- g + geom_dotplot(fill = 'cyan', method = 'dotdensity',
                        binwidth = scale_factor, # dotsize = 0.5,
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
# Standard t Density: Plot1 with Type 1 error


fn_dt_plot1 <- function(inputData){


  list2env(inputData, envir = environment())
  rm(inputData)


  hpmean <- unname(sstat$hpmean[1])


  p <- unname(tstat['p'])
  p_tail <- unname(tail['p_tail'])

  if(p_tail == 'both'){
    q_out <- unname(c(tstat['q_out1'], tstat['q_out2']))
  } else {
    q_out <- unname(tstat['q_out'])
  }

  tcal <- unname(tstat['tcal'])
  df1 <- unname(tstat['df1'])

  hTitle <- bquote(H[0] ~ ':' ~ mu == .(hpmean) ~ ';  ' ~ H[A] ~ ':' ~ mu != .(hpmean))
  tTitle1 <- unname(txtTitle['tTitle1'])

  g <- ggplot(data = NULL, mapping = aes(t_xlim))

  if(p_tail == 'lower'){
    t_xlim1 <- c(t_xlim[1], q_out)
    t_xlim2 <- c(q_out, t_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dt,
                       args = list(df = df1), colour = 'darkred',
                       xlim = t_xlim1, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_area(stat = 'function', fun = dt,
                       args = list(df = df1), colour = 'darkred',
                       xlim = t_xlim2, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
  }


  if(p_tail == 'upper'){
    t_xlim1 <- c(t_xlim[1], q_out)
    t_xlim2 <- c(q_out, t_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dt,
                       args = list(df = df1), colour = 'darkred',
                       xlim = t_xlim1, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_area(stat = 'function', fun = dt,
                       args = list(df = df1), colour = 'darkred',
                       xlim = t_xlim2, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
  }


  if(p_tail == 'both'){
    t_xlim1 <- c(t_xlim[1], q_out[1])
    t_xlim2 <- c(q_out[1], q_out[2])
    t_xlim3 <- c(q_out[2], t_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dt,
                       args = list(df = df1), colour = 'darkred',
                       xlim = t_xlim1, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_area(stat = 'function', fun = dt,
                       args = list(df = df1), colour = 'darkred',
                       xlim = t_xlim2, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_area(stat = 'function', fun = dt,
                       args = list(df = df1), colour = 'darkred',
                       xlim = t_xlim3, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_vline(xintercept = q_out[1], size = 1, linetype = 2, colour = 'orange')
    g <- g + geom_vline(xintercept = q_out[2], size = 1, linetype = 2, colour = 'orange')
  }


  g <- g + geom_vline(xintercept = tcal, size = 2, linetype = 1, colour = 'red')

  g <- g + labs(title = hTitle, subtitle = tTitle1, x = 'Test Statistic: t', y = 'Density')

  g <- g + geom_text(data = annotateDF[1,],
                     aes(x = xpos, y = ypos,
                         hjust = hjustvar, vjust = vjustvar,
                         label = annotateText),
                     colour = c('blue'), size = 4)


  g <- g + geom_text(data = annotateDF[1,],
                     aes(x = xpos, y = ypos,
                         hjust = hjustvar, vjust = vjustvar,
                         label = annotateText),
                     colour = c('blue'), size = 4)


  xscale <- seq(from = -5, to = 5, by = 0.5)
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

  tTitle2 <- unname(txtTitle['tTitle2'])

  mDF <- data.frame(mean_diff = mean_diff, lower = ci_mean_diff[1], upper = ci_mean_diff[2], y = 0)

  g <- ggplot(data = mDF, mapping=aes(x = mean_diff, y = y))

  g <- g + geom_errorbarh(aes(xmin = lower, xmax = upper), size = 1.5, colour = '#0000cc')

  g <- g + geom_point(size = 20, shape = 15, colour = '#ff9966')

  g <- g + labs(title = '', subtitle = tTitle2,
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

    pval <- paste0('Probability = ', tstat['p'], '; Tail: ', unname(tail))

    names(sDF) <- c('Group', 'X')
    sDF$SampleID <- 1:nrow(sDF)
    sDF <- sDF[, c('SampleID', 'Group', 'X')]

    sstat <- sstat[, 1:7]
    names(sstat) <- c('Group', 'N', 'Population Mean', 'Population SD', 'Sample Mean',  'Sample SD',  'SE')

    tstat <- as.data.frame(t(tstat))
    tstat <- tstat[,1:5]
    tstat[,5] <- abs(tstat[,5]) # only take absolute Tabulated t

    tstat$tcal = sprintf('%.4f', tstat$tcal)
    tstat$pcal= sprintf('%1.4f', tstat$pcal)

    names(tstat) <- c('Cal t', 'DF', 'Pr(>|t|)', 'Type 1 Error', 'Tabulated |t|')


    txtCI <- paste0('Mean difference & 95% CI:  ', mean_diff,
                    '    (', ci_mean_diff[1], ', ', ci_mean_diff[2], ')')
    txtCI <- tags$h4(HTML(txtCI), style="color:blue")

    rpt <- list(H = H, sDF = sDF, sstat = sstat, tstat = tstat, txtCI = txtCI)


}




#_________________________________________________________________________________________



