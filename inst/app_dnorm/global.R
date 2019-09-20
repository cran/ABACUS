
# Shiny global function for Normal distribution

library(shiny)
library(ggplot2)


#_________________________________________________________________________________________

# Function to fit Model

fn_InputData <- function(pmean, psd, n, bins, type,
                         p, p_tail, cs_xscale){



  xrs <- rnorm(n = n, mean = pmean, sd = psd)
  smean <- round(mean(xrs, na.rm = TRUE), 2)
  ssd <- round(sd(xrs, na.rm = TRUE), 2)
  sse <- round(ssd / sqrt(n), 2)


  sDF <- data.frame(xrs = xrs)  # density not required

  sstat <- data.frame(n = n,
                      pmean = pmean,
                      psd = psd,
                      bins = bins,
                      smean = smean,
                      ssd = ssd,
                      sse = sse)

  sstat$lower <- sstat$smean - 1.96*sstat$sse
  sstat$upper <- sstat$smean + 1.96*sstat$sse

  xmin <- pmean - 3.5*psd
  xmax <- pmean + 3.5*psd
  norm_xlim <- c(xmin, xmax)

  q_out <- switch(EXPR = p_tail,
                  lower = qnorm(p = p, mean = pmean, sd = psd, lower.tail = TRUE),
                  upper = qnorm(p = p, mean = pmean, sd = psd, lower.tail = FALSE),
                  both = c(qnorm(p = p/2, mean = pmean, sd = psd, lower.tail = TRUE),
                           qnorm(p = p/2, mean = pmean, sd = psd, lower.tail = FALSE)))

  zstat <- c(p = p,  q_out = round(q_out, 2))

  tail <- c(p_tail = p_tail)

  if(p_tail == 'lower' | p_tail == 'upper'){
    q_out_txt <- paste0('  p = ', zstat['p'], '; q = ', zstat['q_out'] )
    xpos1 <- zstat['q_out']
  } else {
    q_out_txt <- paste0('  p = ', zstat['p'],
                        ';  q = ', round(q_out[1], 2), ', ', round(q_out[2], 2) )
    xpos1 <- zstat['q_out1']
  }


  qText = q_out_txt
  annotateText <- c('pText')
  annotateDF <- data.frame(
    xpos = c(xpos1),
    ypos =  c(Inf),
    annotateText = c(qText),
    hjustvar = c(0) ,
    vjustvar = c(2)) #<- adjust

  dTitle <- paste0( 'Population: Mean = ', round(pmean,2), ', SD = ', round(psd,2) )

  rTitle <- paste0( 'Sample: Mean = ', round(smean,2), ', SD = ', round(ssd,2) )

  txtTitle <- c(dTitle = dTitle, rTitle = rTitle)


  out <- list(sDF = sDF, sstat = sstat, zstat = zstat,
              type = type, tail = tail,
              norm_xlim = norm_xlim,
              annotateDF = annotateDF,
              txtTitle = txtTitle,
              cs_xscale = cs_xscale)


  return(out)


}


#_________________________________________________________________________________________
# Sample distribution: Histogram and Density plot


fn_rnorm <- function(inputData){

  list2env(inputData, envir = environment())
  rm(inputData)

  pmean <- sstat$pmean[1]
  psd <- sstat$psd[1]

  smean <- sstat$smean[1]
  ssd <- sstat$ssd[1]

  bins <- sstat$bins[1]

  rTitle <- bquote( 'Population Mean & SD: ' ~
                      mu == .(pmean) ~ ', ' ~
                      sigma == .(psd) ~ '; ' ~
                      'Sample Mean & SD: ' ~
                      bar(x) == .(smean) ~ ', ' ~
                      s == .(ssd) )

  g <- ggplot(data = sDF, aes(x = xrs))

  if(type == 'freq'){
    g <- g + geom_histogram(bins = bins,
                            colour = 'purple', fill = 'darkolivegreen1')
  } else {
    g <- g + geom_histogram(mapping = aes(x = xrs, y =..density.., ),
                            bins = bins, colour = 'purple', fill = 'darkolivegreen1')
    g <- g + geom_density(mapping = aes(x = xrs, y =..density.., colour = 'Empirical Distribution'),
                          n = 1000, size = 1)
    g <- g + stat_function(fun = dnorm, mapping = aes(colour = 'Theoretical Normal Distribution'),
                           args = list(mean = pmean, sd = psd),
                           xlim = norm_xlim, n = 1000, geom = 'line', size = 1)
    g <- g + scale_colour_manual(name = 'Density', values = c('red', 'blue'))
  }

  if(type == 'freq'){
    g <- g + labs(title = rTitle, x = 'X', y = 'Frequency')
  } else {
    g <- g + labs(title = rTitle, x = 'X', y = 'Density')
  }


  g <- g + geom_rug(colour = '#F8766D', sides = 'b')

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
# Normal distribution: Density plot

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

  p_out <- unname(zstat['p_out'])

  dTitle <- bquote( 'Population Mean & SD: ' ~
                      mu == .(pmean) ~ ', ' ~
                      sigma == .(psd) )


  g <- ggplot(data = NULL, mapping = aes(norm_xlim))


  if(p_tail == 'lower'){
    norm_xlim1 <- c(norm_xlim[1], q_out)
    norm_xlim2 <- c(q_out, norm_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim1, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim2, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
  }


  if(p_tail == 'upper'){
    norm_xlim1 <- c(norm_xlim[1], q_out)
    norm_xlim2 <- c(q_out, norm_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim1, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim2, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
  }


  if(p_tail == 'both'){
    norm_xlim1 <- c(norm_xlim[1], q_out[1])
    norm_xlim2 <- c(q_out[1], q_out[2])
    norm_xlim3 <- c(q_out[2], norm_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim1, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim2, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim3, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_vline(xintercept = q_out[1], size = 1, linetype = 2, colour = 'orange')
    g <- g + geom_vline(xintercept = q_out[2], size = 1, linetype = 2, colour = 'orange')
  }


  g <- g + labs(title = dTitle, x = 'Populations: X (unit)', y = 'Density')


  g <- g + geom_text(data = annotateDF[1,],
                     aes(x = xpos, y = ypos,
                         hjust = hjustvar, vjust = vjustvar,
                         label = annotateText),
                     colour = c('blue'), size = 4)


  if(pmean == 0 & psd == 1){
    g <- g + labs(title = dTitle, x = 'z', y = 'Density')
  } else {
    g <- g + labs(title = dTitle, x = 'X', y = 'Density')
  }


  yval <- mean(dnorm(x = pmean, mean = pmean, sd = psd))/2


  xscale <- seq(from = norm_xlim[1], to = norm_xlim[2], length = 21)
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
# Normal distribution: Density plot (Center & Scale)

fn_dnorm2 <- function(inputData){


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

  p_out <- unname(zstat['p_out'])

  dTitle <- bquote( 'Population Mean & SD: ' ~
                      mu == .(pmean) ~ ', ' ~
                      sigma == .(psd) )


  g <- ggplot(data = NULL, mapping = aes(norm_xlim))


  if(p_tail == 'lower'){
    norm_xlim1 <- c(norm_xlim[1], q_out)
    norm_xlim2 <- c(q_out, norm_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim1, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim2, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
  }


  if(p_tail == 'upper'){
    norm_xlim1 <- c(norm_xlim[1], q_out)
    norm_xlim2 <- c(q_out, norm_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim1, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim2, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
  }


  if(p_tail == 'both'){
    norm_xlim1 <- c(norm_xlim[1], q_out[1])
    norm_xlim2 <- c(q_out[1], q_out[2])
    norm_xlim3 <- c(q_out[2], norm_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim1, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim2, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim3, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_vline(xintercept = q_out[1], size = 1, linetype = 2, colour = 'orange')
    g <- g + geom_vline(xintercept = q_out[2], size = 1, linetype = 2, colour = 'orange')
  }

  g <- g + labs(title = dTitle, x = 'Populations: X (unit)', y = 'Density')


  g <- g + theme_bw()

  g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 0),
                 axis.text.y = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 90, vjust = 0.5),
                 axis.title.x = element_text(size = 16, colour = 'purple'),
                 axis.title.y = element_text(size = 16, colour = 'purple'),
                 title = element_text(face = 'plain', color = 'blue',
                                      size = 16, angle = 0))


  g <- g + geom_text(data = annotateDF[1,],
                     aes(x = xpos, y = ypos,
                         hjust = hjustvar, vjust = vjustvar,
                         label = annotateText),
                     colour = c('blue'), size = 4)


  if(pmean == 0 & psd == 1){
    g <- g + labs(title = dTitle, x = 'z', y = 'Density')
  } else {
    g <- g + labs(title = dTitle, x = 'X', y = 'Density')
  }


  yval <- mean(dnorm(x = pmean, mean = pmean, sd = psd))/2

  g <- g + xlim(cs_xscale)

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
# Normal distribution: Density plot (No shading)

fn_dnorm3 <- function(inputData){


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

  p_out <- unname(zstat['p_out'])

  dTitle <- bquote( 'Population Mean & SD: ' ~
                      mu == .(pmean) ~ ', ' ~
                      sigma == .(psd) )


  g <- ggplot(data = NULL, mapping = aes(norm_xlim))


  if(p_tail == 'lower'){
    norm_xlim1 <- c(norm_xlim[1], q_out)
    norm_xlim2 <- c(q_out, norm_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim1, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim2, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
  }


  if(p_tail == 'upper'){
    norm_xlim1 <- c(norm_xlim[1], q_out)
    norm_xlim2 <- c(q_out, norm_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim1, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim2, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
  }


  if(p_tail == 'both'){
    norm_xlim1 <- c(norm_xlim[1], q_out[1])
    norm_xlim2 <- c(q_out[1], q_out[2])
    norm_xlim3 <- c(q_out[2], norm_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim1, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim2, fill = '#ffffff', alpha = 0.7) # No fill
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim3, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_vline(xintercept = q_out[1], size = 1, linetype = 2, colour = 'orange')
    g <- g + geom_vline(xintercept = q_out[2], size = 1, linetype = 2, colour = 'orange')
  }

  g <- g + labs(title = dTitle, x = 'Populations: X (unit)', y = 'Density')


  g <- g + geom_text(data = annotateDF[1,],
                     aes(x = xpos, y = ypos,
                         hjust = hjustvar, vjust = vjustvar,
                         label = annotateText),
                     colour = c('blue'), size = 4)


  if(pmean == 0 & psd == 1){
    g <- g + labs(title = dTitle, x = 'z', y = 'Density')
  } else {
    g <- g + labs(title = dTitle, x = 'X', y = 'Density')
  }


  yval <- mean(dnorm(x = pmean, mean = pmean, sd = psd))/2

  xscale <- seq(from = norm_xlim[1], to = norm_xlim[2], length = 21)
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
# Normal distribution: Cumulative probability distribution plot


fn_pnorm <- function(inputData){



  list2env(inputData, envir = environment())
  rm(inputData)

  pmean <- sstat$pmean[1]
  psd <- sstat$psd[1]

  p <- unname(zstat['p'])
  p_tail <- unname(tail['p_tail'])

  if(p_tail == 'both'){
    q_out <- unname(c(zstat['q_out1'], zstat['q_out2']))
  } else {
    q_out <- unname(zstat['q_out'])
  }

  p_out <- unname(zstat['p_out'])

  dTitle <- bquote( 'Population Mean & SD: ' ~
                      mu == .(pmean) ~ ', ' ~
                      sigma == .(psd) )


  DF <- data.frame(xr = rnorm(n = 10000, mean = pmean, sd = psd))

  g <- ggplot(data = DF, aes(x = xr))

  g <- g + stat_function(fun = pnorm,
                         args = list(mean = pmean, sd = psd, lower.tail = TRUE),
                         xlim = norm_xlim, geom = 'line',
                         color = 'darkred', size = 1)

  if(p_tail == 'lower' | p_tail == 'upper'){
    g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
  }


  if(p_tail == 'both'){
    g <- g + geom_vline(xintercept = q_out[1], size = 1, linetype = 2, colour = 'orange')
    g <- g + geom_vline(xintercept = q_out[2], size = 1, linetype = 2, colour = 'orange')
  }

  g <- g + geom_text(data = annotateDF[1,],
                     aes(x = xpos, y = ypos,
                         hjust = hjustvar, vjust = 4,
                         label = annotateText),
                     colour = c('blue'), size = 4)


  if(pmean == 0 & psd == 1){
    g <- g + labs(title = dTitle, x = 'z', y = 'Cumulative Probability')
  } else {
    g <- g + labs(title = dTitle, x = 'X', y = 'Cumulative Probability')

  }

  pexp <- ''
  g <- g + geom_text(data = annotateDF[1,],
                     aes(x = pmean, y = 0.8),
                     label = pexp, parse = TRUE, size = 8, colour = 'blue')


  xscale <- seq(from = norm_xlim[1], to = norm_xlim[2], length = 21)
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



