
# Shiny global functions for ANOVA

library(shiny)
library(ggplot2)


#_________________________________________________________________________________________
# Function to fit Model


fn_InputData <- function(pmean1, pmean2, pmean3,
                         psd, n1, n2, n3,
                         p, p_tail){


      xrs1 <- round(rnorm(n = n1, mean = pmean1, sd = psd), digits = 1)
      smean1 <- round(mean(xrs1, na.rm = TRUE), 2)
      ssd1 <- round(sd(xrs1, na.rm = TRUE), 2)
      sse1 <- round(ssd1 / sqrt(n1), 2)

      xrs2 <- round(rnorm(n = n2, mean = pmean2, sd = psd), digits = 1)
      smean2 <- round(mean(xrs2, na.rm = TRUE), 2)
      ssd2 <- round(sd(xrs2, na.rm = TRUE), 2)
      sse2 <- round(ssd2 / sqrt(n2), 2)

      xrs3 <- round(rnorm(n = n3, mean = pmean3, sd = psd), digits = 1)
      smean3 <- round(mean(xrs3, na.rm = TRUE), 2)
      ssd3 <- round(sd(xrs3, na.rm = TRUE), 2)
      sse3 <- round(ssd3 / sqrt(n3), 2)


      gr <- c(rep(x = 'Group 1', length = n1),
              rep(x = 'Group 2', length = n2),
              rep(x = 'Group 3', length = n3))


      sDF <- data.frame(Group = gr, xrs = c(xrs1, xrs2, xrs3))  # density not required


      sstat <- data.frame(Group = c('Group 1', 'Group 2', 'Group 3'),
                          n = c(n1, n2, n3),
                          pmean = c(pmean1, pmean2, pmean3),
                          psd = c(psd, psd, psd),
                          smean = c(smean1, smean2, smean3),
                          ssd = c(ssd1, ssd2, ssd3),
                          sse = c(sse1, sse2, sse3))

      sstat$lower <- sstat$smean - 1.96*sstat$sse
      sstat$upper <- sstat$smean + 1.96*sstat$sse


      fm <- lm(xrs ~ Group, data = sDF)
      afm <- anova(fm)

      df1 <- afm$Df[1]
      df2 <- afm$Df[2]
      fcal <- round(afm$`F value`[1], digits = 2)
      pcal <- round(afm$`Pr(>F)`[1], digits = 4)

      bMS <- afm$`Mean Sq`[1]
      wMS <- afm$`Mean Sq`[2]


      df <- c(df1, df2, n1+n2+n3-1)
      vSS = round(c(afm$`Sum Sq`, sum(afm$`Sum Sq`)), digits = 2)
      vMS <- round(c(bMS, wMS, NA), digits = 2)
      SS <- data.frame(Source = c('Between', 'Within', 'Total'), df = df, SS = vSS, MS = vMS)

      afm <- aov(xrs ~ Group, data = sDF)
      mean_diff <- as.data.frame(TukeyHSD(afm)$Group)
      names(mean_diff) <- c('mean_diff', 'lower', 'upper')
      mean_diff$comp <- c('G2 vs G1', 'G3 vs G1', 'G3 vs G2')

      xmin <- min(pmean1 - 3.5*psd, pmean2 - 3.5*psd)
      xmax <- max(pmean1 + 3.5*psd, pmean2 + 3.5*psd)
      norm_xlim <- c(xmin, xmax)

      fr <- rf(n = 10000, df1 = df1, df2 = df2)
      f_xlim <- c(min(fr), max(fr))
      rm(fr)

      q_out <- switch(EXPR = p_tail,
                      lower = qf(p = p, df1 = df1, df2 = df2, lower.tail = TRUE),
                      upper = qf(p = p, df1 = df1, df2 = df2, lower.tail = FALSE))

      fstat <- c(fcal = fcal,
                 pcal = pcal,
                 df1 = df1, df2 = df2,
                 p = p, q_out = round(q_out, 2))

      xpos1 <- fstat['q_out']

      tail <- c(p_tail = p_tail)


      q_out_txt <- paste0( '  p = ', round(p, 2), '; q = ', round(q_out, 2) )


      qText = q_out_txt
      annotateText <- c('pText')
      annotateDF <- data.frame(
        xpos = c(xpos1),
        ypos =  c(Inf),
        annotateText = c(qText),
        hjustvar = c(0) ,
        vjustvar = c(2)) #<- adjust


      hTitle <- paste0('H0: tau1 = tau2 = tau3, ; H1: At least one tau[k] <> 0')


      dTitle <- paste0( 'Population: Mean1 = ', round(pmean1,2),
                        ', Mean2 = ', round(pmean2,2),
                        ', Mean3 = ', round(pmean3,2),
                        ', SD = ', round(psd,2) )

      rTitle <- paste0( 'Sample: Mean1 = ', round(smean1,2),
                        '; Mean2 = ', round(smean2,2),
                        ', Mean3 = ', round(smean3,2),
                        '; Overall Mean = ', round(smean2,2))


      ssTitle <- paste0('Between and Within Mean Squares ( ',
                        'df = (', df1, ', ', df2, ')')


      fTitle <- paste0('Mean Squares (',
                       'Between = ', SS$MS[1],
                       '; Within = ', SS$MS[2],
                       ');  F-statistic = ', round(fcal,2),
                       ';   df = (', df1, ', ', df2, ')',
                       ';   p-value = ', sprintf('%1.2e', pcal))


      txtTitle <- c(hTitle = hTitle, dTitle = dTitle, rTitle = rTitle,
                    ssTitle = ssTitle, fTitle = fTitle)


      out <- list(sDF = sDF, sstat = sstat, fstat = fstat, tail = tail,
                  mean_diff = mean_diff, SS = SS,
                  norm_xlim = norm_xlim, f_xlim = f_xlim,
                  annotateDF = annotateDF,
                  txtTitle = txtTitle)


      return(out)


}





#_________________________________________________________________________________________

# Population density


fn_dnorm <- function(inputData){


      list2env(inputData, envir = environment())
      rm(inputData)

      pmean1 <- sstat$pmean[1]
      pmean2 <- sstat$pmean[2]
      pmean3 <- sstat$pmean[3]

      psd <- sstat$psd[1]

      dTitle1 <- bquote( 'Population Mean & SD: ' ~
                           mu[1] == .(pmean1) ~ ', ' ~
                           sigma[1] == .(psd) ~ '; ' ~
                           mu[2] == .(pmean2) ~ ', ' ~
                           sigma[2] == .(psd) ~ ', ' ~
                           mu[3] == .(pmean3) ~ ', ' ~
                           sigma[3] == .(psd))

      dTitle2 <- 'Rugplots represent the random samples drawn from three populations'

      g <- ggplot(data = NULL, mapping = aes(norm_xlim))


      g <- g + geom_area(stat = 'function', fun = dnorm,
                         args = list(mean = pmean1, sd = psd),
                         xlim = norm_xlim, fill = '#F8766D', alpha = 0.3)

      g <- g + geom_area(stat = 'function', fun = dnorm,
                         args = list(mean = pmean2, sd = psd),
                         xlim = norm_xlim, fill = '#009933', alpha = 0.3)

      g <- g + geom_area(stat = 'function', fun = dnorm,
                         args = list(mean = pmean3, sd = psd),
                         xlim = norm_xlim, fill = '#00BFC4', alpha = 0.3)

      g <- g + geom_rug(data = sDF[(sDF$Group == 'Group 1'),],
                        mapping = aes(x = xrs),
                        colour = '#F8766D', sides = 'b')

      g <- g + geom_rug(data = sDF[(sDF$Group == 'Group 2'),],
                        mapping = aes(x = xrs),
                        colour = '#009933', sides = 'b')

      g <- g + geom_rug(data = sDF[(sDF$Group == 'Group 3'),],
                        mapping = aes(x = xrs),
                        colour = '#00BFC4', sides = 'b')


      g <- g + geom_vline(xintercept = pmean1, size = 1, linetype = 2, colour = 'darkred')
      g <- g + geom_vline(xintercept = pmean2, size = 1, linetype = 2, colour = 'green')
      g <- g + geom_vline(xintercept = pmean3, size = 1, linetype = 2, colour = 'blue')

      g <- g + labs(title = dTitle1, subtitle = dTitle2,
                    x = 'Populations: X (unit)', y = 'Density')

      xscale <- seq(from = norm_xlim[1], to = norm_xlim[2], length = 21)
      xscale <- round(xscale, digits = 1)
      g <- g + scale_x_continuous(breaks = xscale, limits = norm_xlim)

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
# Sample distribution: dotplot


fn_dotplot <- function(inputData){


      list2env(inputData, envir = environment())
      rm(inputData)


      type <- 'freq'

      xmean1 <- sstat$smean[1]
      xmean2 <- sstat$smean[2]
      xmean3 <- sstat$smean[3]
      xsd1 <- sstat$ssd[1]
      xsd2 <- sstat$ssd[2]
      xsd3 <- sstat$ssd[3]

      rTitle <- bquote( 'Sample Mean & SD: ' ~
                          bar(x[1]) == .(round(xmean1,2)) ~ ', ' ~
                          s[1] == .(round(xsd1,2)) ~ '; ' ~
                          bar(x[2]) == .(round(xmean2,2)) ~ ', ' ~
                          s[2] == .(round(xsd2,2)) ~ '; ' ~
                          bar(x[3]) == .(round(xmean3,2)) ~ ', ' ~
                          s[3] == .(round(xsd3,2)) )


      xmean <- mean(sDF$xrs, na.rm = TRUE)

      scale_factor <- (norm_xlim[2] - norm_xlim[1])/100

      g <- ggplot(data = sDF, aes(x = xrs, fill = Group))


      g <- g + geom_dotplot(method = 'dotdensity',
                            binwidth = scale_factor, # dotsize = 0.3,
                            stackdir = 'centerwhole', stackratio = 0.7, alpha = 0.7)

      g <- g + scale_y_continuous(NULL, breaks = NULL)

      g <- g + geom_rug(mapping = aes(colour = Group))


      g <- g + geom_vline(xintercept = xmean, size = 1, linetype = 1, colour = 'purple')

      g <- g + geom_vline(xintercept = xmean1, size = 1, linetype = 2, colour = 'darkred')
      g <- g + geom_vline(xintercept = xmean2, size = 1, linetype = 2, colour = 'green')
      g <- g + geom_vline(xintercept = xmean3, size = 1, linetype = 2, colour = 'blue')


      g <- g + theme_bw()

      g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                                size = 14, angle = 0),
                     axis.text.y = element_text(face = 'plain', color = 'blue',
                                                size = 14, angle = 90, vjust = 0.5),
                     axis.title.x = element_text(size = 16, colour = 'purple'),
                     axis.title.y = element_text(size = 16, colour = 'purple'),
                     title = element_text(face = 'plain', color = 'blue',
                                          size = 16, angle = 0))

      g <- g + theme(legend.position = 'bottom')

      print(g)


}




#_________________________________________________________________________________________
# Sample distribution: boxplot



fn_boxplot <- function(inputData){


      list2env(inputData, envir = environment())
      rm(inputData)

      xmean1 <- sstat$smean[1]
      xmean2 <- sstat$smean[2]
      xmean3 <- sstat$smean[3]

      xsd1 <- sstat$ssd[1]
      xsd2 <- sstat$ssd[2]
      xsd3 <- sstat$ssd[3]

      rTitle <- bquote( 'Sample Mean & SD: ' ~
                          bar(x[1]) == .(round(xmean1,2)) ~ ', ' ~
                          s[1] == .(round(xsd1,2)) ~ '; ' ~
                          bar(x[2]) == .(round(xmean2,2)) ~ ', ' ~
                          s[2] == .(round(xsd2,2)) ~ '; ' ~
                          bar(x[3]) == .(round(xmean3,2)) ~ ', ' ~
                          s[3] == .(round(xsd3,2)) )

      xmean <- mean(sDF$xrs, na.rm = TRUE)

      g <- ggplot(data = sDF, aes(x = Group, y = xrs, fill = Group))


      g <- ggplot(data = sDF, aes(x = Group, y = xrs))

      g <- g + geom_boxplot(mapping = aes(colour = factor(Group), fill = factor(Group)),
                            alpha = 0.4, size = 1.0)

      g <- g + geom_jitter(mapping=aes(colour = factor(Group)),
                           width = 0.25, height = 0.001,
                           shape = 16, size=5, alpha = 0.9)

      g <- g + geom_rug(mapping = aes(colour = factor(Group)), sides = 'b')

      g <- g + geom_hline(yintercept = xmean, size = 1, linetype = 1, colour = 'purple')

      g <- g + geom_hline(yintercept = xmean1, size = 1, linetype = 2, colour = 'darkred')
      g <- g + geom_hline(yintercept = xmean2, size = 1, linetype = 2, colour = 'green')
      g <- g + geom_hline(yintercept = xmean3, size = 1, linetype = 2, colour = 'blue')

      g <- g + labs(title = rTitle, x = 'Group', y = 'Variable (unit)')

      yscale <- seq(from = norm_xlim[1], to = norm_xlim[2], length = 21)
      yscale <- round(yscale, digits = 1)
      g <- g + scale_y_continuous(breaks = yscale, limits = norm_xlim)


      g <- g + coord_flip()

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
# Standard F Density: Plot1 with Type 1 error


fn_df_plot1 <- function(inputData){


      list2env(inputData, envir = environment())
      rm(inputData)


      p_tail <- unname(tail['p_tail'])

      p <- unname(fstat['p'])
      q_out <- unname(fstat['q_out'])

      fcal <- unname(fstat['fcal'])
      df1 <- unname(fstat['df1'])
      df2 <- unname(fstat['df2'])

      hTitle <- bquote(H[0] ~ ':' ~ 'All k Group effects ' ~ tau[k] == 0 ~ ';   '
                       ~ H[A] ~ ':' ~ 'At least one Group effect ' ~ tau[k] != 0)

      fTitle <- unname(txtTitle['fTitle'])

      g <- ggplot(data = NULL, mapping = aes(f_xlim))

      if(p_tail == 'lower'){
        f_xlim1 <- c(f_xlim[1], q_out)
        f_xlim2 <- c(q_out, f_xlim[2])
        g <- g + geom_area(stat = 'function', fun = dt,
                           args = list(df = df1), colour = 'darkred',
                           xlim = f_xlim1, fill = '#ff0000', alpha = 0.5)
        g <- g + geom_area(stat = 'function', fun = dt,
                           args = list(df = df1), colour = 'darkred',
                           xlim = f_xlim2, fill = '#ffff00', alpha = 0.7)
        g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
      }


      if(p_tail == 'upper'){
        f_xlim1 <- c(f_xlim[1], q_out)
        f_xlim2 <- c(q_out, f_xlim[2])
        g <- g + geom_area(stat = 'function', fun = dt,
                           args = list(df = df1), colour = 'darkred',
                           xlim = f_xlim1, fill = '#ffff00', alpha = 0.7)
        g <- g + geom_area(stat = 'function', fun = dt,
                           args = list(df = df1), colour = 'darkred',
                           xlim = f_xlim2, fill = '#ff0000', alpha = 0.5)
        g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
      }


      g <- g + geom_vline(xintercept = fcal, size = 2, linetype = 1, colour = 'red')

      g <- g + labs(title = hTitle, subtitle = fTitle, x = 'Test Statistic: F', y = 'Density')


      g <- g + geom_text(data = annotateDF[1,],
                         aes(x = xpos, y = ypos,
                             hjust = hjustvar, vjust = vjustvar,
                             label = annotateText),
                         colour = c('blue'), size = 4)


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


fn_mean <- function(inputData){


        list2env(inputData, envir = environment())
        rm(inputData)

        mDF <- sstat

        xmean <- mean(sDF$xrs, na.rm = TRUE)

        g <- ggplot(data = mDF, mapping=aes(x = smean, y = Group, colour = Group))

        g <- g + geom_point(size = 20, shape = 15, colour = 'blue')

        g <- g + geom_errorbarh(aes(xmin = lower, xmax = upper), size = 1.5, colour = 'darkred')

        g <- g + labs(title = '', subtitle = 'Group Means & 95% CI',
                      x = 'Mean & 95% CI (unit)',
                      y = 'Group')

        g <- g + geom_vline(xintercept = xmean, size = 1.5, linetype = 2, colour = 'purple')

        g <- g + theme_bw()

        g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                                  size = 14, angle = 0),
                       axis.text.y = element_text(face = 'plain', color = 'blue',
                                                  size = 14, angle = 90, vjust = 0.5),
                       axis.title.x = element_text(size = 16, colour = 'purple'),
                       axis.title.y = element_text(size = 16, colour = 'purple'),
                       title = element_text(face = 'plain', color = 'blue',
                                            size = 16, angle = 0),
                       # axis.title.y = element_blank(),
                       # axis.text.y = element_blank(),
                       # axis.ticks.y = element_blank(),
                       # axis.ticks.length = unit(0, "pt"),
                       axis.line = element_blank(),
                       panel.grid.major = element_blank(), panel.grid.minor = element_blank())

        print(g)

}



#_________________________________________________________________________________________
# Sum of Squares


fn_SS <- function(inputData){


        list2env(inputData, envir = environment())
        rm(inputData)

        sDF <- SS
        sDF$Source <- factor(sDF$Source,
                             levels = c('Between', 'Within', 'Total'),
                             labels = c('Between', 'Within', 'Total'))

        g <- ggplot(data = sDF, mapping=aes(x = factor(Source), label = SS))

        g <- g + geom_bar(mapping = aes(weight=SS), position='dodge',
                          fill = c('#ffbf00', '#00bfff', '#669900'))

        g <- g + geom_text(mapping = aes(y = SS), size = 10, position = position_stack(vjust = 0.5))

        g <- g + labs(title = '', subtitle = 'Between, Within and Total Sum of Squares ',
                      x = 'Source',
                      y = 'Sum of Squares')


        g <- g + coord_flip()

        g <- g + theme_bw()

        g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                                  size = 14, angle = 0),
                       axis.text.y = element_text(face = 'plain', color = 'blue',
                                                  size = 10, angle = 0, vjust = 0.5),
                       axis.title.x = element_text(size = 16, colour = 'purple'),
                       axis.title.y = element_text(size = 16, colour = 'purple'),
                       title = element_text(face = 'plain', color = 'blue',
                                            size = 16, angle = 0),
                       axis.ticks.y = element_blank(),
                       axis.line = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())

        g <- g + theme(legend.position = 'bottom')

        print(g)

}



#_________________________________________________________________________________________
# Sum of Squares as Stacked: proportion

fn_SS_stack <- function(inputData){


        list2env(inputData, envir = environment())
        rm(inputData)

        sDF <- SS[1:2,]
        sDF$Source <- as.character(sDF$Source)
        sDF$Source <- factor(sDF$Source,
                             levels = c('Between', 'Within'),
                             labels = c('Between', 'Within'))
        sDF$pSS <- round(sDF$SS / sum(sDF$SS), 2)

        g <- ggplot(data = sDF, mapping=aes(x = factor('SS'), y = pSS, fill = Source, label = pSS))

        g <- g + geom_col(width = 0.3)

        g <- g + geom_text(mapping = aes(y = pSS), size = 10, position = position_stack(vjust = 0.5))

        g <- g + scale_fill_manual(values = c('#ffbf00', '#00bfff'))

        g <- g + labs(title = '', subtitle = 'Between and Within Sum of Squares ',
                      x = 'Source',
                      y = 'Sum of Squares')


        g <- g + coord_flip()

        g <- g + theme_bw()

        g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                                  size = 14, angle = 0),
                       axis.text.y = element_text(face = 'plain', color = 'blue',
                                                  size = 10, angle = 0, vjust = 0.5),
                       axis.title.x = element_text(size = 16, colour = 'purple'),
                       axis.title.y = element_text(size = 16, colour = 'purple'),
                       title = element_text(face = 'plain', color = 'blue',
                                            size = 16, angle = 0),
                       axis.line = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())

        g <- g + theme(legend.position = 'bottom')

        print(g)

}



#_________________________________________________________________________________________
# Mean Squares

fn_MS <- function(inputData){


        list2env(inputData, envir = environment())
        rm(inputData)

        sDF <- SS[1:2,]
        sDF$Source <- as.character(sDF$Source)
        sDF$Source <- factor(sDF$Source,
                             levels = c('Between', 'Within'),
                             labels = c('Between', 'Within'))

        fTitle <- unname(txtTitle['fTitle'])

        g <- ggplot(data = sDF, mapping=aes(x = factor(Source), label = MS))

        g <- g + geom_bar(mapping = aes(weight=MS), position='dodge',
                          fill = c('#ffbf00', '#00bfff'))

        g <- g + geom_text(mapping = aes(y = MS), size = 10, position = position_stack(vjust = 0.5))

        # g <- g + geom_text(data = sDF, mapping = aes(x = Source, y = 0),
        #                   label = levels(sDF$Source), position = position_stack(vjust = 0.5), size = 4)

        g <- g + labs(title = '', subtitle = fTitle,
                      x = 'Source',
                      y = 'Mean Squares')


        g <- g + coord_flip()

        g <- g + theme_bw()

        g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                                  size = 14, angle = 0),
                       axis.text.y = element_text(face = 'plain', color = 'blue',
                                                  size = 10, angle = 0, vjust = 0.5),
                       axis.title.x = element_text(size = 16, colour = 'purple'),
                       axis.title.y = element_text(size = 16, colour = 'purple'),
                       title = element_text(face = 'plain', color = 'blue',
                                            size = 16, angle = 0),
                       axis.ticks.y = element_blank(),
                       axis.line = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())


        g <- g + theme(legend.position = 'bottom')

        print(g)

}



#_________________________________________________________________________________________
# Mean Difference & CI

fn_mean_diff <- function(inputData){


        list2env(inputData, envir = environment())
        rm(inputData)

        mDF <- mean_diff

        g <- ggplot(data = mDF, mapping=aes(x = mean_diff, y = comp, colour = comp))

        g <- g + geom_errorbarh(aes(xmin = lower, xmax = upper), size = 1.5, colour = '#0000cc')

        g <- g + geom_point(size = 20, shape = 15, colour = '#ff9966')

        g <- g + labs(title = '', subtitle = 'Mean Difference between Groups & 95% CI',
                      x = 'Mean difference & 95% CI (unit)',
                      y = 'Comparisons')

        g <- g + geom_vline(xintercept = 0, size = 1.5, linetype = 2, colour = 'purple')

        g <- g + theme_bw()

        g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                                  size = 14, angle = 0),
                       axis.text.y = element_text(face = 'plain', color = 'blue',
                                                  size = 14, angle = 0, vjust = 0.5),
                       axis.title.x = element_text(size = 16, colour = 'purple'),
                       axis.title.y = element_text(size = 16, colour = 'purple'),
                       title = element_text(face = 'plain', color = 'blue',
                                            size = 16, angle = 0),
                       axis.line = element_blank(),
                       panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  print(g)

}



#_________________________________________________________________________________________
# Report preparation

fn_Report <- function(inputData){


        list2env(inputData, envir = environment())
        rm(inputData)

        H <- h4(HTML("Hypothesis: &nbsp;  &nbsp;
                       H<sub>0</sub>: &tau; <sub>1</sub> =
                                      &tau; <sub>2</sub> =
                                      &tau; <sub>3</sub> &nbsp; &nbsp;
                       H<sub>1</sub>: At least one &tau; <sub>k</sub> &ne; 0"), style="color:blue")

        pval <- paste0('Probability = ', fstat['p'], '; Tail: ', unname(tail))

        names(sDF) <- c('Group', 'X')
        sDF$SampleID <- 1:nrow(sDF)
        sDF <- sDF[, c('SampleID', 'Group', 'X')]

        sstat <- sstat[, 1:7]
        names(sstat) <- c('Group', 'N', 'Population Mean', 'Population SD',
                          'Sample Mean',  'Sample SD',  'SE')


        # Change Group level for presentation
        nDF <- sDF
        nDF$Group <- factor(nDF$Group,
                            levels = c('Group 1', 'Group 2', 'Group 3'),
                            labels = c('1', '2', '3'))
        fm1 <- lm(X ~ Group, data = nDF)
        fm2 <- aov(X ~ Group, data = nDF)
        afm <- anova(fm1)
        sfm <- summary(fm1)
        hsd <- as.data.frame(round(TukeyHSD(fm2)$Group, 4))
        names(hsd) <- c('Mean Diff', '95% LCL', '95% UCL', 'Adj P-value')
        row.names(hsd) <- c('Group 2-Group 1', 'Group 3-Group 1', 'Group 3-Group 2')

        rst <- list(ANOVA = afm, SUMMARY = sfm, `Tukey's Honest Significant Differences` = hsd)


        rpt <- list(H = H, sDF = sDF, sstat = sstat, rst = rst)


}


#_________________________________________________________________________________________


