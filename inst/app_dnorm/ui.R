
# Shiny ui for Normal distribution

source('global.R')

# Define UI for the application

shinyUI(pageWithSidebar(

  headerPanel(title = div(img(src='abacus.png', align = 'left'),
                          "Normal Distribution"),
              windowTitle = "Normal Distribution"),

  sidebarPanel(

      tags$hr(style="border-color: purple;"),
      tags$p(style="color:blue", tags$strong("Simulation Features")),
      tags$hr(style="border-color: purple;"),


      checkboxInput(inputId = 'chkUpdate',
                    label = tags$strong('Check the box to update instantly', style="color:darkblue"),
                    value = FALSE, width = '100%'),

      actionButton(inputId = 'cmdUpdate', label = 'Update'),


      tags$hr(style="border-color: purple;"),

      numericInput(inputId = 'numRN',
                   label = tags$p('Seed value for generating the random number', style="color:darkblue"),
                   value = 12345, min = 1),

      tags$hr(style="border-color: purple;"),
      tags$p(style="color:blue", tags$strong("Population Parameters")),
      tags$hr(style="border-color: purple;"),


      numericInput(inputId = 'pmean',
                   label = tags$strong('True Population Mean: ', HTML("&mu;"), style="color:darkblue"),
                   value = 20),

      numericInput(inputId = 'psd',
                   label = tags$strong('True Population Standard Deviation: ', HTML("&sigma;"), style="color:darkblue"),
                   value = 4, min = 1),


      sliderInput(inputId = 'cs_xscale',
                  label = tags$p('X-axis scale for the center and scale effect', style="color:darkblue"),
                  min = -20, max = 1000, value = c(-20,60), step = 20),

      tags$hr(style="border-color: purple;"),
      tags$p(style="color:blue", tags$strong("Sample Characteristics")),
      tags$hr(style="border-color: purple;"),


      sliderInput(inputId = 'n',
                  label = tags$strong('Sample: Number of observations', style="color:darkblue"),
                  value = 50, min = 0, max = 10000),

      sliderInput(inputId = 'bins',
                  label = tags$strong('Number of bins', style="color:darkblue"),
                  value = 20, min = 1, max = 1000),


      radioButtons(inputId = 'type',
                   label = tags$strong('Plot type:', style="color:darkblue"),
                   choices = c('Frequency Distribtion ' = 'freq',
                               'Overlay Normal Density' = 'density')),


      tags$hr(style="border-color: purple;"),
      tags$p(style="color:blue", tags$strong("Distribution Function")),
      tags$hr(style="border-color: purple;"),

      sliderInput(inputId = 'p',
                  label = tags$strong('Cumulative probability', style="color:darkblue"),
                  min = 0.01, max = 1, value = 0.05, step = 0.01),

      radioButtons(inputId = 'p_tail',
                   label = tags$strong('Probability Tail', style="color:darkblue"),
                   choices = c('Lower tail (Left tail)' = 'lower',
                               'Upper tail (Right tail) ' = 'upper',
                               'Both tails (Two-tailed)' = 'both'),
                   selected = 'both'),


      tags$hr(style="border-color: purple;")


  ),




  mainPanel(

    tabsetPanel(
      tabPanel(title = 'Sample',
               plotOutput(outputId = 'rnorm_plot', height = '800')),
      tabPanel(title = 'Distribution',
               plotOutput(outputId = 'dnorm1_plot', height = '400'),
               plotOutput(outputId = 'dnorm2_plot', height = '400')),
      tabPanel(title = 'Probability & Quantile',
               plotOutput(outputId = 'pnorm_plot', height = '400'),
               plotOutput(outputId = 'dnorm3_plot', height = '400'))

    )


  )


))

