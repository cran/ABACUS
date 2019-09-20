# Shiny ui: Sampling

shinyUI(pageWithSidebar(

  headerPanel(title = div(img(src='abacus.png', align = 'left'),
                          "Sampling Distribution"),
              windowTitle = "Sampling Distribution"),

  sidebarPanel(

      tags$hr(style="border-color: purple;"),
      tags$p(style="color:blue", tags$strong("Simulation features")),
      tags$hr(style="border-color: purple;"),


      sliderInput(inputId = 'k',
                  label = tags$strong('Number of samples', style="color:darkblue"),
                  min=10, max=1000, value=100, step = 10),


      tags$hr(style="border-color: green;"),


      checkboxInput(inputId = 'chkUpdate',
                    label = tags$strong('Check the box to update instantly', style="color:darkblue"),
                    value = FALSE, width = '100%'),

      actionButton(inputId = 'cmdUpdate', label = 'Update'),

      tags$hr(style="border-color: green;"),

      tags$br(),

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


      tags$hr(style="border-color: purple;"),
      tags$p(style="color:blue", tags$strong("Sample Characteristics")),
      tags$hr(style="border-color: purple;"),

      sliderInput(inputId = 'n',
                  label = tags$strong('Sample size', style="color:darkblue"),
                  min=2, max=500, value=50, step = 1),


      tags$hr(style="border-color: purple;")

  ),



  mainPanel(

    tabsetPanel(
      tabPanel(title = 'Population & Sample',
               plotOutput(outputId = 'dnorm_plot', height = '400'),
               plotOutput(outputId = 'dotplot1', height = '400')),
      tabPanel(title = 'Sample',
               plotOutput(outputId = 'dotplot2', height = '800'),
               textOutput(outputId = 'txt1')),
      tabPanel(title = 'Sample Estimator',
               plotOutput(outputId = 'distn_mean', height = '800'),
               textOutput(outputId = 'txt2')),
      tabPanel(title = 'Confidence Interval',
               plotOutput(outputId = 'CI_1', height = '400'),
               plotOutput(outputId = 'CI_2', height = '400'),
               textOutput(outputId = 'txtCI')),
      tabPanel(title = 'Summary',
               h2("Sample"),
               tableOutput(outputId = 'samp'),
               tags$hr(style="border-color: purple;"),
               h2("Sample Distribution"),
               uiOutput('smean', height = '50'),
               tags$hr(style="border-color: purple;"),
               h2("Confidence Interval"),
               uiOutput('CI', height = '50'),
               tags$hr(style="border-color: purple;"))

    )


  )


))

