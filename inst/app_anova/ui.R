
# Shiny ui for ANOVA

library(shiny)

library(ggplot2)

source('global.R')

# Define UI for the application

shinyUI(pageWithSidebar(

  # Application title
  headerPanel(title = div(img(src='abacus.png', align = 'left'),
                          "Hypothesis Testing: One-way Analysis of Variance"),
              windowTitle = "Hypothesis Testing: One-way Analysis of Variance"),


  sidebarPanel(

        tags$hr(style="border-color: purple;"),
        tags$p(style="color:blue", tags$strong("Simulation Features")),
        tags$hr(style="border-color: purple;"),


        checkboxInput(inputId = 'chkUpdate',
                      label = tags$strong('Check the box to update instantly',
                                          style="color:darkblue"),
                      value = FALSE, width = '100%'),

        actionButton(inputId = 'cmdUpdate', label = 'Update'),

        tags$hr(style="border-color: green;"),


        numericInput(inputId = 'numRN',
                     label = tags$p('Seed value for generating the random number',
                                    style="color:darkblue"),
                     value = 12345, min = 1),

        tags$hr(style="border-color: purple;"),
        tags$p(style="color:blue", tags$strong("Population Parameters")),
        tags$hr(style="border-color: purple;"),


        numericInput(inputId = 'pmean1',
                     label = tags$strong('True Population 1 Mean: ',
                                         HTML("&mu;<sub>1"), style="color:darkblue"),
                     value = 20),

        numericInput(inputId = 'pmean2',
                     label = tags$strong('True Population 2 Mean: ',
                                         HTML("&mu;<sub>2"), style="color:darkblue"),
                     value = 18),

        numericInput(inputId = 'pmean3',
                     label = tags$strong('True Population 3 Mean: ',
                                         HTML("&mu;<sub>3"), style="color:darkblue"),
                     value = 19),


        tags$hr(style='border-color: green;'),


        numericInput(inputId = 'psd',
                     label = tags$strong('True Population Standard Deviation: ',
                                         HTML("&sigma;"), style="color:darkblue"),
                     value = 2, min = 1),


        tags$hr(style="border-color: purple;"),
        tags$p(style="color:blue", tags$strong("Sample Characteristics")),
        tags$hr(style="border-color: purple;"),



        sliderInput(inputId = 'n1',
                    label = tags$strong('Sample 1 (Group 1) Size: ',
                                        HTML("n<sub>1"), style="color:darkblue"),
                    min = 10, max = 50, value = 15, step = 1),


        sliderInput(inputId = 'n2',
                    label = tags$strong('Sample 2 (Group 2) Size: ',
                                        HTML("n<sub>2"), style="color:darkblue"),
                    min = 10, max = 50, value = 20, step = 1),


        sliderInput(inputId = 'n3',
                    label = tags$strong('Sample 3 (Group 3) Size: ',
                                        HTML("n<sub>3"), style="color:darkblue"),
                    min = 10, max = 50, value = 20, step = 1),


        tags$hr(style="border-color: purple;"),
        tags$p(style="color:blue", tags$strong("Distribution Function")),
        tags$hr(style="border-color: purple;"),


        sliderInput(inputId = 'p',
                    label = tags$strong('Type 1 Error', style="color:darkblue"),
                    value = 0.05, min = 0.01, max = 0.10, step = 0.01),

        radioButtons(inputId = 'p_tail',
                     label = tags$strong('Probability Tail', style="color:darkblue"),
                     choices = c('Lower tail (Left tail)' = 'lower',
                                 'Upper tail (Right tail)' = 'upper'),
                     selected = 'upper'),


        tags$hr(style="border-color: purple;")

  ),





  mainPanel(

    tabsetPanel(
      tabPanel(title = 'Population',
               plotOutput(outputId = 'dnorm_plot', height = '800')),
      tabPanel(title = 'Sample',
               plotOutput(outputId = 'dotplot', height = '400'),
               plotOutput(outputId = 'boxplot', height = '400')),
      tabPanel(title = 'SS & MS',
               plotOutput(outputId = 'ss_plot', height = '300'),
               plotOutput(outputId = 'ss_stack', height = '300'),
               plotOutput(outputId = 'ms_plot', height = '200')),
      tabPanel(title = 'Test Statistic',
               plotOutput(outputId = 'mdiff_plot1', height = '200'),
               plotOutput(outputId = 'df_plot1', height = '600')),
      tabPanel(title = 'Summary',
               h1("Hypothesis"),
               uiOutput('H', height = '20px'),
               tags$hr(style="border-color: purple;"),
               h2("Sample"),
               tableOutput(outputId = 'dt_sample'),
               tags$hr(style="border-color: purple;"),
               tags$hr(style="border-color: purple;"),
               h2("Summary Statistics"),
               tableOutput(outputId = 'dt_sstat'),
               tags$hr(style="border-color: purple;"),
               h2("Model Outputs"),
               verbatimTextOutput(outputId = 'txt_rst'),
               tags$head(tags$style("#txt_rst{overflow-y:scroll; height: 300px;}")),
               tags$hr(style="border-color: purple;"))

    )


  )




))

