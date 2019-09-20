
# Shiny ui for One-sample t-test


shinyUI(pageWithSidebar(

  headerPanel(title = div(img(src='abacus.png', align = 'left'),
                          "Hypothesis Testing: One Sample, Student's t-Test"),
              windowTitle = "Hypothesis Testing: One Sample, Student's t-Test"),

  sidebarPanel(

        tags$hr(style="border-color: purple;"),
        tags$p(style="color:blue", tags$strong("Simulation Features")),
        tags$hr(style="border-color: purple;"),


        checkboxInput(inputId = 'chkUpdate',
                      label = tags$strong('Check the box to update instantly', style="color:darkblue"),
                      value = FALSE, width = '100%'),

        actionButton(inputId = 'btnUpdate', label = 'Update'),

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

        numericInput(inputId = 'hpmean',
                     label = tags$strong('Hypothesised Population Mean: ', HTML("&mu;<sub>0"), style="color:darkblue"),
                     value = 21),


        tags$hr(style='border-color: purple;'),


        numericInput(inputId = 'psd',
                     label = tags$strong('True Population Standard Deviation: ', HTML("&sigma;"), style="color:darkblue"),
                     value=4, min = 1),


        tags$hr(style="border-color: purple;"),
        tags$p(style="color:blue", tags$strong("Sample Characteristics")),
        tags$hr(style="border-color: purple;"),


        sliderInput(inputId = 'n',
                    label = tags$strong('Sample Size', style="color:darkblue"),
                    min = 10, max = 50, value = 15, step = 1),



        tags$hr(style="border-color: purple;"),
        tags$p(style="color:blue", tags$strong("Distribution Function")),
        tags$hr(style="border-color: purple;"),


        sliderInput(inputId = 'p',
                    label = tags$strong('Type 1 Error', style="color:darkblue"),
                    min = 0.01, max = 0.10, value = 0.05, step = 0.01),


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
      tabPanel(title = 'Population',
               plotOutput(outputId = 'dnorm_plot', height = '1000px')),
      tabPanel(title = 'Sample',
               plotOutput(outputId = 'dotplot', height = '500px'),
               plotOutput(outputId = 'boxplot', height = '500px')),
      tabPanel(title = 'Test Statistic',
               plotOutput(outputId = 'mdiff_plot1', height = '250px'),
               plotOutput(outputId = 'dt_plot1', height = '750px')),
      tabPanel(title = 'Summary',
               h1("Hypothesis"),
               uiOutput('H', height = '20px'),
               tags$hr(style="border-color: purple;"),
               h2("Sample"),
               tableOutput(outputId = 'samp'),
               tags$hr(style="border-color: purple;"),
               h2("Summary Statistics"),
               tableOutput(outputId = 'sstat'),
               tags$hr(style="border-color: purple;"),
               h2("Test Statistic"),
               tableOutput(outputId = 'tstat'),
               tags$hr(style="border-color: purple;"),
               h2("Confidence Interval"),
               uiOutput('CI', height = '50'))
    )


  )


))

