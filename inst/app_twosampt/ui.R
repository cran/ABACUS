
# Hypothesis Testing of Means: Two Samples, Unknown Equal Variance
# Shiny ui for Two-sample t-test


# Define UI for the application


shinyUI(pageWithSidebar(

  headerPanel(title = div(img(src='abacus.png', align = 'left'),
                          "Hypothesis Testing: Two Samples, Student's t-Test"),
              windowTitle = "Hypothesis Testing: Two Samples, Student's t-Test"),


  sidebarPanel(

    tags$hr(style="border-color: purple;"),
    tags$p(style="color:blue", tags$strong("Simulation Features")),
    tags$hr(style="border-color: purple;"),


    checkboxInput(inputId = 'chkUpdate',
                  label = tags$strong('Check the box to update instantly', style="color:darkblue"),
                  value = FALSE, width = '100%'),

    actionButton(inputId = 'cmdUpdate', label = 'Update'),

    tags$hr(style="border-color: green;"),


    numericInput(inputId = 'numRN',
                 label = tags$p('Seed value for generating the random number', style="color:darkblue"),
                 value = 12345, min = 1),


    tags$hr(style="border-color: purple;"),
    tags$p(style="color:blue", tags$strong("Population Parameters")),
    tags$hr(style="border-color: purple;"),

    numericInput(inputId = 'pmean1',
                 label = tags$strong('True Population 1 Mean: ', HTML("&mu;<sub>1"), style="color:darkblue"),
                 value = 20),

    numericInput(inputId = 'pmean2',
                 label = tags$strong('True Population 2 Mean: ', HTML("&mu;<sub>2"), style="color:darkblue"),
                 value = 21),


    tags$hr(style="border-color: green;"),


    numericInput(inputId = 'psd',
                 label = tags$strong('True Population Standard Deviation: ', HTML("&sigma;"), style="color:darkblue"),
                 value = 2, min = 1),


    tags$hr(style="border-color: purple;"),
    tags$p(style="color:blue", tags$strong("Sample Characteristics")),
    tags$hr(style="border-color: purple;"),



    sliderInput(inputId = 'n1',
                label = tags$strong('Sample 1 (Group 1) Size: ', HTML("n<sub>1"), style="color:darkblue"),
                min = 10, max = 50, value = 15, step = 1),


    sliderInput(inputId = 'n2',
                label = tags$strong('Sample 2 (Group 2) Size: ', HTML("n<sub>2"), style="color:darkblue"),
                min = 10, max = 50, value = 20, step = 1),


    tags$hr(style="border-color: purple;"),
    tags$p(style="color:blue", tags$strong("Distribution Function")),
    tags$hr(style="border-color: purple;"),


    sliderInput(inputId = 'p',
                label = tags$strong('Type 1 Error', style="color:darkblue"),
                value = 0.05, min = 0.01, max = 0.10, step = 0.01),

    radioButtons(inputId = 'p_prob_type',
                 label = tags$strong('Probability Tail', style="color:darkblue"),
                 choices = c('Lower tail (Left tail)' = 'lower',
                             'Upper tail (Right tail) ' = 'upper',
                             'Both tails (Two-tailed)' = 'both'),
                 selected = 'both'),


    tags$hr(style="border-color: purple;")


  ),  # sidebarpanel




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
               tableOutput(outputId = 'dt_sample'),
               tags$hr(style="border-color: purple;"),
               tags$hr(style="border-color: purple;"),
               h2("Summary Statistics"),
               tableOutput(outputId = 'dt_sstat'),
               tags$hr(style="border-color: purple;"),
               h2("Test Statistic"),
               tableOutput(outputId = 'dt_tstat'),
               tags$hr(style="border-color: purple;"),
               h2("Confidence Interval"),
               uiOutput('CI', height = '50'))
      )


  )



))



