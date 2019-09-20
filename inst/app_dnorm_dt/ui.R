
# Shiny ui for Normal & t distributions

source('global.R')

# Define UI for the application

shinyUI(pageWithSidebar(

  headerPanel(title = div(img(src='abacus.png', align = 'left'),
                          "Properties of Standard Normal and Student's t Distributions"),
              windowTitle = "Properties of Standard Normal and Student's t Distributions"),

  sidebarPanel(

      tags$br(),

      tags$hr(style="border-color: purple;"),
      tags$p(style="color:blue", tags$strong("Standard Normal Distribution: Parameters")),
      tags$hr(style="border-color: purple;"),

      tags$br(),

      tags$div(
        tags$span(style="color:darkred",
                  tags$strong(
                    HTML("Mean ("),
                    HTML("&mu;"),
                    HTML(") =   0")
                  )
        )
      ),

      tags$br(),

      tags$div(
        tags$span(style="color:darkred",
                  tags$strong(
                    HTML("Standard deviation ("),
                    HTML("&sigma;"),
                    HTML(") =   1")
                  )
        )
      ),


      tags$br(),
      tags$br(),



      tags$hr(style="border-color: purple;"),
      tags$p(style="color:blue", tags$strong("Student's t Distribution: Parameter")),
      tags$hr(style="border-color: purple;"),


      tags$br(),

      sliderInput(inputId = 'df',
                  label = tags$strong('Degrees of freedom: df', style="color:darkblue"),
                  value = 1, min = 1, max = 500, step = 1),


      tags$br(),

      tags$hr(style="border-color: purple;"),
      tags$p(style="color:blue", tags$strong("Probability")),
      tags$hr(style="border-color: purple;"),

      sliderInput(inputId = 'p',
                  tags$strong('Cumulative probability', style="color:darkblue"),
                  min = 0.05, max = 1, value = 0.05, step = 0.01),

      radioButtons(inputId = 'p_tail',
                   label = tags$strong('Probability Tail:', style="color:darkblue"),
                   choices = c('Lower tail (Left tail)' = 'lower',
                               'Upper tail (Right tail)' = 'upper',
                               'Both tails (Two-tailed)' = 'both'),
                   selected = 'both'),


      tags$hr(style="border-color: blue;")


  ),




  mainPanel(

    tabsetPanel(
      tabPanel(title = 'Probability Density Function',
               plotOutput(outputId = 'dnorm_dt_plot', height = '800')),
      tabPanel(title = 'Distribution & Probability',
               plotOutput(outputId = 'dnorm_plot', height = '400'),
               plotOutput(outputId = 'dt_plot', height = '400'))

    )


  )


))

