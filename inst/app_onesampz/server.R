
# Shiny server for One-sample z-test

#_________________________________________________________________________________________

shinyServer(function(input, output) {

#_________________________________________________________________________________________

# eventReactive----


inputVal_eventReactive <- eventReactive(input$cmdUpdate, {

  iseed <- input$numRN + as.integer(input$cmdUpdate)

  pmean <- input$pmean
  hpmean <- input$hpmean
  psd <- input$psd
  n <- input$n

  p <- input$p  # user probability
  p_tail <- input$p_tail


  fn_InputData_repeatable <- repeatable(rngfunc = fn_InputData, seed = iseed)

  fn_InputData_repeatable(pmean = pmean, hpmean = hpmean,
                          psd = psd, n = n,
                          p = p, p_tail = p_tail)



})



#_________________________________________________________________________________________

# reactive----

inputVal_reactive <- reactive({

  pmean <- input$pmean
  hpmean <- input$hpmean
  psd <- input$psd
  n <- input$n

  p <- input$p  # user probability
  p_tail <- input$p_tail


  fn_InputData(pmean = pmean, hpmean = hpmean,
               psd = psd, n = n,
               p = p, p_tail = p_tail)


})


#_________________________________________________________________________________________



output$dnorm_plot <- renderPlot({

  if(input$chkUpdate){

    fn_dnorm1(inputVal_reactive())

  } else {

    fn_dnorm1(inputVal_eventReactive())

  }


})



output$dotplot <- renderPlot({

  if(input$chkUpdate){

    fn_dotplot(inputVal_reactive())

  } else {

    fn_dotplot(inputVal_eventReactive())

  }


})




output$boxplot <- renderPlot({

  if(input$chkUpdate){

    fn_boxplot(inputVal_reactive())

  } else {

    fn_boxplot(inputVal_eventReactive())

  }


})



output$mdiff_plot1 <- renderPlot({

  if(input$chkUpdate){

    fn_mean_diff(inputVal_reactive())

  } else {

    fn_mean_diff(inputVal_eventReactive())

  }


})



output$dnorm_z_plot1 <- renderPlot({

  if(input$chkUpdate){

    fn_dnorm_z_plot1(inputVal_reactive())

  } else {

    fn_dnorm_z_plot1(inputVal_eventReactive())

  }


})



#_________________________________________________________________________________________


output$H <- renderUI({
  if(input$chkUpdate){
    fn_Report(inputVal_reactive())[['H']]
  } else {
    fn_Report(inputVal_eventReactive())[['H']]
  }
})



sample <- renderTable({
  if(input$chkUpdate){
    fn_Report(inputVal_reactive())[['sDF']]
  } else {
    fn_Report(inputVal_eventReactive())[['sDF']]
  }
}, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')

output$sample <- renderUI(fluidPage(sample, style="overflow-y:scroll; height: 300px"))



output$sstat <- renderTable({
  if(input$chkUpdate){
    fn_Report(inputVal_reactive())[['sstat']]
  } else {
    fn_Report(inputVal_eventReactive())[['sstat']]
  }
}, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')



output$zstat <- renderTable({
  if(input$chkUpdate){
    fn_Report(inputVal_reactive())[['zstat']]
  } else {
    fn_Report(inputVal_eventReactive())[['zstat']]
  }
}, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')



output$CI <- renderUI(
  if(input$chkUpdate){
    fn_Report(inputVal_reactive())[['txtCI']]
  } else {
    fn_Report(inputVal_eventReactive())[['txtCI']]
  }
)


#_________________________________________________________________________________________




#_________________________________________________________________________________________



})

