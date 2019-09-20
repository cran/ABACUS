# # Shiny server: Sampling

#_________________________________________________________________________________________

shinyServer(function(input, output) {

#_________________________________________________________________________________________

# eventReactive----


inputVal_eventReactive <- eventReactive(input$cmdUpdate, {

  iseed <- input$numRN + (as.integer(input$cmdUpdate)-1)

  pmean <- input$pmean
  psd <- input$psd

  n <- input$n
  k <- input$k

  fn_InputData_repeatable <- repeatable(rngfunc = fn_InputData, seed = iseed)

  fn_InputData_repeatable(pmean = pmean, psd = psd,
                          n = n, k = k)

})



#_________________________________________________________________________________________

# reactive----

inputVal_reactive <- reactive({

  pmean <- input$pmean
  psd <- input$psd

  n <- input$n
  k <- input$k

  fn_InputData(pmean = pmean, psd = psd,
               n = n, k = k)


})


#_________________________________________________________________________________________
# Output ----

output$dnorm_plot <- renderPlot({

  if(input$chkUpdate){

    fn_dnorm(inputVal_reactive())

  } else {

    fn_dnorm(inputVal_eventReactive())

  }
})


output$dotplot1 <- renderPlot({

  if(input$chkUpdate){

    fn_dotplot1(inputVal_reactive())

  } else {

    fn_dotplot1(inputVal_eventReactive())

  }
})



output$dotplot2 <- renderPlot({

  if(input$chkUpdate){

    fn_dotplot2(inputVal_reactive())

  } else {

    fn_dotplot2(inputVal_eventReactive())

  }
})



output$txt1 <- renderText({

  k = input$k
  paste0("... continuing to Sample ", k,".")

})



output$distn_mean <-renderPlot({

  if(input$chkUpdate){

    fn_distn_mean(inputVal_reactive())

  } else {

    fn_distn_mean(inputVal_eventReactive())

  }
})



output$txt2 <- renderText({

  distname = "population following normal distribution"

  k = input$k
  n = input$n
  paste("Distribution of means of", k, "random samples,\n
        each consisting of", n, " observations\n
        from a", distname)

})



output$CI_1 <- renderPlot({
  if(input$chkUpdate){

    fn_CI_1(inputVal_reactive())

  } else {

    fn_CI_1(inputVal_eventReactive())

  }
})



output$CI_2 <- renderPlot({
  if(input$chkUpdate){

    fn_CI_2(inputVal_reactive())

  } else {

    fn_CI_2(inputVal_eventReactive())

  }
})



output$txtCI <- renderText({

  distname = "population following normal distribution"

  k = input$k
  n = input$n
  paste("95% Confidence Interval of means of", k, "random samples,\n
        each consisting of", n, " observations\n
        from a", distname)

})



samp <- renderTable({
  if(input$chkUpdate){
    fn_Report(inputVal_reactive())[['sstat']]
  } else {
    fn_Report(inputVal_eventReactive())[['sstat']]
  }
}, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')

output$samp <- renderUI(fluidPage(samp, style="overflow-y:scroll; height: 300px"))



output$smean <- renderUI(
  if(input$chkUpdate){
    fn_Report(inputVal_reactive())[['txtMean']]
  } else {
    fn_Report(inputVal_eventReactive())[['txtMean']]
  }
)


output$CI <- renderUI(
  if(input$chkUpdate){
    fn_Report(inputVal_reactive())[['txtCI']]
  } else {
    fn_Report(inputVal_eventReactive())[['txtCI']]
  }
)


#_________________________________________________________________________________________


})


#_________________________________________________________________________________________


