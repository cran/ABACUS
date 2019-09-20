
# Shiny server for ANOVA


#_________________________________________________________________________________________

shinyServer(function(input, output) {

#_________________________________________________________________________________________
# eventReactive function

# Reactive


inputVal_eventReactive <- eventReactive(input$cmdUpdate, {

      iseed <- input$numRN + as.integer(input$cmdUpdate)

      pmean1 <- input$pmean1
      pmean2 <- input$pmean2
      pmean3 <- input$pmean3

      psd <- input$psd

      n1 <- input$n1
      n2 <- input$n2
      n3 <- input$n3

      p <- input$p  # user probability

      p_tail <- input$p_tail


      fn_InputData_repeatable <- repeatable(rngfunc = fn_InputData, seed = iseed)

      fn_InputData_repeatable(pmean1 = pmean1,
                              pmean2 = pmean2,
                              pmean3 = pmean3,
                              psd = psd,
                              n1 = n1, n2 = n2, n3 = n3,
                              p = p, p_tail = p_tail)


})



#_________________________________________________________________________________________
# Reactive function


inputVal_reactive <- reactive({

    pmean1 <- input$pmean1
    pmean2 <- input$pmean2
    pmean3 <- input$pmean3

    psd <- input$psd

    n1 <- input$n1
    n2 <- input$n2
    n3 <- input$n3

    p <- input$p  # user probability

    p_tail <- input$p_tail

    fn_InputData(pmean1 = pmean1,
                 pmean2 = pmean2,
                 pmean3 = pmean3,
                 psd = psd,
                 n1 = n1, n2 = n2, n3 = n3,
                 p = p, p_tail = p_tail)



})





#_________________________________________________________________________________________
# Output


output$dnorm_plot <- renderPlot({

  if(input$chkUpdate){

    fn_dnorm(inputVal_reactive())

  } else {

    fn_dnorm(inputVal_eventReactive())

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



output$ss_plot <-renderPlot({

  if(input$chkUpdate){

    fn_SS(inputVal_reactive())

  } else {

    fn_SS(inputVal_eventReactive())

  }


})



output$ss_stack <- renderPlot({

  if(input$chkUpdate){

    fn_SS_stack(inputVal_reactive())

  } else {

    fn_SS_stack(inputVal_eventReactive())

  }


})



output$ms_plot <-renderPlot({

  if(input$chkUpdate){

    fn_MS(inputVal_reactive())

  } else {

    fn_MS(inputVal_eventReactive())

  }


})



output$mdiff_plot1 <- renderPlot({

  if(input$chkUpdate){

    fn_mean_diff(inputVal_reactive())

  } else {

    fn_mean_diff(inputVal_eventReactive())

  }


})



output$df_plot1 <- renderPlot({

  if(input$chkUpdate){

    fn_df_plot1(inputVal_reactive())

  } else {

    fn_df_plot1(inputVal_eventReactive())

  }


})


output$H <- renderUI({
  if(input$chkUpdate){
    fn_Report(inputVal_reactive())[['H']]
  } else {
    fn_Report(inputVal_eventReactive())[['H']]
  }
})



dt_sample <- renderTable({
  if(input$chkUpdate){
    fn_Report(inputVal_reactive())[['sDF']]
  } else {
    fn_Report(inputVal_eventReactive())[['sDF']]
  }
}, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')

output$dt_sample <- renderUI(fluidPage(dt_sample, style="overflow-y:scroll; height: 300px"))



output$dt_sstat <- renderTable({
  if(input$chkUpdate){
    fn_Report(inputVal_reactive())[['sstat']]
  } else {
    fn_Report(inputVal_eventReactive())[['sstat']]
  }
}, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')



output$txt_rst <- renderPrint({
  if(input$chkUpdate){
    fn_Report(inputVal_reactive())[['rst']]
  } else {
    fn_Report(inputVal_eventReactive())[['rst']]
  }
})




#_________________________________________________________________________________________

})

#_________________________________________________________________________________________








