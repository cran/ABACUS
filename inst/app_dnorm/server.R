
# Shiny server for Normal distribution

#_________________________________________________________________________________________


shinyServer(function(input, output) {


#_________________________________________________________________________________________
# Reactive


inputVal_eventReactive <- eventReactive(input$cmdUpdate, {

      iseed <- input$numRN + as.integer(input$cmdUpdate)

      pmean <- input$pmean
      psd <- input$psd
      n <- input$n

      type <- input$type
      bins <- input$bins

      p <- input$p  # user probability
      p_tail <- input$p_tail

      cs_xscale <- input$cs_xscale


      fn_InputData_repeatable <- repeatable(rngfunc = fn_InputData, seed = iseed)

      fn_InputData_repeatable(pmean = pmean, psd = psd,
                              n = n, bins = bins,
                              type = type,
                              p = p, p_tail = p_tail,
                              cs_xscale = cs_xscale)

})



inputVal_reactive <- reactive({

      pmean <- input$pmean
      psd <- input$psd
      n <- input$n

      type <- input$type
      bins <- input$bins

      p <- input$p  # user probability
      p_tail <- input$p_tail

      cs_xscale <- input$cs_xscale

      fn_InputData(pmean = pmean, psd = psd,
                   n = n, bins = bins,
                   type = type,
                   p = p, p_tail = p_tail,
                   cs_xscale = cs_xscale)


})


#_________________________________________________________________________________________


output$rnorm_plot <- renderPlot({

    if(input$chkUpdate){

      fn_rnorm(inputVal_reactive())

    } else {

      fn_rnorm(inputVal_eventReactive())

    }


})



output$dnorm1_plot <- renderPlot({

    if(input$chkUpdate){

      fn_dnorm1(inputVal_reactive())

    } else {

      fn_dnorm1(inputVal_eventReactive())

    }


})




output$dnorm2_plot <- renderPlot({

    if(input$chkUpdate){

      fn_dnorm2(inputVal_reactive())

    } else {

      fn_dnorm2(inputVal_eventReactive())

    }


})



output$pnorm_plot <- renderPlot({

    if(input$chkUpdate){

      fn_pnorm(inputVal_reactive())

    } else {

      fn_pnorm(inputVal_eventReactive())

    }


})




output$dnorm3_plot <- renderPlot({

    if(input$chkUpdate){

      fn_dnorm3(inputVal_reactive())

    } else {

      fn_dnorm3(inputVal_eventReactive())

    }


})



#_________________________________________________________________________________________


})


#_________________________________________________________________________________________

