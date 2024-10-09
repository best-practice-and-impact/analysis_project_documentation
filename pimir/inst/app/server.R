library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {

  gfcf <- reactive({
    if(is.null(input$gfcf)){
      return(as.numeric(readLines("../data/gfcf.txt")))
    }
    return(as.numeric(readLines(input$gfcf$datapath)))
  })

  lifeLengths <- reactive({
    if(is.null(input$lifeLenghts)){
      return(read.csv("../data/life_lengths.csv", header=TRUE, stringsAsFactors = FALSE))
    }
    return(read.csv(input$lifeLenghts$datapath, header=TRUE, stringsAsFactors = FALSE))
  })

  pimResults <- reactive({
    if(input$rightTruncate == "TRUE"){
      rt <- TRUE
    }else{
      rt <- FALSE
    }
    config <- pimir::pimConfig(profileType = input$profileType,
                               profileFunctionName = input$profileFunction,
                               retirementDistName = input$retDist,
                               rightTruncate = rt,
                               combinationMethod = input$combinationMethod,
                               discountRate =  input$discount,
                               inflationRate =  input$inflation,
                               offSet = input$offSet, profileFunctionParam = input$profileParam)
    results <- pimir::run(gfcf(), lifeLengths(), config)

    cs <- data.frame(GFCF= gfcf(), GrossStock=results$GrossStock,
                     NetStock=results$NetStock,
                     ProductiveStock=results$ProductiveStock,
                     Age=seq_len(length(gfcf())),
                     stringsAsFactors = FALSE)
    cs
  })

  output$capstock <- renderPlot({
    cs <- tidyr::gather(pimResults(), key="Stock", value="Value", -Age)
    ggplot2::qplot(x=Age, y=Value, data=cs, colour=Stock, geom="line") +
      geom_line(size = 1.5) + facet_grid(~Stock) +
      theme(text = element_text(size = 16),
            legend.position = "bottom",
            legend.text = element_text(size = 20), legend.title = element_blank())
  })

  output$retirement <- renderPlot({

    profileCfg <- pimir::pimConfig(profileType = input$profileType,
                               profileFunctionName = input$profileFunction,
                               retirementDistName = input$retDist,
                               rightTruncate = as.logical(input$rightTruncate),
                               combinationMethod = input$combinationMethod,
                               discountRate =  input$discount,
                               inflationRate =  input$inflation,
                               offSet = input$offSet, profileFunctionParam = input$profileParam)

    sv <- pimir::calcSurvivalValues(lifeLengths(), profileCfg)
    qplot(x = 1:length(sv$values[[1]]), y = sv$values[[1]]) +
      labs(title = "Survival Values for Retirement Distribution",
           x = "Time", y = "Probability of survival") + geom_line()
  })

  output$tableData <- renderTable({
    pimResults()
  })

})
