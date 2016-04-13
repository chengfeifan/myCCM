library(shiny)
# library(plotly)

shinyServer(function(input, output) {

  output$summary<-renderPrint({
    input$Table
  })
})
