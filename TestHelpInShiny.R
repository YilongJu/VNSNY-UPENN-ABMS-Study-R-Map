library(shiny)
library(tools)

ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    selectInput("package", "select package", .packages(all.available = TRUE)),
    uiOutput("choose_topic")
  ),
  mainPanel(uiOutput("documentation"))
))

server <- function(input, output, session){
  tmp <- tempfile()
  onSessionEnded(function(){ unlink(tmp) })
  
  RdDatabase <- reactive({
    Rd_db(input$package)
  })
  
  output$choose_topic <- renderUI({
    selectInput("topic", "select topic", sub(".Rd", "", names(RdDatabase())))
  })
  
  output$documentation <- renderUI({
    rdfile <- paste0(input$topic, ".Rd")
    req(rdfile %in% names(RdDatabase()))
    Rd2HTML(RdDatabase()[[rdfile]], tmp, no_links = TRUE, package = input$package)
    includeHTML(tmp)
  })
}

shinyApp(ui, server)