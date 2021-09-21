library(tidyverse)
library(shiny)
library(rhandsontable)

ui <- fluidPage(title = "Openarray 384 samplesetup",
  titlePanel("Openarray 384 samplesetup tracker"),
  tags$div(
    tags$p("For any question, please  contact ",
           tags$a(href = "mailto:roman.tremmel@ikp-stuttgart.de?Subject=PGxLIMS",target="_top", "Roman"))),
  hr(),
  h4("Select input plate format"),              
  shiny::selectInput("plate_format", NULL, choices = c(96, 384), multiple = F, selected = 384),
  h4("Paste sample information"),
  rHandsontableOutput("plate"),
  hr(),
  h4("Copy or download sample setup for Openarray"),
  DT::DTOutput("DT_cp")
)

server <- function(input, output){

 data.in <- reactiveValues(plate_96 = data.frame(matrix("", nrow = 8, ncol = 12)) %>% set_names(1:12) %>% mutate(R=LETTERS[1:n()]) %>%
                             column_to_rownames("R"),
                           daten_348 = data.frame(matrix("", nrow = 16, ncol = 24)) %>% set_names(1:24)%>% mutate(R=LETTERS[1:n()]) %>%
                             column_to_rownames("R"))


  output$plate <- renderRHandsontable({
    req(input$plate_format)
    if(input$plate_format == "96"){
    tmp <-   data.in$plate_96
    }else{
    tmp <-   data.in$daten_348
    }
    
    rhandsontable(tmp)
  })
  

  observeEvent(eventExpr = input$plate, {
    data.in$values <- hot_to_r(input$plate)
    print(data.in$values)

    output$DT_cp <-  DT::renderDT({
      if(!is.null(tryCatch(DT::datatable(data.in$values), error = function(e){})))
      { data.in$values %>% 
          rownames_to_column(var = "Row") %>% 
          pivot_longer(-Row, names_to = "Column", values_to = "Sample Name") %>% 
          mutate(Column = str_remove(Column, "x")) %>% 
          unite(Well, Row, Column, sep="") %>% DT::datatable(rownames = F,extensions = 'Buttons',
                                                             options = list(pageLength=-1, dom = 'Brti',
                                                                            buttons = list(list(extend = "copy", title = NULL), 
                                                                                           list(extend = "csv", title = NULL,fieldBoundary= '' ))))}
    })
  })
}
shinyApp(ui, server)
