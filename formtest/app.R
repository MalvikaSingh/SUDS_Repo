fieldsMandatory <- c("name", "sdg_indicator","area")
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}
fieldsAll <- c("name", "sdg_indicator", "past_achievements", "ongoing_projects", "date","area","org_type","social_media","funding_type")
responsesDir <- file.path("C:/Users/Public/formtest/tables")
epochTime <- function() {
  as.integer(Sys.time())
}
appCSS <-
  ".mandatory_star { color: red; }
   #error { color: red; }"

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
loadData <- function() {
  files <- list.files(file.path("C:/Users/Public/formtest/tables"), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  data <- dplyr::bind_rows(data)
  data
}
#####################
library(tidyverse)
library(shiny)
library(rgdal)
census = read.csv("Allegheny.csv")
sf = readOGR("cb_2017_42_tract_500k.shp")

allegheny = rep(NA,402)
j = 0
for(i in 1:3217){
  if(sf$GEOID[i] %in% census$GEO.id2){
    j = j+1
    allegheny[j] = i
  }
}
allegheny


#########################
shinyApp(
  ui <- fluidPage(
    plotOutput("plot1", click = "plot_click"),
    verbatimTextOutput("info"),
    
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    titlePanel("Form for user input"),
    DT::dataTableOutput("responsesTable"),
    
    div(
      id = "form",
      
      textInput("name", labelMandatory("Organisation Name"), ""),
      textInput("sdg_indicator", labelMandatory("SDG Indicator Required")),
      textInput("past_achievements", "Past Achievements"),
      textInput("ongoing_projects", "Ongoing Projects"),
      dateInput("date", label = h3("Expected End Date of Ongoing Project"), value = "2018-01-01"),
  
      selectInput("area",labelMandatory("Area/Neighborhood"),
                  c("",  "Shadyside", "Squirrel Hill", "Oakland")),
      selectInput("org_type", "Type of Organisation",
                  c("",  "Profit", "Non-Profit")),
      textInput("social_media", "Social Media Links"),
      selectInput("funding_type", "Source of Funding",
                  c("",  "Government", "Internal/Self Generated","Angel Investor")),
      actionButton("submit", "Submit", class = "btn-primary"),
      
      shinyjs::hidden(
        span(id = "submit_msg", "Submitting..."),
        div(id = "error",
            div(br(), tags$b("Error: "), span(id = "error_msg"))
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = "thankyou_msg",
        h3("Thanks, your response was submitted successfully!"),
        actionLink("submit_another", "Submit another response"),
        actionButton("view_data", "View Data", class = "btn-primary"),
        downloadButton("downloadBtn", "Download responses")
      )
    )
  ),
  server = function(input, output, session) {
    output$plot1 <- renderPlot({
      tracts = ggplot()
      for(i in allegheny){
        ct = data.frame(sf@polygons[[i]]@Polygons[[1]]@coords)
        C = NA
        index = which(as.vector(census$GEO.id2)==as.vector(sf$GEOID[i]))[[1]]
        amount = as.numeric(as.vector(census$HC01_EST_VC13))[index]
        if(!is.na(amount)){
          if(amount<37000){
            C = "light green"
          }
          else if(amount<67000){
            C = "green"
          }
          else{
            C = "dark green"
          }
        }
        tract = geom_polygon(data = ct,aes(x = X1, y = X2), colour = "black",fill=C)
        tracts = tracts + tract
      }
      tracts
    },height=400,width=400)
    
    output$info <- renderText({
      X = input$plot_click$x
      Y = input$plot_click$y
      clicked = NA
      for(i in allegheny){
        ct = data.frame(sf@polygons[[i]]@Polygons[[1]]@coords)
        c1 = min(ct$X1)<=X
        c2 = X<=max(ct$X1)
        c3 = min(ct$X2)<=Y
        c4 = Y<=max(ct$X2)
        if(c1+c2+c3+c4==4){
          clicked = i
        }
      }
      place = which(as.vector(census$GEO.id2)==as.vector(sf$GEOID[clicked]))[[1]]
      income = which(as.vector(census$GEO.id2)==as.vector(sf$GEOID[clicked]))[[1]]
      paste0(census$GEO.display.label[place],"\n Median Income: $",census$HC01_EST_VC13[income])
    })
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })
    
    saveData <- function(data) {
      fileName <- sprintf("%s_%s.csv",
                          humanTime(),
                          digest::digest(data))
      
      write.csv(x = data, file = file.path("C:/Users/Public/formtest/tables", fileName),
                row.names = FALSE, quote = TRUE)
    }
    
    # action to take when submit button is pressed
    observeEvent(input$submit, {
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    })
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
    observeEvent(input$view_data, {
      shinyjs::hide("thankyou_msg")
        
      output$responsesTable <- DT::renderDataTable(
        loadData(),
        rownames = FALSE,
        options = list(searching = FALSE, lengthChange = FALSE)
      )
      output$downloadBtn <- downloadHandler(
        filename = function() { 
          sprintf("mimic-google-form_%s.csv", humanTime())
        },
        content = function(file) {
          write.csv(loadData(), file, row.names = FALSE)
        }
      )
    })
  }
)