library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

#themes
theme <- bs_theme(
  bg = "#0b3d91", fg = "white", primary = "#FCC780",
  base_font = font_google("Space Mono"),
  code_font = font_google("Space Mono")
)

# Define UI for application makes separate pages for uploading and graphs
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  navbarPage("FIFA22 PLAYER DATA",
             tabPanel("UploadCSV",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                          ),
                          tags$hr(),
                          checkboxInput("header", "Header", TRUE),
                          radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Tab = "\t"),
                                       selected = ","),
                          radioButtons("quote", "Quotes",
                                       choices = c(None = "",
                                                   "Single Quote" = "'"),
                                       selected = '"'),
                          radioButtons("disp", "Display",
                                       choices = c(Head = "head",
                                                   All = "all"),
                                       selected = "head")
                        ),
                        mainPanel(
                          tableOutput("contents")
                        )
                      )
             ),
             tabPanel("Graph1", 
                      sidebarLayout(
                                 sidebarPanel(
                                   radioButtons("Color", "Color Change",
                                                c("Original"="p", "Inverse"="l")
           
                                   ),
                                   sliderInput(inputId = "HR",
                                               label = "Height (CM) Range:",
                                               min = 155,
                                               max = 220,
                                               value = c(155, 220),
                                               dragRange = TRUE),
                                   sliderInput(inputId = "WR",
                                               label = "Weight (Kg) Range:",
                                               min = 55,
                                               max = 200,
                                               value = c(55, 200),
                                               dragRange = TRUE),
                                   textInput(inputId = "main",
                                             label = "Title for Graph 1")
                                   #FONT CHANGE
                                 
                                 
                                 ),
                                 mainPanel(
                                   plotOutput("Graph1")
                                 )
                               )
                      
             ),
             tabPanel("Graph2", 
                      
                               sidebarLayout(
                                 sidebarPanel(
                                   radioButtons("Color2", "Color Change",
                                                c("Original"="p2", "Inverse"="l")

                                   ),
                                   sliderInput(inputId = "Age",
                                               label = "Age Range:",
                                               min = 15,
                                               max = 35,
                                               value = c(15, 35),
                                               dragRange = TRUE),
                                   sliderInput(inputId = "Potential",
                                               label = "Potential Range:",
                                               min = 70,
                                               max = 95,
                                               value = c(70,  95),
                                               dragRange = TRUE),
                                   textInput(inputId = "main2",
                                             label = "Title for Graph 2")
                                 ),
                                 mainPanel(
                                   plotOutput("Graph2")
                                 )
                               )
                      
             ),
             tabPanel("Graph3", 
                      
                               sidebarLayout(
                                 sidebarPanel(
                                   radioButtons("Color3", "Color Change",
                                                c("Original"="p3", "Inverse"="l")
                                   ),
                                   sliderInput(inputId = "Value",
                                               label = "Value (EUR) Range:",
                                               min = 1000000,
                                               max = 100000000,
                                               value = c(1000000,100000000),
                                               dragRange = TRUE),
                                   sliderInput(inputId = "Wage3",
                                               label = "Wage (EUR, Weekly) Range:",
                                               min = 1000,
                                               max = 320000,
                                               value = c(1000, 320000),
                                               dragRange = TRUE),
                                   textInput(inputId = "main3",
                                             label = "Title for Graph 3")
                                 ),
                                 mainPanel(
                                   plotOutput("Graph3")
                                 )
                               )
                      
             )
  )
) 


server <- function(input, output) {

  output$contents <- renderTable({
    req(input$file)
    tryCatch(
      {
        df <- read.csv(input$file$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })

  output$Graph1 <- renderPlot({
    req(input$file)
    
    tryCatch(
      {
        df <- read.csv(input$file$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      })
    
    xval1 = input$HR[1]
    xval2 = input$HR[2]
    yval1 = input$WR[1]
    yval2 = input$WR[2]
    dat <- df %>% filter(Height >= xval1 & Height <= xval2 & Weight <= yval1 & Weight <= yval2)
    
    if(input$Color == "p"){
      col1 = "grey90"
      col2 = "black"
    } 
    else {
      col1 = "black"
      col2 = "white"
    }
    base <- ggplot(dat, aes(x=Height, y=Weight)) +
      ggtitle((input$main)) +
      geom_point(color=col2) 
    base + theme(panel.background = element_rect(fill = col1))
  })
  
  output$Graph2 <- renderPlot({
    req(input$file)
    tryCatch(
      {
        df <- read.csv(input$file$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      })
    
    xval3 = input$Age[1]
    xval4 = input$Age[2]
    yval3 = input$Potential[1]
    yval4 = input$Potential[2]
    dat2 <- df %>% filter(Age >= xval3 & Age <= xval4 & Potential <= yval3 & Potential <= yval4 )
    
    if(input$Color2 == "p2"){
      col1 = "grey90"
      col2 = "black"
    } 
    else {
      col1 = "black"
      col2 = "white"
    }
    
    
  
    
      base <-ggplot(dat2, aes(fill = Potential, x=Age, y=Overall)) +
        ggtitle((input$main2)) +
        geom_bar(stat="identity") 
      base + theme(panel.background = element_rect(fill = col1))
  })
  
  output$Graph3 <- renderPlot({
    req(input$file)
    tryCatch(
      {
        df <- read.csv(input$file$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      })
    if(input$Color3 == "p3"){
      col1 = "grey90"
      col2 = "black"
    } 
    
    
    else {
      col1 = "black"
      col2 = "white"
    }
    
    xval5 = input$Value[1]
    xval6 = input$Value[2]
    yval5 = input$Wage3[1]
    yval6 = input$Wage3[2]
    dat3 <- df %>% filter(ValueEUR >= xval5 & ValueEUR <= xval6 & WageEUR <= yval5 & WageEUR <= yval6 )
    
    base <- ggplot(dat3, aes(x=ValueEUR , y=WageEUR)) +
      geom_point(color=col2) +
      ggtitle((input$main3))
    base + theme(panel.background = element_rect(fill = col1))
  })
  
}

shinyApp(ui = ui, server = server)
