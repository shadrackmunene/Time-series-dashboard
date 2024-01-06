library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(stringr)
library(DT)
library(scales)
library(lubridate)


df <- tibble::tribble(
    ~invoice_date, ~product, ~source, ~category, ~quantity, ~net,
    "2018-12-17",  "apple", "shop", "fruit", 1, 5,
    "2018-12-22", "banana", "shop", "fruit",1, 5,
    "2018-12-21", "banana","market", "fruit", 1, 5,
    "2018-12-21", "carrot","shop", "vegetable", 1, 5,
    "2018-11-29", "banana","shop", "fruit", 1, 5,
    "2018-12-18", "carrot","market", "vegetable", 1, 5,
    "2018-12-05",  "apple","shop", "fruit", 1, 5,
    "2018-12-20", "banana","shop", "fruit",1, 5,
    "2018-12-19", "carrot", "market", "vegetable", 1, 5
)

df$product <- as.factor(df$product)
df$source <- as.factor(df$source)

df$category <- as.factor(df$category)




header <- dashboardHeader(title = "test")

sidebar <- dashboardSidebar(
    sidebarMenuOutput("menu"),
    dateRangeInput('dateRange',
                   label = 'Date range input: dd/mm/yyyy',
                   start = Sys.Date() - 28, end = Sys.Date() + 2,
                   format = "dd/mm/yyyy"),
    selectInput(inputId = "source", 
                label = "Selected Source:", 
                choices = ""),  
    selectInput(inputId = "category", 
                label = "Selected Category:", choices = ""),
    selectInput(inputId = "product", 
                label = "Selected Product:", choices = "")
)

body <- dashboardBody(
    fluidPage(
        titlePanel("test Sales Dashboard"),
        
        
        
        hr(),
        
        fluidRow(
            column(4, wellPanel(
                h4("Total Sales Value"),
                dateRangeInput('dateRange',
                               label = 'Date range input: yyyy-mm-dd',
                               start = Sys.Date() - 2, end = Sys.Date() + 2
                )),
                wellPanel(h4("Pie Chart showing sales by Platform"),
                          br()),
                wellPanel(h4("Pie Chart showing sales by Product"),
                          br())
                
            ),
            
            
            column(8, wellPanel(
                h4("LineChart showing sales by platform for the time period"),
                br()),
                fluidRow(
                    column(6, wellPanel(
                        h4("top selling product SKUs by value shown in a list"),
                        DT::dataTableOutput(outputId = "selected_df_table"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                        br())),
                    column(6, wellPanel(
                        h4("Line chart showing sales history by top 5 categories"),
                        br()))
                )
            )
        )
    )
)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    
    dashboardPage(header, sidebar, body)
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$dateRangeText  <- renderText({
        paste("input$dateRange is", 
              paste(as.character(input$dateRange), collapse = " to ")
        )
    })
    
    
    
    observe({
        
        
        updateSelectInput(
            session,
            inputId = "source",
            choices = df$source,
            selected = tail(df$source,1)
        )
    })
    
    observe({
        updateSelectInput(
            session,
            inputId = "category",
            choices = df %>% 
                filter(source == input$source) %>% 
                select(category) %>% 
                .[[1]]
        )
    })
    
    observe({
        updateSelectInput(
            session,
            inputId = "product",
            choices = df %>% 
                filter(category == input$category) %>% 
                select(product) %>% 
                .[[1]]
            
        )
    })
    
    observe({
        updateDateRangeInput(
            session,
            inputId = "dateRange"
        )
    })
    
    output$selected_df_table <- renderDataTable({
        if (input$product == "") {
            return()
        }
        
        df %>%
            select(invoice_date, source, category, product, quantity, net) %>% 
            filter(invoice_date >= input$dateRange[1]  & invoice_date <= input$dateRange[2]) %>%
            filter(source == input$source) %>% 
            filter(category == input$category) %>% 
            filter(product == input$product) %>% 
            datatable()
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)