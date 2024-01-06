

# MAVEN PIZZA ANALYTICS

# lIBRARIES

# Shiny
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)

# Analysis
library(tidyverse)
library(timetk)
library(lubridate)
library(plotly)
library(DT)

# UI----
ui <- navbarPage(
    title = "Pizza Analysis",
    inverse = FALSE,
    collapsible = TRUE,
    theme=shinytheme("darkly"),
    tags$head(
        tags$link(rel="stylesheet", type="text/css",href="styles.css")
    ),
    
    
    tabPanel(
        title = "Dashboard",
        
        # CSS
        #shinythemes::themeSelector(),
        
        # 1.0 Header----
        div(
            class = "jumbotron",
            id ="header",
            style ="background:url('pizza.jpg'); background-size:cover;",
            h1(class="page-header"," Maven Pizza Dashboard")
        ),
        
        div( class="",
             id="app_ui",
             width = 12,
             # 2.0 User Inputs----
             column(
                 class= "container",
                 id="range_buttons",
                 width = 6,
                 dateRangeInput(inputId = "date_range_1",
                                label   = "Enter a Data Range",
                                start   = "2015-01-01",
                                end     = "2015-12-31"
                 )
             ),
             
             column(
                 class= "container pull-right",
                 width = 6,
                 actionButton(inputId = "day",label="Daily"),
                 actionButton(inputId = "week",label="Weekly"),
                 actionButton(inputId = "month",label="Monthy"),
                 actionButton(inputId = "reset_index",label="Reset", icon=icon("sync"))
             ),
             
             div(
                 id= "range_settings",
                 class = "hidden",
                 selectInput(inputId = "range_analysis",
                             label = "Type of analysis",
                             choices = c("Daily"="day","Weekly"="week","Monthly"="month"), selected = "day")
             ),
             
             # 3.2 Revenue Cards----
             # column(
             #     width = 8,
             #     div(
             #         column(
             #             width = 2,
             #             div( 
             #                 class= "thumbnail",
             #                 img(src="chicken.jpg"),
             #                 p("Revenue &80")
             #             )
             #         ),
             #         column(
             #             width = 2,
             #             div( 
             #                 class= "thumbnail",
             #                 img(src="classic.jpg"),
             #                 p("Revenue &80")
             #             )
             #         ),
             #         column(
             #             width = 2,
             #             div( 
             #                 class= "thumbnail",
             #                 img(src="supreme.jpg", ),
             #                 p("Revenue &80")
             #             )
             #         ),
             #         column(
             #             width = 2,
             #             div( 
             #                 class= "thumbnail",
             #                 img(src="veggie.jpg"),
             #                 p("Revenue &80")
             #             )
             #         )
             #     )
             #  ),
             # 
             # 3.3 Custom Plot----
             column(
                 width=12,
                 plotlyOutput("plot1")
             )
        ),
        
        hr(),
        
        # 4.0 Comments
        div(
            class="",
            id   ="comments",
            column(
                width = 12,
                div(class="panel",
                    p("This analysis is powered by data from", a(href="https://mavenanalytics.io/blog/maven-pizza-challenge", 
                                                            targer="_blank","Maven analytics"))
                   
                    
                )
                
            )
        )
        
       
    )
    
    
    
)

#SERVER----
server <- function(input, output, session){
    
    # 1.0 Settings----
    
    observe({
        updateDateInput(session=session,
                        inputId = "date_range_1")
    })
    
    
    observeEvent(input$week, {
        updateSelectInput(session, "range_analysis", choices = c("Daily" = "day", "Weekly" = "week", "Monthly" = "month"), selected = "week")
    })
    # 
    observeEvent(input$month, {
        updateSelectInput(session, "range_analysis", choices = c("Daily" = "day", "Weekly" = "week", "Monthly" = "month"), selected = "month")
    })
    
    # Reset 
    observeEvent(input$reset_index,{
        updateSelectInput(session, "range_analysis", choices = c("Daily" = "day", "Weekly" = "week", "Monthly" = "month"), selected = "day")
        updateDateInput(session=session, inputId = "date_range_1", min =  "2015-01-01", max= "2015-12-31")
    })
    
     # Get data----
    pizza_order_tbl <- reactive({
        readRDS("00_data/pizza_category_tbl.rds")
    })
    
    start <- reactive({
         pizza_order_tbl()$date %>% min() %>% format("%Y-%m-%d")
    })
    end <- reactive({
         pizza_order_tbl()$date %>% max() %>% format("%Y-%m-%d")
    })
 
    
    # Plot fun
   output$plot1 <-  renderPlotly({
       g <- pizza_order_tbl() %>% group_by(category) %>%
           summarise_by_time(date,.by=input$range_analysis, value=sum(revenue)) %>%
           filter(date >= input$date_range_1[1] & date <= input$date_range_1[2]) %>%
           plot_time_series(date,value, .smooth = F,.color_var = category,.facet_ncol = 2,
                            .title = "Pizza Revenue by Type",.interactive = F)+
           theme_dark()+
           theme(
               plot.background = element_rect(fill = "#303030"),
               panel.background = element_rect(fill = "#303030"),
               legend.background = element_rect(fill = "#303030"),
               legend.title = element_text(color = "white"),
               legend.text = element_text(color = "white"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               plot.title = element_text(color = "white"),
               axis.title.x = element_text(color = "white"),
               axis.title.y = element_text(color = "white"),
               axis.text.x = element_text(color = "white"),
               axis.text.y = element_text(color = "white")
           )
       ggplotly(g)
   })
   
   output$table <- renderDT({
       datatable(pizza_order_tbl(), options=list(pageLength=10))
   })
   
 
   
}




shinyApp(ui, server)

