library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyBS)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(metathis)
library(readxl)
library(shinyWidgets)
library(rmarkdown)
library(markdown)

source('read_list.R')

ui <- dashboardPage(
  
  dashboardHeader(title = "Global Factor Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", icon = icon("dashboard"),
               menuSubItem("Overview", tabName = "overview"),
               menuSubItem("Raw Data", tabName = "raw_return")
               ),
      br(),
      pickerInput(
        inputId = "region",
        label = "Region/Country", 
        choices = c('All Regions', 'All Countries'),
        selected = 'All Regions',
        ),
      
      pickerInput(
        inputId = "factor",
        label = "Theme/Factor", 
        choices =  c('All Themes', 'All Factors'),
        selected =  'All Themes'),
      
      pickerInput(
        inputId = "freq",
        label = "Data Frequency", 
        choices = data[[3]][, 1] %>% pull(),
        selected = data[[3]][2, 1]
      ),
      
      pickerInput(
        inputId = "wt",
        label = "Weighting", 
        choices = data[[4]][, 1] %>% pull(),
        selected = data[[4]][1, 1]
      ),
      
      br(),
      
      tags$style(type="text/css",
                 ".align-center { display: flex; justify-content: center; }
                 #action { 
    width: 70%;
    background-color: #D9AE6A;
  }"
      ),
      tags$div(class="align-center",
               tags$style(type="text/css", "#action { width: 70%;}"),
               actionButton(
                 inputId = "action",
                 label = "Show Data"
               )
      ),
      hr(),
      menuItem("Source", tabName = "source", icon = icon("info-circle")),
      menuItem("App Maker's Youtube", href = "https://www.youtube.com/channel/UCHfiWvw33aSBktAlWICfPKQ?sub_confirmation=1", icon = icon("link"))
      
    )
  ),
  dashboardBody(
    
    meta() %>%
      meta_social(
        title = "Global Factor Data",
        description = "Global Factor Data by Jensen, Kelly and Pedersen",
        url = "https://doomoolmori.shinyapps.io/jkpfactors/"
      ),
    
    shiny::includeCSS("www/custom.css"),
    tabItems(
      tabItem("overview",
              fluidRow(
                column(3, uiOutput('location')),
                column(3, uiOutput('select_factor'))
              ),
              tabsetPanel(
                tabPanel("Graph",
                         br(),
                         uiOutput('welcome'),
                         plotOutput('graph', height = "600px") %>% withSpinner()
                         ),
                tabPanel("Summary",
                         br(),
                         dataTableOutput('stat_table_dt') %>% withSpinner()
                         ),
              )
              ),
      tabItem("raw_return",
              uiOutput("dynamic") %>% withSpinner()
      ),
      tabItem("source",
              includeMarkdown('source.Rmd')
              )
      )
    
  )
)

server <- function(input, output, session) {
  
  down_tbl <- eventReactive(input$action, {

    q_region <- data[[1]] %>% filter(region == input$region) %>% select(query) %>% pull()
    q_factor <- data[[2]] %>% filter(factor == input$factor) %>% select(query) %>% pull()
    q_freq <- data[[3]] %>% filter(frequency == input$freq) %>% select(query) %>% pull()
    q_wt <- data[[4]] %>% filter(weight == input$wt) %>% select(query) %>% pull()
    
    url <- glue::glue('https://jkpfactors.com/data/[{q_region}]_[{q_factor}]_[{q_freq}]_[{q_wt}].zip')
    
    temp <- tempfile(fileext = ".zip")
    download.file(url, temp)
    unzip(temp, exdir = tempdir())
    files <- list.files(tempdir(), pattern = ".csv", full.names = TRUE)
    tbl <- read.csv(files, fileEncoding = "CP949", encoding = "UTF-8")
    unlink(temp)
    file.remove(files[1])
    
    tbl_select <- tbl %>% select(location, name, date, ret, freq, weighting) %>%
      mutate(date = as.Date(date)) %>%
      left_join(data[[1]], by = c('location' = 'query')) %>%
      left_join(data[[2]], by = c('name' = 'query')) %>%
      select(region, factor, date, ret, freq, weighting) %>%
      magrittr::set_colnames(c('Location', 'Factor', 'Date', 'Return', 'Frequency', 'Weighting'))
    
    return (tbl_select)
  })
  
  output$table <- renderDT({
    
    down_tbl() %>%
      DT::datatable(rownames = FALSE,
                    filter = "top",
                    options = list(dom = 'tp',
                                   pageLength = 15
                                   
                    )
      ) %>%
      DT::formatRound(columns=c('Return'), digits=4)
  }, server = TRUE)
  
  stat_table <- reactive({
    
    mult = ifelse(unique(down_tbl()$Frequency) == 'monthly', 12, 255)
    
    down_tbl() %>%
      filter(Location %in% input$location) %>%
      filter(Factor %in% input$select_factor) %>%
      group_by(Location, Factor) %>%
      summarise(
        data_count = n(),
        annual_return_arith = (mean(Return) * mult) * 100,  
        annual_return_geom = ((prod(1 + Return)) ^ (mult/n()) - 1) * 100,
        annual_volatility = (sd(Return) * sqrt(mult)) * 100,
        sharpe_ratio = (annual_return_arith) / (annual_volatility),
        mdd = max(1 - cumprod(1 + Return)/cummax(cumprod(1 + Return))) * 100
      ) %>%
      ungroup() %>%
      mutate(across(where(is.numeric), ~round(.x, 2))) %>%
      magrittr::set_colnames(c(
        'Location', 'Factor', 'N', 'Ann Arith Ret(%)', 'Ann Geo Ret(%)',
        'Volatility(%)', 'Sharpe Ratio', 'MDD(%)'))
    
  })
  
  output$stat_table_dt <- renderDT({
    
    stat_table() %>%
      DT::datatable(rownames = FALSE, extensions = c('RowGroup', 'Buttons'),
                    filter = "top",
                    options = list(dom = 'tB',
                                   pageLength = nrow(stat_table()),
                                   scrollY = '500px',
                                   buttons = c('copy', 'csv', 'excel'),
                                   columnDefs = list(
                                     list(visible = FALSE, targets = 0),
                                     list(orderable = FALSE, targets = 0)
                                   ),
                                   rowGroup = list(dataSrc = 0)
                    ),
                    callback = JS(
                      "table.on('draw.dt', function(){
                  table.column(0).nodes().each(function(cell, i){
                    if(cell.innerHTML.includes('<span')) cell.innerHTML = '';
                  });
                });"
                    )
      ) 
      # formatStyle(columns = names(stat_table()), fontSize = '15px')
    
  }, server = TRUE)
  
  output$location <- renderUI({
    
    req(input$region)
    
    pickerInput(
      inputId = "location",
      label = "Select a sub-country", 
      choices = unique(down_tbl()$Location),
      selected = if (length(unique(down_tbl()$Location)) >= 10) {
        c('United States', 'Japan', 'Korea', 'China')
      } else {
        unique(down_tbl()$Location)[1:5]
        },
      options = list(`actions-box` = TRUE, `live-search` = TRUE), 
      multiple = TRUE
    )
  })
  
  output$select_factor <- renderUI({
    
    req(input$factor)
    
    pickerInput(
      inputId = "select_factor",
      label = "Select a sub-factor", 
      choices = unique(down_tbl()$Factor),
      selected = if (length(unique(down_tbl()$Factor)) <= 20) {
        c('Low Risk', 'Momentum', 'Profitability', 'Quality', 'Size', 'Value')
      } else {
        unique(down_tbl()$Factor)[1:5]
      },
      options = list(`actions-box` = TRUE, `live-search` = TRUE), 
      multiple = TRUE
    )
  })
  
  output$downloadData = downloadHandler(
    
    filename = function() {
      paste("factor_data", ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(down_tbl()), file)
    }
  ) 
  
  output$welcome <- renderUI({
    
    if (input$action == 0) {
     HTML(
       '
       <br>
       <p style="font-size: 30px; padding-left: 30px;">
1. Select the desired item from the left menu.<br>
2. Click [Show Data].<br>
3. Selected data appears in the dashboard.       
</p>
       '
     )
    }
  })
  
  output$dynamic <- renderUI({
    
    req(input$action)
    
    if (input$action > 0) {
      tagList(
        tags$style(type="text/css", "#downloadData {background-color:black;color: white;"),
        downloadButton("downloadData", "Download", class = "butt1"),
        dataTableOutput("table")
      )
    }
  })
  
  output$graph <- renderPlot({
    
    tbl = down_tbl() %>%
      filter(Location %in% input$location) %>%
      filter(Factor %in% input$select_factor) %>%
      mutate(Return = log(1 + Return)) %>%
      group_by(Factor, Location) %>%
      arrange(Date) %>%
      mutate(Cum_ret = cumsum(Return))
    
    p = tbl %>%
      ggplot(aes(x = Date, y = Cum_ret, group = Factor, color = Factor)) +
      geom_line() +
      xlab('') + ylab('Log Cumulative Return') +
      theme_bw() +
      labs(color = NULL) +
      theme(text = element_text(size = 20))
    
    if(length(unique(tbl$Location)) > 1) {
      p <- p + facet_wrap(~ Location, scales = 'free')
    } 
    
    return(p)
      
  })
  
  }

shinyApp(ui, server)

# rsconnect::deployApp()