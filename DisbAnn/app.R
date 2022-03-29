#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

require(tidyverse)
require(readxl)

theme_set(theme_minimal())

dir <- getwd()   #Updates the path to the directory where the R Code is present

input_dir <- paste0(dir, "/../Inputs/") 

output_dir <- paste0(dir , "/../Outputs/")

ops_data_in <- read_excel("../Tests/idb_data.xlsx", sheet = "ops_data")

model_input <- read_csv(file = paste0(input_dir, "isdb_test_prjs.csv")) %>% 
    select( -...1) 

model_output <- readRDS( file = paste0(output_dir , "model_output.rda")) %>% 
    janitor::clean_names() %>% 
    as_tibble() %>% 
    mutate( ptype = str_extract( project_title , "[A-Za-z ]+") )

profiles <- readRDS(file = paste0(output_dir , "full_disb_profile.rda")) %>% 
    janitor::clean_names() %>% 
    as_tibble()

proj_type <- model_output %>% 
    distinct(ptype)

prjs_list <- model_output %>% 
   select(project_id)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Project Disb Profile Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("prjtype",
                        "Project Type",
                        choices = proj_type),
            selectInput("prjs",
                        "Project",
                        choices = prjs_list)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           #plotOutput("distPlot"),
           DT::dataTableOutput('proj_table'),
           plotOutput("profPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output , session) {
    
    
    
    observe({
        prjs_list2 <- model_output %>% 
            filter( ptype == input$prjtype) %>% 
            select(project_id)
        
        updateSelectInput(session, 'prjs',
                          choices = prjs_list2
        )
        
    })
    
    
    
    prj_info <- reactive({
        t1 <- model_output %>% filter(project_id == input$prjs) %>% 
                select( date_of_approval , 
                        date_of_signature , 
                        date_of_effectiveness ,
                        date_of_first_disbursement,
                        date_of_final_disbursement) %>%  
                pivot_longer(  1:5, names_to = "Date" , values_to = "DateOf")
        
        t2 <- model_output %>% filter(project_id == input$prjs) %>% 
                transmute( apr = NA ,days_from_approval_to_signature,
                           days_from_signature_to_effectiveness,
                           days_from_effectiveness_to_first_disbursement,
                           days_from_first_to_final_disbursement) %>% 
                pivot_longer(  1:5, names_to = "Days" , values_to = "DaysFrom") %>% 
                select(-Days)
        
        t1 %>% bind_cols(t2)
        
    })
    
    output$proj_table <- DT::renderDataTable(
           DT::datatable( prj_info(),
                          options = list(info=FALSE, paging = FALSE, searching = FALSE ))
        )
    

    output$profPlot <- renderPlot({
        profiles %>% filter( project_id == input$prjs ) %>%  
            ggplot( aes( standard_tenor , percentage_disbursed)) + 
            geom_line()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)








