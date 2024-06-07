#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(waiter)
library(ggplot2)
library(tibble)

# the treat-simmer
source("./model.R")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarid",
    menuItem("Interative simulation", icon = icon("dashboard"), 
             tabName = "intsim"),
    menuItem("About", icon = icon("th"), tabName = "about")
  ),
  
  # conditionalPanel allows use to dynamically add/hide the inputs depending
  # on if the interactive page is active.
  # source: https://stackoverflow.com/questions/61299368/shiny-and-shinydashboard-how-to-display-inputs-in-sidebar-only-on-certain-tabs
  conditionalPanel(
    'input.sidebarid == "intsim"',
    
    # n triage bays
    sliderInput(inputId = "n_triage",
                label = "Triage bays",
                value = DEFAULT_N_TRIAGE,
                min = 1,
                max = 10,
                step = 1),
    
    # n registration clerks
    sliderInput(inputId = "n_reg",
                label = "Registration Clerks",
                value = DEFAULT_N_REG,
                min = 1,
                max = 10),
    
    # n exam rooms
    sliderInput(inputId = "n_exam",
                label = "Examination Rooms",
                value = DEFAULT_N_EXAM,
                min = 1,
                max = 10),
    
    # n non-trauma treatment cubicles
    sliderInput(inputId = "n_nt_cubicles",
                label = "Non-trauma Cubicles",
                value = DEFAULT_NON_TRAUMA_CUBICLES,
                min = 1,
                max = 10),
    
    # n trauma stabilisation rooms
    sliderInput(inputId = "n_trauma",
                label = "Stabilisation rooms",
                value = DEFAULT_N_TRAUMA,
                min = 1,
                max = 10),
    
    # n trauma treatment cubicles
    sliderInput(inputId = "n_trauma_cubicles",
                label = "Trauma Cubicles",
                value = DEFAULT_TRAUMA_CUBICLES,
                min = 1,
                max = 10)
  )
)

body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "intsim",
      h2("Treatment Centre Simulation Model"),
      
      h6("A simple simulation model of a urgent care and treatment centre."),
      
      # allow image to be dynamically resized with CSS
      # source: https://stackoverflow.com/questions/30478541/how-can-i-make-the-width-of-an-image-in-a-shiny-app-dynamic
      tags$head(tags$style(
        type="text/css",
        "#processImage img {max-width: 100%; width: 100%; height: auto}"
      )),
      
      box(title = "Treatment Process",
          collapsible = TRUE, 
          collapsed = TRUE,
          solidHeader = TRUE,
          imageOutput("processImage"),
      ),
      
      box(title = "Daily Arrival Pattern",
          collapsible = TRUE, 
          collapsed = TRUE,
          solidHeader = TRUE,
          plotOutput("arrivals_plot")
      ),
      
      box(title = "Simulation control",
          collapsible = FALSE, 
          solidHeader = FALSE,
          width=12,
          actionButton("run_model", "Run simulation"),
      ),
      
      
      box(title = "Tabular results",
          collapsible = TRUE, 
          solidHeader = TRUE,
          tableOutput("sim_summary_table")
      ),
      
    
    ),
    
    tabItem(tabName = "about",
            includeMarkdown("www/txt/about.md")
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Treat-Simmer"),
  sidebar,
  body
)




server <- function(input, output){
  
  # load arrival profile
  arrival_data <- reactive({
    csv_data <- getURL(NSPP_PATH)
    df <- read.csv(text=csv_data)
    
    # lock in order of time of day for bar chart display
    df$period <- factor(df$period, levels = df$period)
    df
  })
  
  
  # when action button pressed ...
  observeEvent(input$run_model,
               ignoreNULL = F, {
                 
   # Run  model function with Shiny inputs
   # 1st create the experiment using Shiny input
   exp = create_experiment(n_triage_bays = input$n_triage,
                           n_reg_clerks = input$n_reg,
                           n_exam_rooms = input$n_exam,
                           n_non_trauma_cubicles = input$n_nt_cubicles,
                           n_trauma_rooms = input$n_trauma,
                           n_trauma_cubicles= input$n_trauma_cubicles,
                           log_level=0)
   
   # run multiple replications of the model...
   df_model_reps = multiple_replications(
     exp = exp,
     n_reps=5,
     random_seed=42)
   
   #—— CREATE SUMMARY TABLE ——#
   
   # renderTable continuously updates table
   output$sim_summary_table <- renderTable({
     df_res_table <- create_summary_table(df_model_reps, exp)
     
     # print the results table
     df_res_table
     
   }) # table plot end.
   
   
      }) # Observe event end
  
  
  # Send a pre-rendered image, and don't delete the image after sending it
  output$processImage <- renderImage({
    filename <- normalizePath(file.path('./www/process_flow_img.png'))

    # Return a list containing the filename and alt text
    list(src = filename,
         alt = "Process Flow Logo"
         )

  }, deleteFile = FALSE)
  
  # time dependent arrival profile plot
  output$arrivals_plot <- renderPlot({
    ggplot(data=arrival_data(), aes(x=period, y=arrival_rate)) +
      geom_bar(stat="identity", fill="steelblue") + 
      theme(axis.text.x = element_text(angle = 90, 
                                       vjust = 0.5, 
                                       hjust=1)) +
      xlab("Hour of day") + 
      ylab("Mean arrivals (patients/hr)")
  }, res = 96)
  
  
  
  
} # Server end


# Run the application 
shinyApp(ui = ui, server = server)
