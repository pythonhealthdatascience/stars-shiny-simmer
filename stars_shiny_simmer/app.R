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
library(png)

# the treat-simmer
source("./model.R")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Interative simulation", icon = icon("dashboard"), 
             tabName = "intsim"),
    menuItem("About", icon = icon("th"), tabName = "about")
  ),
  
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

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "intsim",
      h2("Treatment Centre Simulation Model"),
      
      h6("A simple simulation model of a urgent care and treatment centre."),
      
      box(title = "Treatment Process",
          collapsible = TRUE, 
          solidHeader = TRUE,
          #uiOutput("imageUI"),
          # checkout the width options... auto...
          # https://www.rdocumentation.org/packages/shiny/versions/0.10.2.1/topics/imageOutput
          img(src="process_flow_img.png", height = 200, align="center")
      ),
      
      box(title = "Daily Arrival Pattern",
          collapsible = TRUE, 
          solidHeader = TRUE,
          textInput("given", "Given Name"),
          textInput("surname", "Surname"),
          selectInput("pet", "What is your favourite pet?", c("cats", "dogs", "ferrets"))
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
            h2("Widgets tab content")
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "Treat-Simmer"),
  sidebar,
  body
)




server <- function(input, output){
  
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
  
  
  imgFileName <- reactive({
    paste0("./www/process_flow_img.png")
  })
  
  imgFile <- reactive({
    readPNG(imgFileName(), info=TRUE)
  })
  
  
  imgSize <- reactive({
    info <- unlist(stringr::str_split(attr(imgFile(), "info")$dim, stringr::fixed(" ")))
    info <- paste0(info, "px")
    names(info) <- c("width", "height")
    info <- as.list(info)
    info
  })
  
  output$info <- renderText({
    paste0("Height: ", imgSize()$height, "; Width: ", imgSize()$width)
  })

  output$image <- renderImage({
    list(
      src=imgFileName(),
      contentType="image/png",
      width=imgSize()$width,
      height=imgSize()$height,
      alt=paste0("A process flow image"),
      class="myImageClass"
    )
  })
  
  
  # Send a pre-rendered image, and don't delete the image after sending it
  output$processImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('./www/process_flow_img.png'))

    # Return a list containing the filename and alt text
    list(src = filename,
         alt = "Process Flow Logo",
         height = 200,
         )

  }, deleteFile = FALSE)
  
  
  
  # update the UI here by changing the height of the image div
  output$imageUI <- renderUI({
    imageOutput("image", height = imgSize()$height)
  })
  
} # Server end


# Run the application 
shinyApp(ui = ui, server = server)
