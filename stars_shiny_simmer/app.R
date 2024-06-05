#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(waiter)

# the treat-simmer
source("./model.R")

ui <- fluidPage (    # creates empty page

  # auto-waiter  provides a spinner for while the model is running
  autoWaiter(),
  #  title of app
  titlePanel("treat-simmer in Shiny"),
  
  # layout is a sidebar—layout
  sidebarLayout(
    
    sidebarPanel( # open sidebar panel
      
      # n triage bays
      sliderInput(inputId = "n_triage",	
                   label = "Triage bays",
                   value = DEFAULT_N_TRIAGE,
                   min = 1,
                   max = 10),
      
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
                  max = 10),

      # action button runs model when pressed
      actionButton(inputId = "run_model",
                   label   = "Run model")
      
    ),  # close sidebarPanel
    
    # open main panel
    mainPanel(
      
      # heading (results table)
      h3("Simulation Results"),
      
      # tableOutput id = icer_table, from server
      tableOutput(outputId = "sim_summary_table"),
      
    ) # close mainpanel
    
  )# close side barlayout
  
) # close UI fluidpage


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
  
} # Server end


# Run the application 
shinyApp(ui = ui, server = server)
