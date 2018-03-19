# shinyapps.io wd:  "/srv/connect/apps/ai-models"

library(shiny)

# Define server logic
shinyServer(function(input, output) {
  
	# Print wd ####
  output$check_WD <- renderPrint({getwd()})
  
  # Print ls of wd ####
  output$check_WDls <- renderPrint({dir(getwd())})
  
})
