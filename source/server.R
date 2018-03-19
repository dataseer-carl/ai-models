# shinyapps.io wd:  "/srv/connect/apps/ai-models"

library(shiny)

runtime.df <- runtime.raw

# Define server logic
shinyServer(function(input, output) {

	# reg00 ####
	output$dataRuntime <- renderDataTable(
		{
			return(runtime.df)
		},
		options = list(
			pageLength = 5, lengthChange = FALSE, searching = FALSE
		)
	)#dataRuntime
	
	  # Test ####
# 	# Print wd ###
#   output$check_WD <- renderPrint({getwd()})
#   
#   # Print ls of wd ###
#   output$check_WDls <- renderPrint({dir(getwd())})
  
})
