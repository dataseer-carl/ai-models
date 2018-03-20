# shinyapps.io wd:  "/srv/connect/apps/ai-models"

library(shiny)
library(markdown)

# Define UI for application
shinyUI(navbarPage(
	
  "Predictive Analytics and Applied AI for business", # Application title
  
  # Regression ####
  navbarMenu(
  	"Regression", # menu name
  	"Statistical learning", # section title
  	## _OLS ####
  	tabPanel(
  		"OLS", # tab name
  		tabsetPanel(
  			tabPanel(
  				"Run Together",
  				### Intro 
		  		fileInput("rawRuntime", "Upload Runtime.xlsx"),
		  		h1("Run Together"),
		  		includeMarkdown("readme_reg00_runtime0_desc.md"),
		  		### Data
		  		wellPanel(
			  		h2("Data"),
			  		includeMarkdown("readme_reg00_runtime1_data_a.md"),
			  		fluidRow(
			  			column(
			  				width = 4, offset = 4,
					  		wellPanel(
					  			dataTableOutput("dataRuntime")
			  				)
			  			)#column
			  		),#fluidRow
			  		includeMarkdown("readme_reg00_runtime1_data_b.md")
		  		),
		  		### EDA
		  		wellPanel(
			  		h2("EDA"),
						p("Let's visualise the data to see if there are any patterns we can exploit:"),
						plotOutput("plotRuntime", width = "600px", height = "600px"),
						includeMarkdown("readme_reg00_runtime2_eda_a.md")
		  		),
		  		### Model
		  		wellPanel(
						h2("Model"),
			  		fluidRow(
			  			column(
			  				width = 5,
				  			wellPanel(
				  				includeMarkdown("readme_reg00_runtime3_model_aa.md"),
				  				withMathJax("$$\\text{predicted RunTime} = \\text{Slope} \\cdot \\text{RunTime} + \\text{Intercept}$$"),
				  				includeMarkdown("readme_reg00_runtime3_model_ab.md"),
				  				inputPanel(
				  					numericInput(
						  				"runtime.slope", "Slope", 1,
						  				min = -3, max = 3, step = 0.10, width = "100%"
						  			),#numericInput
				  					numericInput(
						  				"runtime.intercept", "Intercept", 100,
						  				step = 10, width = "100%"
						  			)#numericInput
				  				),#inputPanel
				  				plotOutput("lineRuntime", width = "620px", height = "600px")
				  			)
				  		),#column
			  			column(
			  				width = 7,
				  			wellPanel(
				  				h3("Residuals"),
				  				includeMarkdown("readme_reg00_runtime3_model_b.md"),
				  				dataTableOutput("showPredRuntime"),
				  				h3("RMSE"),
				  				includeMarkdown("readme_reg00_runtime3_model_c.md"),
				  				fluidRow(
				  					column(
				  						5,
				  						p("Summarise the indidual errors into a single measurement, such as the RMSE; which is given by the ff formula:"),
				  						withMathJax("$$\\text{RMSE} = \\sqrt{\\frac{\\sum (\\text{Actual} - \\text{Predicted})^2}{n}}$$")
				  					),
				  					column(
				  						5, 
				  						uiOutput("textParamsRuntime"),
				  						div(tableOutput("showResidRuntime"), style = "font-size: 200%")
				  					)
				  				),#fluidRow
				  				h3("Least Squared Residuals"),
				  				helpText("Get Slope and Intercept that gives least RMSE:"),
				  				actionButton("findOLSruntime", "Least RMSE")
			  				)
			  			)#column
			  		)#fluidRow
		  		),#wellPanel
		  		wellPanel(
			  		h2("Predict"),
			  		tableOutput("predictBatch")
		  		)#wellPanel
  			),#tabPanel
  			tabPanel(
  				"Toy",
  				fluidRow(
  					column(
  						width = 4,
  						inputPanel(
		  					numericInput(
				  				"toy.slope", "Slope", 0,
				  				min = -100, max = 100, step = NA, width = "100%"
				  			),#numericInput
		  					numericInput(
				  				"toy.intercept", "Intercept", 0,
				  				min = -15, max = 15, step = NA, width = "100%"
				  			)#numericInput
  						),#inputPanel
  						plotOutput("plotToy")
  					),#column
  					column(
  						width = 6,
  						wellPanel(
	  						fluidRow(
	  							column(
	  								width = 6,
	  								wellPanel(
		  								h2("Ordinary Least Squares"),
		  								withMathJax("$$\\text{RMSE} = \\sqrt{\\frac{\\sum (\\text{Actual} - \\text{Predicted})^2}{n}}$$"),
		  								helpText("Find Slope and Intercept such that:"),
		  								actionButton("findOLStoy", "Least RMSE"),
		  								div(tableOutput("showRMSEtoy"), style = "font-size: 200%")
	  								)#wellPanel
	  							),#column
	  							column(
	  								width = 6,
	  								wellPanel(
		  								h2("Least Absolute Deviation"),
		  								withMathJax("$$\\text{MAE} = \\frac{\\sum |\\text{Actual} - \\text{Predicted}|}{n}$$"),
		  								helpText("Find Slope and Intercept such that:"),
		  								actionButton("findLADtoy", "Least MAE"),
		  								div(tableOutput("showMAEtoy"), style = "font-size: 200%")
	  								)#wellPanel
	  							)#column
	  						),#fluidRow
	  						dataTableOutput("viewToy")
	  					)#column
	  				)#fluidRow
  						)#wellPanel
  			)#tabPanel
  		)#tabsetPanel
  	),#tabPanel:OLS
  	"----",
  	"Machine learning", # section title
  	## _DT&RF ####
  	tabPanel("DT & RF"),#tabPanel:Trees&Forests
  	## _Neural net ####
  	tabPanel("Neural net")#tabPanel:NeuralNet
  ),#navbarMenu:Regression
  
  # Classification ####
  navbarMenu(
  	"Classification", # menu name
  	"Statistical learning", # section title
  	## _Logit ####
  	tabPanel("Logistic"),#tabPanel:Logistic
  	"----",
  	"Machine learning", # section title
  	## _DT&RF ####
  	tabPanel("DT & RF"),#tabPanel:Trees&Forests
  	## _Neural net ####
  	tabPanel("Neural net"),#tabPanel:NeuralNet
  	## _SVM ####
  	tabPanel("SVM")#tabPanel:SVM
  ),#navbarMenu:Classification
  
  # About ####
  tabPanel(
  	"About", # tab name
  	p("Lorem ipsum")
  	# Test ####
  	# verbatimTextOutput("check_WD"),
  	# verbatimTextOutput("check_WDls")
  ),#tabPanel
  
  # Layout config
  # selected = "",
  inverse = TRUE,
  collapsible = TRUE,
  fluid = TRUE
  
))#navbarPage, shinyUI
