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
		  		fileInput("rawRuntime", "Upload Runtime.csv"),
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
					  			DT::dataTableOutput("dataRuntime")
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
						# fluidRow(
						# 	column(
						# 		width = 12,
						# 		p()
						# 	)#column
						# ),#fluidRow
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
				  				DT::dataTableOutput("showPredRuntime"),
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
			  		div(tableOutput("predictBatch"), style = "font-size: 250%")
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
	  						DT::dataTableOutput("viewToy")
							)#wellPanel
  					)#column
					),#fluidRow
					fluidRow(
						h2("Other Error Measures"),
						column(
							width = 4,
							wellPanel(
								h2("Relative Squared Error"),
								withMathJax("$$\\text{RSE} = \\frac{\\sum (\\text{Actual} - \\text{Predicted})^2}{\\sum (\\text{Actual} - \\overline{\\text{Actual}})^2}$$")
							)
						),#column
						column(
							width = 4,
							wellPanel(
								h2("Relative Absolute Error"),
								withMathJax("$$\\text{RAE} = \\frac{\\sum |\\text{Actual} - \\text{Predicted}|}{\\sum |\\text{Actual} - \\overline{\\text{Actual}}|}$$")
							)
						),#column
						column(
							width = 4,
							wellPanel(
								h2("Coefficient of Determination"),
								helpText("Proportion of variation in the response variable explained by the model"),
								withMathJax("$$R^2 = 1 - \\frac{\\text{Var}(\\text{Residuals})}{\\text{Var}(\\text{Actual})}$$")
							)
						)#column
					)#fluidRow
  			),#tabPanel
#   			tabPanel(
#   				"Advertising",
#   				fileInput("rawAds", "Upload Advertising.csv"),
# 		  		h1("Fudge"),
#   				includeMarkdown("readme_reg01_ads0_desc.md"),
			  	### Data
# 					wellPanel(
# 			  		h2("Data"),
# 			  		includeMarkdown("readme_reg01_ads1_data_a.md"),
# 			  		fluidRow(
# 			  			column(
# 			  				width = 4, offset = 4,
# 					  		wellPanel(
# 					  			DT::dataTableOutput("dataAds")
# 			  				)
# 			  			)#column
# 			  		),#fluidRow
# 			  		includeMarkdown("readme_reg01_ads1_data_b.md")
# 					),
# 					wellPanel(
# 						h2("Model"),
# 						fluidRow(
# 							column(
# 								width = 5,
# 								radioButtons(
# 									"pickModelAds", "Pick model to show:",
# 									c(
# 										"Full additive model",
# 										"Shortlisted additive model",
# 										"Short. add. model w/ trans."
# 									),
# 									inline = TRUE
# 								),
# 								verbatimTextOutput("showOLSads")
# 							),#column
# 							column(
# 								width = 7,
# 								uiOutput("eqnOLSads")
# 							)#column
# 						)#fluidRow
# 					),#wellPanel
# 					wellPanel(
# 						h2("Interpretation"),
# 						column(
# 							width = 12,
# 							p("Lorem ipsum")
# 						)#column
# 					)#wellPanel
#   			)#tabPanel
				tabPanel(
					"Loans: NPL",
					fileInput("rawNPL", "Upload DBS_lon_npl_wREtag.csv"),
		  		h1("Yin-Yang of Banking"),
		  		includeMarkdown("readme_reg03_npl0_desc.md"),
					wellPanel(
			  		h2("Data"),
			  		fluidRow(
			  			column(
			  				width = 8, offset = 0,
					  		wellPanel(
					  			DT::dataTableOutput("dataNPL")
			  				)
			  			),#column
		  				column(
		  					width = 4,
		  					h3("Field descriptions"),
		  					tags$ul(
					  			tags$li(tags$strong("Qtr.date"), ":= as of quarter-end date"),
					  			tags$li(tags$strong("Industry"), ":= Name of industry"),
					  			tags$li(tags$strong("REtag"), ":= Industry Real Estate tag"),
					  			tags$li(tags$strong("Loans"), ":= Gross customer loans (in SGD m)"),
					  			tags$li(tags$strong("NPL"), ":= Non-performing loans (in SGD m)"),
					  			tags$li(tags$strong("Loans.yoy"), ":= Year-on-year gross customer loans growth rate"),
					  			tags$li(tags$strong("NPL.ratio"), ":= NPL / Loans")
					  		),#tags$ul
		  					wellPanel(
		  						p(
		  							"Problem can be answered by predicting",
		  							tags$strong("NPL.ratio"),
		  							"using",
		  							tags$em("Loans.yoy"), "and", tags$em("REtag")
		  						)
		  					)
		  				)#column
			  		)#fluidRow
		  		),#wellPanel
					h2("Model"),
					fluidRow(
						column(
							width = 5,
							inputPanel(
								radioButtons(
									"scaleNPL", "NPL transformation:",
									choices = c("NPL ratio" = "NPL.ratio", "log(NPL ratio)" = "NPLratio.log"),
									inline = TRUE
								)
							),#inputPanel
							checkboxInput("labelsNPL", "Show labels?", TRUE),
							plotOutput("plotNPL"),
							h3("Effective Coefficients"),
							div(
								tableOutput("showCoefNPL"),
								align = "center"
							)
						),#column
						column(
							width = 7,
							wellPanel(
								checkboxInput("plusREtag", "Include Real Estate tag?", FALSE),
								conditionalPanel(
									condition = "input.plusREtag",
									radioButtons(
										"configREtag", "How should Real Estate tag be incorporated?",
										choices = c(
											"Additive" = "add",
											"Interaction only" = "int",
											"Cross-product" = "cross"
										),
										inline = TRUE
									)#radioButtons
								),#conditionalPanel
								h3("R Formula"),
								verbatimTextOutput("showFormulaNPL"),
								h3("Fitted OLS"),
								verbatimTextOutput("showlmNPL")
							)#wellPanel
						)#column
					)#fluidRow
				)#tabPanel
  		)#tabsetPanel
  	),#tabPanel:OLS
  	"----",
  	"Machine learning", # section title
  	## _DT&RF ####
  	tabPanel(
  		"DT & RF",
  		h1("Decision tree"),
  		tags$ul(
  			tags$li("Leaf nodes:sometimes referred to as size of tree"),
  			tags$li("Non-leaf nodes"),
  			tags$li("Pruning: reducing size of tree to reduce complexity")
  		),
  		h1("Random forest"),
  		tags$ul(
  			tags$li("Bagging"),
  			tags$li("Random subspace sampling"),
  			tags$li("mtry"),
  			tags$li("ntrees")
  		)
  	),#tabPanel:Trees&Forests
  	## _Neural net ####
  	tabPanel("Neural net")#tabPanel:NeuralNet
  ),#navbarMenu:Regression
  
  # Model Eval ####
  navbarMenu(
  	"Model Evaluation",
  	tabPanel(
  		"Train-test split",
  		p("Lorem ipsum")
  	),#tabPanel:traintestsplit
  	tabPanel(
  		"Cross-validation",
  		p("Lorem ipsum")
  	),#tabPanel:Cross-validation
  	"----",
  	tabPanel(
  		"Feature selection",
  		p("Lorem ipsum")
  	)#tabPanel:FeatureSelection
  ),#navbarMenu:ModelEval
  
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
  
	# Workshops ####
  navbarMenu(
  	"Workshop",
  	tabPanel(
  		"Day 1: Potential claims",
  		# fileInput("answerkeyDay1", "Upload Runtime.csv"),
  		h1("Potential claims prediction"),
  		p("Day 1 Workshop"),
  		fluidRow(
  			column(
  				width = 7,
  				wellPanel(
  					includeMarkdown("readme_workshop_day1.md")
  				)
  				# div(img(src = "car-insurance1.jpg"), align = "center", style = "width: 150px;"),
  			),#column
  			column(
  				width = 5,
  				fileInput("answerClaims", "Answer key"),
  				fileInput("submitClaims", "Submissions"),
  				div(
						dataTableOutput("rankClaims"),
						tags$style(type="text/css", "#view tr:last-child {font-weight:bold;}")
					)
  			)
  		)#fluidRow
  	),#tabPanel
  	tabPanel(
  		"Day 2: Regression",
  		p("Lorem ipsum")
  	),#tabPanel
  	tabPanel(
  		"Day 3: Classification",
  		p("Lorem ipsum")
  	)#tabPanel
  ),#navbarMenu

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
