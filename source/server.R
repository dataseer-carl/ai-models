# shinyapps.io wd:  "/srv/connect/apps/ai-models"

library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(DT)
library(Metrics)
library(L1pack)

endLab <- function(first.lab, last.lab){
	temp.fxn <- function(x){
		x.lab <- c(
			first.lab,
			rep("", length(x) - 2),
			last.lab
		)
		x.out <- paste(x, x.lab)
		x.out <- trimws(x.out, "right")
		return(x.out)
	}
	return(temp.fxn)
}

# runtime.df <- runtime.raw

# Define server logic
shinyServer(function(input, output, session) {

	# reg00 ####
	## Load
	runtimeIngest <- reactive({
		require(readxl)
		runtime.filepath <- input$rawRuntime$datapath
		runtime.raw <- read_excel(
			runtime.filepath,
			col_type = c("text", "numeric", "numeric")
		)
		runtime.raw <- runtime.raw %>% select(Case, RunSize, RunTime)
		return(runtime.raw)
	})#runtime.raw
	## Show
	output$dataRuntime <- renderDataTable(
		{
			if(length(input$rawRuntime) == 0) return(data.frame("Error" = "Upload Runtime.xlsx"))
			runtime.df <- runtimeIngest()
			return(runtime.df)
		},
		rownames = FALSE,
		options = list(
			pageLength = 5, lengthChange = FALSE, searching = FALSE
		)
	)#dataRuntime
	### Explore
	runtimePlot <- reactive({
		lastMins <- endLab("mins", "mins")
		lastUnits <- endLab("units", "units")
		runtime.df <- runtimeIngest()
		runtime.gg <- ggplot(runtime.df) +
			geom_point(
				aes(x = RunSize, y = RunTime),
				pch = 19, alpha = 1/2, size = 6, colour = "steelblue"
			) +
			scale_x_continuous(labels = lastUnits) +
			scale_y_continuous(labels = lastMins) +
			theme(
				text = element_text(size = 12),
				plot.title = element_text(size = 22),
				plot.subtitle = element_text(size = 16, colour = "#787c84"),
				plot.background = element_rect(colour = "#787c84", fill = NA),
				panel.background = element_blank(),
				panel.grid = element_blank(),
				axis.line = element_line(colour = "black")
			)
		return(runtime.gg)
	})#runtimePlot
	output$plotRuntime <- renderPlot({
		if(length(input$rawRuntime) == 0) return(NULL)
		runtime.gg <- runtimePlot() +
			labs(
				title = "RunTime is linear with respect to RunSize",
				subtitle = "Each additional unit in production run generally increases production time by\na fixed duration"
			)
		print(runtime.gg)
	})#plotRuntime
	### Calibrate
	output$lineRuntime <- renderPlot({
		if(length(input$rawRuntime) == 0) return(NULL)
		runtime.gg <- runtimePlot() +
			geom_abline(
				slope = input$runtime.slope,
				intercept = input$runtime.intercept,
				linetype = "dashed", size = 1.5, colour = "firebrick"
			)
		print(runtime.gg)
	})#plotRuntime
	predRuntime <- reactive({
		runtime.raw <- runtimeIngest()
		runtime.df <- runtime.raw %>% 
			mutate(
				`predicted RunTime` = input$runtime.slope * RunSize + input$runtime.intercept,
				Residual = RunTime - `predicted RunTime`
			)
		return(runtime.df)
	})
	output$showPredRuntime <- renderDataTable(
		{
			if(length(input$rawRuntime) == 0) return(data.frame("Error" = "Upload Runtime.xlsx"))
			runPred.df <- predRuntime()
			runPred.df <- runPred.df %>% 
				mutate_at(
					vars(`predicted RunTime`, Residual),
					funs(round(., digits = 2))
				)
			runPred.dt <- datatable(
					runPred.df,
					rownames = FALSE,
					options = list(
						pageLength = 10, lengthChange = FALSE, searching = FALSE
					)
				) %>% 
				formatStyle(
					c("Case", "RunSize", "RunTime"),
					color = "#787c84"
				) %>% 
				formatStyle(
					c("Residual"),
					fontWeight = "bold"
				)
			return(runPred.dt)
		}
		# https://rstudio.github.io/DT/010-style.html
		# http://shiny.rstudio.com/gallery/datatables-options.html
	)
	residRuntime <- reactive({
		if(length(input$rawRuntime) == 0) return(data.frame("Error" = "Upload Runtime.xlsx"))
		predRuntime.df <- predRuntime()
		residRuntime.df <- predRuntime.df %>% 
			summarise(
				RMSE = rmse(RunTime, `predicted RunTime`)
			)
		return(residRuntime.df)
	})
	output$showResidRuntime <- renderTable({residRuntime()}, digits = 4)
	output$textParamsRuntime <- renderUI({
		p(
			paste(
				"RMSE of model with Slope of", input$runtime.slope,
				"and Intercept of", paste0(input$runtime.intercept, ":")
			)
		)
	})
	olsRuntime <- reactive({
		runtime.df <- runtimeIngest()
		ols.lm <- lm(RunTime ~ RunSize, data = runtime.df)
		ols.coef <- coef(ols.lm)
		return(ols.coef)
	})
	observeEvent(
  	input$findOLSruntime,
  	{
  		ols.coef <- olsRuntime() %>% unname() # Error if named vector
  		updateNumericInput(session, "runtime.slope", value = ols.coef[2])
	  	updateNumericInput(session, "runtime.intercept", value = ols.coef[1])
  	}
  )
	### Predict
	output$predictBatch <- renderTable({
		runtime.coef <- olsRuntime() %>% unname() # Error if named vector
		runtime.batch <- tibble(
			`Batch size` = c(100, 300),
			`Unit production rate` = input$runtime.slope,
			`Production lead time` = input$runtime.intercept
			) %>% 
			mutate(
				`Estimated RunTime` = `Batch size` * `Unit production rate` + `Production lead time`
			)
		return(runtime.batch)
	})
	# reg01 ####
	toyIngest <- reactive({
		toy.df <- data.frame(
			X = c(2, 5, 9, 11),
			Y = c(5, 8, 7, 14)
		)
		return(toy.df)
	})
	olsToy <- reactive({
		toy.df <- toyIngest()
		ols.lm <- lm(Y ~ X, data = toy.df)
		ols.coef <- coef(ols.lm)
		return(ols.coef)
	})
	ladToy <- reactive({
		toy.df <- toyIngest()
		lad.lm <- lad(Y ~ X, data = toy.df)
		lad.coef <- coef(lad.lm)
		return(lad.coef)
	})
	### Plot
	output$plotToy <- renderPlot({
		toy.df <- toyIngest()
  	plot.gg <- ggplot() +
			geom_abline(
				slope = input$toy.slope, intercept = input$toy.intercept,
				colour = "steelblue", alpha = 1, size = 1.5
			) +
			geom_point(
				aes(x = X, y = Y),
				size = 6,
				data = toy.df
			) +
			scale_x_continuous(limits = c(0, 13), name = "x") +
			scale_y_continuous(limits = c(0, 15), name = "y") +
			theme(
				panel.background = element_blank(),
				panel.grid = element_blank(),
				axis.line = element_line(colour = "black")
			)
  	print(plot.gg)
  })#plotToy
	residToy <- reactive({
		toy.df <- toyIngest()
		ann.df <- toy.df %>% 
  		mutate(
  			Y.hat = X * input$toy.slope + input$toy.intercept,
  			Resid = Y - Y.hat
  		)
  	return(ann.df)
	})
	output$viewToy <- renderDataTable(
  	{
  		out.df <- residToy()
  		out.df <- out.df %>% 
  			mutate(
  				Y.hat = round(Y.hat),
  				Resid = round(Resid, digits = 4)
  			)
  	},
  	rownames = FALSE,
  	options = list(paging = FALSE, lengthChange = FALSE, searching = FALSE)
  )
	observeEvent(
  	input$findOLStoy,
  	{
  		ols.coef <- olsToy() %>% unname() # Error if named vector
  		updateNumericInput(session, "toy.slope", value = ols.coef[2])
	  	updateNumericInput(session, "toy.intercept", value = ols.coef[1])
  	}
  )
  observeEvent(
  	input$findLADtoy,
  	{
  		lad.coef <- ladToy() %>% unname() # Error if named vector
  		updateNumericInput(session, "toy.slope", value = lad.coef[2])
	  	updateNumericInput(session, "toy.intercept", value = lad.coef[1])
  	}
  )
  output$showRMSEtoy <- renderTable(
  	{
			toy.df <- residToy()
			toy.resid <- toy.df %>% 
				summarise(
					RMSE = rmse(Y, Y.hat)
				)
			return(toy.resid)
		},
		digits = 4
	)
  output$showMAEtoy <- renderTable(
  	{
			toy.df <- residToy()
			toy.resid <- toy.df %>% 
				summarise(
					MAE = mae(Y, Y.hat)
				)
			return(toy.resid)
		},
		digits = 4
	)

	  # Test ####
# 	# Print wd ###
#   output$check_WD <- renderPrint({getwd()})
#   
#   # Print ls of wd ###
#   output$check_WDls <- renderPrint({dir(getwd())})
  
})
