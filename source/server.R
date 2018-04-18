# shinyapps.io wd:  "/srv/connect/apps/ai-models"

library(ggplot2)
library(ggrepel)
library(scales)
library(dplyr)
library(tidyr)
library(Metrics)
library(L1pack)
library(shiny)
library(DT)
library(stringr)
library(readxl)

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
		require(readr)
		runtime.filepath <- input$rawRuntime$datapath
		# runtime.raw <- read_excel(
		# 	runtime.filepath,
		# 	col_type = c("text", "numeric", "numeric")
		# )
		runtime.raw <- read_csv(
			runtime.filepath,
			col_types = cols(
				Case = col_character(),
				RunTime = col_integer(),
				RunSize = col_integer()
			)
		)
		runtime.raw <- runtime.raw %>% select(Case, RunSize, RunTime)
		return(runtime.raw)
	})#runtime.raw
	## Show
	output$dataRuntime <- renderDataTable(
		{
			# if(length(input$rawRuntime) == 0) return(data.frame("Error" = "Upload Runtime.xlsx"))
			runtime.df <- runtimeIngest()
			runtime.dt <- datatable(
					runtime.df,
					rownames = FALSE,
					options = list(
						pageLength = 5, lengthChange = FALSE, searching = FALSE
					)
				)
			return(runtime.dt)
		}
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
		# if(length(input$rawRuntime) == 0) return(NULL)
		runtime.gg <- runtimePlot() +
			labs(
				title = "RunTime is linear with respect to RunSize",
				subtitle = "Each additional unit in production run generally increases production time by\na fixed duration"
			)
		print(runtime.gg)
	})#plotRuntime
	### Calibrate
	output$lineRuntime <- renderPlot({
		# if(length(input$rawRuntime) == 0) return(NULL)
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
			# if(length(input$rawRuntime) == 0) return(data.frame("Error" = "Upload Runtime.xlsx"))
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
		# if(length(input$rawRuntime) == 0) return(data.frame("Error" = "Upload Runtime.xlsx"))
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
  		out.dt <- datatable(
  			out.df,
  			rownames = FALSE,
  			options = list(paging = FALSE, lengthChange = FALSE, searching = FALSE)
  		) %>% 
			formatStyle(
				c("X", "Y"),
				color = "#787c84"
			) %>% 
			formatStyle(
				c("Resid"),
				fontWeight = "bold"
			)
  		return(out.dt)
  	}
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
#   # reg02 ####
#   ## Load
# 	adsIngest <- reactive({
# 		require(readr)
# 		ads.filepath <- input$rawAds$datapath
# 		ads.df <- read_csv(
# 			ads.filepath,
# 			col_types = cols(
# 				RowID = col_character(),
# 				TV = col_number(),
# 				Radio = col_number(),
# 				Newspaper = col_number(),
# 				Sales = col_number()
# 			)
# 		)
# 		return(ads.df)
# 	})#adsIngest
# 	## Show
# 	output$dataAds <- renderDataTable(
# 		{
# 			# if(length(input$rawRuntime) == 0) return(data.frame("Error" = "Upload Runtime.xlsx"))
# 			ads.df <- adsIngest()
# 			ads.dt <- datatable(
# 					ads.df,
# 					rownames = FALSE,
# 					options = list(
# 						pageLength = 10, lengthChange = FALSE, searching = FALSE
# 					)
# 				)
# 			return(ads.dt)
# 		}
# 	)#dataAds
# 	modelAds0 <- reactive({
# 		ads.df <- adsIngest()
# 		ads.lm0 <- lm(Sales ~ TV + Radio + Newspaper, data = ads.df)
# 		return(ads.lm0)
# 	})
# 	modelAds1 <- reactive({
# 		ads.df <- adsIngest()
# 		ads.lm1 <- lm(Sales ~ TV + Radio, data = ads.df)
# 		return(ads.lm1)
# 	})
# 	modelAds2 <- reactive({
# 		ads.df <- adsIngest()
# 		ads.lm2 <- lm(Sales ~ sqrt(TV) + Radio, data = ads.df)
# 		return(ads.lm2)
# 	})
# 	olsAds <- reactive({
# 		ads.lm <- switch(
# 			input$pickModelAds,
# 			`Full additive model` = modelAds0(),
# 			`Shortlisted additive model` = modelAds1(),
# 			`Short. add. model w/ trans.` = modelAds2()
# 		)
# 		return(ads.lm)
# 	})
# 	output$showOLSads <- renderPrint({summary(olsAds())})
# 	output$eqnOLSads <- renderUI({
# 		ads.coef <- coef(olsAds())
# 		x.term <- paste(
# 			round(ads.coef, digits = 2),
# 			names(ads.coef),
# 			sep = "\\cdot",
# 			collapse = " + "
# 		)
# 		eqn <- paste("Sales", x.term, sep = " = ")
# 		eqn.print <- paste(
# 			"$$", eqn, "$$",
# 			sep = " "
# 		)
# 		withMathJax(helpText(eqn.print))
# 	})
  # reg03 ####
  ## Load
	nplIngest <- reactive({
		require(readr)
		npl.filepath <- input$rawNPL$datapath
		npl.raw <- read_csv(
			npl.filepath,
			col_types = cols(
			  Qtr.date = col_date(format = "%Y-%m-%d"),
			  Industry = col_character(),
			  Loans = col_integer(),
			  NPL = col_integer(),
			  NPL.ratio = col_double(),
			  Loans.yoy = col_double(),
			  REtag = col_character()
			)
		)
		npl.df <- npl.raw %>%
			select(
				Qtr.date, Industry, REtag, Loans, NPL, Loans.yoy, NPL.ratio
			) %>%
			mutate(
				NPLratio.log = log(NPL.ratio)
			)
		return(npl.df)
	})#npl.raw
  output$dataNPL <- renderDataTable(
		{
			# if(length(input$rawRuntime) == 0) return(data.frame("Error" = "Upload Runtime.xlsx"))
			npl.df <- nplIngest()
			npl.df <- npl.df %>% select(-NPLratio.log)
			npl.dt <- datatable(
					npl.df,
					rownames = FALSE,
					options = list(
						paging = FALSE, lengthChange = FALSE, searching = FALSE
					)
				) %>%
				formatStyle(
					c("Qtr.date", "Industry", "Loans", "NPL"),
					color = "#787c84"
				) %>%
				formatPercentage(c("Loans.yoy", "NPL.ratio"), digits = 2) %>%
				formatRound(c("Loans", "NPL"), digits = 0) %>%
				formatStyle(
					c("REtag", "Loans.yoy", "NPL.ratio"),
					fontWeight = "bold"
				) %>%
				formatStyle(
					c("NPL.ratio"),
					background = "#FDF5E6"
				)
			return(npl.dt)
		}
	)#dataRuntime
  useNPL <- reactive({
  	switch(
  		input$scaleNPL,
  		"NPL.ratio" = "NPL ratio",
  		"NPLratio.log" = "log(NPL ratio)"
  	)
  })
  scaleFxnNPL <- reactive({
  	switch(
  		input$scaleNPL,
  		"NPL.ratio" = percent,
  		"NPLratio.log" = comma
  	)
  })
  formulaNPL <- reactive({
  	NPL.y <- useNPL() %>% str_replace_all(c(" " = "."))
  	NPL.x <- "Loans.yoy"
  	if(input$plusREtag){
  		operator <- c(
  			"add" = " + ",
  			"int" = ":",
  			"cross" = " * "
  		)
  		RE.operator <- operator[match(input$configREtag, names(operator))]
  		NPL.x <- paste(NPL.x, "REtag", sep = RE.operator)
  	}
  	NPL.eqn <- as.formula(paste(NPL.y, NPL.x, sep = " ~ "))
  	return(NPL.eqn)
  })
  lmNPL <- reactive({
  	loans.df <- nplIngest()
  	NPL.eqn <- formulaNPL()
  	NPL.lm <- lm(NPL.eqn, data = loans.df)
  	return(NPL.lm)
  })
  output$showlmNPL <- renderPrint({
  	summary(lmNPL())
  })
  output$showFormulaNPL <- renderPrint({
  	print(formulaNPL())
  })
  coefNPL <- reactive({

  	npl.lm <- lmNPL()

		vec2df <- function(x) data.frame(X1 = names(x), X2 = x, stringsAsFactors = FALSE)

		base.x <- "Loans.yoy"
		dummy.x <- "REtag"
		dummy.relevel <- "non-Real Estate"

		coef.df <- coef(npl.lm) %>%
			vec2df() %>%
			rename(Variable = X1, Coef = X2) %>%
			mutate(
				isSlope = Variable %>% str_detect(base.x),
				isBase = Variable %in% c("(Intercept)", base.x),
				X = ifelse(isSlope, "Slope", "Intercept"),
				Dummies = Variable %>%
					str_extract(
						paste0("(?<=", dummy.x, ").+$")
					)
			) %>%
			replace_na(list(Dummies = dummy.relevel)) %>%
			group_by(X) %>%
			arrange(Variable) %>%
			mutate(
				Coef.adj = (`!`(isBase) * Coef) + sum(isBase * Coef)
			) %>%
			ungroup() %>%
			select(X, Dummies, Coef.adj) %>%
			complete(X, Dummies) %>%
			group_by(X) %>%
			arrange(Coef.adj) %>%
			fill(Coef.adj) %>%
			ungroup() %>%
			spread("X", "Coef.adj") %>%
			rename(REtag = Dummies)
		if(!input$plusREtag) coef.df <- coef.df %>% mutate(REtag = "All")
		return(coef.df)
  })
  output$plotNPL <- renderPlot({
		# if(length(input$rawRuntime) == 0) return(NULL)
  	loans.df <- nplIngest()

  	loans.df$NPL.y = loans.df[,input$scaleNPL]
  	labNPL <- scaleFxnNPL()

  	coef.df <- coefNPL()

  	npl.gg <- if(input$plusREtag){
  		ggplot(loans.df) +
  			aes(x = Loans.yoy, y = NPL.y, label = Industry, colour = REtag) +
  			geom_abline(
					aes(slope = Slope, intercept = Intercept, colour = REtag),
					data = coef.df
				) +
  			scale_colour_discrete(guide = FALSE)
  	} else{
  		ggplot(loans.df) + 
  			geom_abline(
					aes(slope = Slope, intercept = Intercept),
					data = coef.df
				) +
  			aes(x = Loans.yoy, y = NPL.y, label = Industry)
  	}

		npl.gg <- npl.gg +
			geom_point() +
			scale_x_continuous(labels = percent, name = "YoY growth in loans", limits = c(-0.075, 0.175)) +
			scale_y_continuous(labels = labNPL, name = useNPL()) +
			theme(
				panel.background = element_blank(),
				panel.grid = element_blank(),
				axis.line = element_line(colour = "black")
			)
		if(input$labelsNPL) npl.gg <- npl.gg + geom_text_repel(colour = "#787c84")
		print(npl.gg)
	})#plotNPL
  output$showCoefNPL <- renderTable({
  	coef.df <- coefNPL()
  	return(coef.df)
  })
  
	# CV00 ####
	# CV01 ####
	# CV02 ####
	
  # Workshop ####
  scoreClaims <- reactive({
  	
  	answer.path <- input$answerClaims$datapath
  	# answer.path <- file.path(".", "source", "Data", "potential-claims predictions.rds")
  	key.df <- readRDS(answer.path)
  	
  	submit.path <- input$submitClaims$datapath
  	# submit.path <- file.path(".", "source", "Data", "sample00.xlsx")
  	
  	teams <- excel_sheets(submit.path)
  	submit.ls <- lapply(
			as.list(teams),
			function(temp.team){
				temp.df <- read_excel(submit.path, sheet = temp.team, col_names = TRUE)
				temp.df <- temp.df %>%
					mutate(Team = temp.team) %>% 
					rename(Claims.hat = `Total Claim Amount`)
				return(temp.df)
			}
		)
		names(submit.ls) <- teams
		
		submit.df <- do.call(bind_rows, submit.ls)
		results.df <- submit.df %>% 
			left_join(key.df) %>% 
			group_by(Team) %>% 
			summarise(RMSE = rmse(`Total Claim Amount`, Claims.hat)) %>% 
			ungroup() %>% 
			mutate(Rank = rank(RMSE))
  	
		return(results.df)
		
  })#scoreClaims
  output$rankClaims <- renderDataTable(
  	{
  		scores.df <- scoreClaims()
  		scores.dt <- scores.df %>% 
  			select(Rank, Team, RMSE) %>% 
  			arrange(RMSE) %>% 
  			datatable(
  				rownames = FALSE,
  				options = list(
			  		paging = FALSE, lengthChange = FALSE, searching = FALSE, 
			  		ordering = FALSE, info = FALSE
			  	)
  			) %>% 
  			formatStyle(
  				"Rank", target = "row",
  				backgroundColor = styleEqual(
  					1:3,
  					c("gold", "silver", "orange")
  				)
  			) %>% 
  			formatRound("RMSE")
  		# https://rstudio.github.io/DT/010-style.html
  		return(scores.dt)
  	}
  )#showScores
  # Workshop ####
  scoreHousing <- reactive({
  	
  	answer.path <- input$answerHousing$datapath
  	# answer.path <- file.path(".", "source", "Data", "potential-claims predictions.rds")
  	key.df <- readRDS(answer.path)
  	
  	submit.path <- input$submitHousing$datapath
  	# submit.path <- file.path(".", "source", "Data", "sample00.xlsx")
  	
  	teams <- excel_sheets(submit.path)
  	submit.ls <- lapply(
			as.list(teams),
			function(temp.team){
				temp.df <- read_excel(submit.path, sheet = temp.team, col_names = TRUE)
				temp.df <- temp.df %>%
					mutate(Team = temp.team) %>% 
					rename(Price.hat = `Scored Labels`)
				return(temp.df)
			}
		)
		names(submit.ls) <- teams
		
		submit.df <- do.call(bind_rows, submit.ls)
		results.df <- submit.df %>% 
			left_join(key.df) %>% 
			group_by(Team) %>% 
			summarise(RMSE = rmse(Price, Price.hat)) %>% 
			ungroup() %>% 
			mutate(Rank = rank(RMSE))
  	
		return(results.df)
		
  })#scoreHousing
  output$rankHousing <- renderDataTable(
  	{
  		scores.df <- scoreHousing()
  		scores.dt <- scores.df %>% 
  			select(Rank, Team, RMSE) %>% 
  			arrange(RMSE) %>% 
  			datatable(
  				rownames = FALSE,
  				options = list(
			  		paging = FALSE, lengthChange = FALSE, searching = FALSE, 
			  		ordering = FALSE, info = FALSE
			  	)
  			) %>% 
  			formatStyle(
  				"Rank", target = "row",
  				backgroundColor = styleEqual(
  					1:3,
  					c("gold", "silver", "orange")
  				)
  			) %>% 
  			formatRound("RMSE")
  		# https://rstudio.github.io/DT/010-style.html
  		return(scores.dt)
  	}
  )#showScores
  # Workshop ####
  scoreCredit <- reactive({
  	
  	answer.path <- input$answerCredit$datapath
  	# answer.path <- file.path(".", "source", "Data", "potential-claims predictions.rds")
  	key.df <- readRDS(answer.path)
  	
  	submit.path <- input$submitCredit$datapath
  	# submit.path <- file.path(".", "source", "Data", "sample00.xlsx")
  	
  	teams <- excel_sheets(submit.path)
  	submit.ls <- lapply(
			as.list(teams),
			function(temp.team){
				temp.df <- read_excel(submit.path, sheet = temp.team, col_names = TRUE)
				temp.df <- temp.df %>% mutate(Team = temp.team, ID = as.character(1:n()))
				names(temp.df)[1] <- "Default.hat"
				return(temp.df)
			}
		)
		names(submit.ls) <- teams
		
		submit.df <- do.call(bind_rows, submit.ls)
		results.df <- submit.df %>% 
			left_join(key.df) %>% 
			group_by(Team) %>% 
			summarise(Accuracy = accuracy(Default, Default.hat)) %>% 
			ungroup()
		val.df <- results.df %>% 
			select(Accuracy) %>% 
			distinct() %>% 
			mutate(Rank = rank(-Accuracy, ties.method = "min"))
		rank.df <- results.df %>% left_join(val.df)
		return(rank.df)
		
  })#scoreCredit
  output$rankCredit <- renderDataTable(
  	{
  		scores.df <- scoreCredit()
  		scores.dt <- scores.df %>% 
  			select(Rank, Team, Accuracy) %>% 
  			arrange(desc(Accuracy)) %>% 
  			datatable(
  				rownames = FALSE,
  				options = list(
			  		paging = FALSE, lengthChange = FALSE, searching = TRUE, 
			  		ordering = FALSE, info = FALSE
			  	)
  			) %>% 
  			formatStyle(
  				"Rank", target = "row",
					backgroundColor = styleInterval(
  					c(1.99, 2.99, 3.99),
  					c("gold", "silver", "orange", "white")
  				)
  			) %>% 
  			formatPercentage("Accuracy", digits = 4)
  		# https://rstudio.github.io/DT/010-style.html
  		return(scores.dt)
  	}
  )#showScores
  
	  # Test ####
# 	# Print wd ###
#   output$check_WD <- renderPrint({getwd()})
#   
#   # Print ls of wd ###
#   output$check_WDls <- renderPrint({dir(getwd())})
  
})
