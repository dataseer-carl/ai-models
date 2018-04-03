# Initialise ################################

library(readr)
library(ggplot2)
library(dplyr)
library(Metrics)

data.path <- file.path(".", "source", "Data")

#*******************************************#

# Data ####

runtime.file <- "Runtime.csv"
runtime.filepath <- file.path(data.path, runtime.file)
runtime.df <- read_csv(
	runtime.filepath,
	col_types = cols(
		Case = col_character(),
		RunTime = col_integer(),
		RunSize = col_integer()
	)
)

# EDA ####

ggplot(runtime.df) +
	geom_point(
		aes(x = RunSize, y = RunTime),
		pch = 19, alpha = 1/2,
		colour = "steelblue", size = 6
	) +
	theme(
		plot.background = element_blank(),
		panel.background = element_blank(),
		axis.line = element_line()
	)

# Model ####

runtime.lm <- lm(RunTime ~ RunSize, data = runtime.df)

summary(runtime.lm) # Show OLS regression results

## Compute for fitted values and fitted residuals

runtime.train <- runtime.df %>% 
	mutate(
		`predicted RunTime` = predict(runtime.lm, runtime.df),
		Residuals = RunTime - `predicted RunTime`
	)

runtime.rmse <- runtime.train %>% 
	summarise(
		RMSE = rmse(RunTime, `predicted RunTime`)
	)

# Predict ####

batch.df <- data.frame(RunSize = c(100, 300))

batch.pred <- batch.df %>% 
	mutate(
		`Estimated RunTime` = predict(runtime.lm, batch.df)
	)

# Export ####

batch.file <- "out00r_runtime-batches.csv"
batch.filepath <- file.path(data.path, batch.file)
write.csv(batch.pred, batch.filepath, row.names = FALSE, quote = TRUE)
