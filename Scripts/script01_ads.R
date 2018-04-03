# Initialise ################################

library(readr)
library(ggplot2)
library(dplyr)
library(Metrics)
library(GGally)

data.path <- file.path(".", "source", "Data")

#*******************************************#

# Data ####

ads.file <- "Advertising.csv"
ads.filepath <- file.path(data.path, ads.file)
ads.df <- read_csv(
	ads.filepath,
	col_types = cols(
		RowID = col_character(),
		TV = col_number(),
		Radio = col_number(),
		Newspaper = col_number(),
		Sales = col_number()
	)
)

## Remove Row.ID
ads.df <- ads.df %>% select(-RowID)

# EDA ####

ggpairs(ads.df) +
	theme(
		plot.background = element_blank(),
		panel.background = element_rect(fill = NA, colour = "#787c84"),
		panel.grid = element_blank(),
		strip.background = element_rect(fill = "#787c84", colour = NA),
		strip.text = element_text(colour = "white")
	)

# Model ####

## Full additive model

ads.lm0 <- lm(Sales ~ TV + Radio + Newspaper, data = ads.df)

summary(ads.lm0) # Show OLS regression results

## Less nuisance

ads.lm1 <- update(ads.lm0, . ~ . - Newspaper)
## Same as:
# ads.lm1 <- lm(Sales ~ TV + Radio, data = ads.df)

summary(ads.lm1)

## sqrt(TV)

ads.lm2 <- lm(Sales ~ sqrt(TV) + Radio, data = ads.df)

summary(ads.lm2)

# Interpret ####

## Radio
coef(ads.lm2)["Radio"] * 1000 * 0.20
## Each dollar ads USD 39 in sales

## TV
TV.base <- data.frame(TV = 0:300) %>% 
	mutate(
		TV.Sales = sqrt(TV) * coef(ads.lm2)["sqrt(TV)"] * 1000 * 0.20,
		TV.marg.returns = TV.Sales - lag(TV.Sales)
	)

ggplot(TV.base) +
	geom_line(
		aes(x = TV, y = TV.marg.returns),
		size = 1.5, colour = "steelblue"
	) +
	# scale_x_continuous(limits = c(2, 300)) +
	# scale_y_continuous(limits = c(0, 100)) +
	theme(
		plot.background = element_blank(),
		panel.background = element_blank(),
		axis.line = element_line()
	)

# Export ####

batch.file <- "out00r_ads-batches.csv"
batch.filepath <- file.path(data.path, batch.file)
write.csv(batch.pred, batch.filepath, row.names = FALSE, quote = TRUE)
