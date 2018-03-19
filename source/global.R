# Script header #####

## proxy data://
# proxydata.path <- file.path(".", "source", "Data") ## For local testing
proxydata.path <- file.path(".", "Data")

#*******************#

# Runtime ####

library(readxl)

runtime.file <- "Runtime.xlsx"
runtime.filepath <- file.path(proxydata.path, runtime.file)
runtime.raw <- read_excel(
	runtime.filepath,
	col_type = c("text", "numeric", "numeric")
)
