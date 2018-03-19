# Script header #####

library(googledrive)
drive_auth()

source("./template_fxns.R")
# source("./source/template_fxns.R") ## For local testing

## Shared folder
shared.id <- as_id("1t1DHSGPATHTD_qFDk6ads5iacQrYC5_k")

### Day1
day1.id <- drive_sub_id(shared.id, "Day 1")
day1.reg00.id <- drive_sub_id(day1.id, "regression00_ols0_runtime")
day1.reg00.data.id <- drive_sub_id(day1.reg00.id, "Data")

#*******************#

# Runtime ####

runtime.file <- "Runtime.xlsx"
## Get GDrive ID
runtime.id <- drive_sub_id(day1.reg00.data.id, runtime.file)

## Download
drive_download(runtime.id, path = runtime.file, overwrite = TRUE)