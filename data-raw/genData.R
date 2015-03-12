
# read raw JSON file
json.file <- "inst/extdata/james.json"
james <- readJAMES(json.file)

# save as binary data
devtools::use_data(james, overwrite = TRUE)

