## Closures review, step 15: fetch the text of every catalogued instrument not
## already in closures/text/, and of the compiled versions.
cat_df <- read.csv("closures/catalogue/instrument_catalogue.csv", stringsAsFactors = FALSE)
vers <- read.csv("closures/catalogue/instrument_versions.csv", stringsAsFactors = FALSE)
have <- sub("\\.txt$", "", list.files("closures/text", pattern = "\\.txt$"))
args <- character()
for (i in seq_len(nrow(cat_df))) if (!cat_df$id[i] %in% have) args <- c(args, paste0(cat_df$id[i], "=", cat_df$registered[i]))
cv <- vers[grepl("C[0-9]{5}$", vers$registerId) & !vers$registerId %in% have, ]
for (i in seq_len(nrow(cv))) args <- c(args, sprintf("%s=%s@%s", cv$registerId[i], cv$titleId[i], cv$start[i]))
cat(length(args), "to fetch\n")
if (length(args)) {
  commandArgs <- function(trailingOnly = TRUE) args
  source("closures/R/08_fetch_register_text.R", local = TRUE)
}
