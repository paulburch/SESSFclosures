## =============================================================================
## Closures review, step 21: download the authorised PDF of every closures
## instrument (as made) and every compilation from the Federal Register of
## Legislation, to closures/legislation/<registerId>.pdf, and list them with
## their page counts in closures/output/legislation_pdfs.csv.
## Register API: https://api.prod.legislation.gov.au/v1/documents (OData).
## Written 17 October 2026 with the assistance of Claude (Anthropic).
## =============================================================================
suppressPackageStartupMessages(library(jsonlite))
cat_df <- read.csv("closures/catalogue/instrument_catalogue.csv", stringsAsFactors = FALSE)
vers   <- read.csv("closures/catalogue/instrument_versions.csv", stringsAsFactors = FALSE)
ids <- unique(c(cat_df$id, vers$registerId[grepl("C[0-9]{5}$", vers$registerId)]))
dir.create("closures/legislation", showWarnings = FALSE)
api <- "https://api.prod.legislation.gov.au/v1/documents?$filter="
out <- list()
for (id in ids) {
  q <- URLencode(sprintf("registerId eq '%s' and format eq 'Pdf' and type eq 'Primary'", id), reserved = TRUE)
  d <- tryCatch(fromJSON(paste0(api, q, "&$select=titleId,registerId,start,retrospectiveStart,rectificationVersionNumber,uniqueTypeNumber,volumeNumber,compilationNumber,pageCount")), error = function(e) NULL)
  v <- if (!is.null(d)) d$value else NULL
  if (is.null(v) || !NROW(v)) { cat(id, ": no PDF listed\n"); out[[id]] <- data.frame(register_id = id, title_id = NA, start = NA, pages = NA, file = NA, url = NA); next }
  v <- v[order(-v$rectificationVersionNumber), ][1, ]
  url <- sprintf("https://api.prod.legislation.gov.au/v1/documents(titleId='%s',start=%s,retrospectiveStart=%s,rectificationVersionNumber=%d,type='Primary',uniqueTypeNumber=%d,volumeNumber=%d,format='Pdf')",
                 v$titleId, v$start, v$retrospectiveStart, v$rectificationVersionNumber, v$uniqueTypeNumber, v$volumeNumber)
  f <- file.path("closures/legislation", paste0(id, ".pdf"))
  if (!file.exists(f)) tryCatch(download.file(url, f, mode = "wb", quiet = TRUE), error = function(e) cat(id, ": download failed\n"))
  web <- if (grepl("C[0-9]{5}$", id)) sprintf("https://www.legislation.gov.au/%s/%s/%s/text", v$titleId, substr(v$start, 1, 10), substr(v$start, 1, 10))
         else sprintf("https://www.legislation.gov.au/%s/asmade/text", id)
  out[[id]] <- data.frame(register_id = id, title_id = v$titleId, start = substr(v$start, 1, 10),
                          pages = if (file.exists(f)) pdftools::pdf_info(f)$pages else NA, file = f, url = web)
}
out <- do.call(rbind, out)
out$name <- cat_df$name[match(out$title_id, cat_df$id)]
write.csv(out, "closures/output/legislation_pdfs.csv", row.names = FALSE, na = "")
cat(sum(!is.na(out$pages)), "of", nrow(out), "PDFs downloaded\n")
