## =============================================================================
## Closures review, step 14: catalogue every SESSF closures instrument on the
## Federal Register of Legislation (API api.prod.legislation.gov.au/v1), with
## in-force dates, what repealed it, and its versions (compilations).
## Output: closures/catalogue/instrument_catalogue.csv, instrument_versions.csv
## Written 17 October 2026 with the assistance of Claude (Anthropic).
## =============================================================================
api <- "https://api.prod.legislation.gov.au/v1/"
get <- function(path) jsonlite::fromJSON(paste0(api, path), simplifyVector = FALSE)
q <- function(s) utils::URLencode(s, reserved = TRUE)
terms <- c("Closures", "Closure", "SESSF Direction")
ids <- list()
for (tm in terms) {
  skip <- 0
  repeat {
    j <- get(sprintf("titles?$filter=%s&$top=100&$skip=%d", q(sprintf("contains(name,'%s')", tm)), skip))
    for (v in j$value) ids[[v$id]] <- v
    if (length(j$value) < 100) break
    skip <- skip + 100
  }
}
keep <- vapply(ids, function(v) grepl("Scalefish|SESSF", v$name) &&
                 grepl("Clos", v$name), TRUE)
ids <- ids[keep]
rows <- lapply(ids, function(v) {
  st <- v$statusHistory
  start <- if (length(st)) substr(st[[1]]$start, 1, 10) else NA
  rep <- Filter(function(s) s$status %in% c("Repealed", "Ceased", "Revoked"), st)
  end <- if (length(rep)) substr(rep[[1]]$start, 1, 10) else NA
  by <- if (length(rep) && length(rep[[1]]$reasons)) paste(vapply(rep[[1]]$reasons, function(r) {
          a <- r$affectedByTitle; if (is.null(a)) gsub("\\s+", " ", r$markdown) else paste(a$titleId, a$name) }, ""), collapse = "; ") else NA
  data.frame(id = v$id, name = v$name, made = substr(v$makingDate, 1, 10),
             registered = substr(v$asMadeRegisteredAt, 1, 10), in_force_from = start,
             no_longer_in_force_from = end, status = v$status, ended_by = by, stringsAsFactors = FALSE)
})
cat_df <- do.call(rbind, rows); cat_df <- cat_df[order(cat_df$registered), ]
## versions (compilations) of each title
vers <- do.call(rbind, lapply(cat_df$id, function(id) {
  j <- tryCatch(get(sprintf("versions?$filter=%s", q(sprintf("titleId eq '%s'", id)))), error = function(e) NULL)
  if (is.null(j) || !length(j$value)) return(NULL)
  do.call(rbind, lapply(j$value, function(x) data.frame(titleId = id,
    registerId = if (is.null(x$registerId)) NA else x$registerId,
    compilationNumber = if (is.null(x$compilationNumber)) NA else x$compilationNumber,
    start = if (is.null(x$start)) NA else substr(x$start, 1, 10),
    retrospectiveStart = if (is.null(x$retrospectiveStart)) NA else substr(x$retrospectiveStart, 1, 10),
    end = if (is.null(x$end)) NA else substr(x$end, 1, 10), stringsAsFactors = FALSE)))
}))
dir.create("closures/catalogue", showWarnings = FALSE)
write.csv(cat_df, "closures/catalogue/instrument_catalogue.csv", row.names = FALSE, na = "")
if (!is.null(vers)) write.csv(vers, "closures/catalogue/instrument_versions.csv", row.names = FALSE, na = "")
cat(nrow(cat_df), "instruments;", if (is.null(vers)) 0 else nrow(vers), "versions\n")
print(cat_df[, c("id", "registered", "in_force_from", "no_longer_in_force_from", "name")], row.names = FALSE)
