## =============================================================================
## Closures review, step 8: read instrument text from the Federal Register of
## Legislation (HTML text view of each "as made" instrument) and save it in the
## same layout as the PDF extracts (closures/text/<id>.txt), with table rows
## joined onto one line ("2   38° 01.917' south   149° 19.630' east").
## Usage: Rscript closures/R/08_fetch_register_text.R F2023L00439=2023-04-14 ...
## Written 17 October 2026 with the assistance of Claude (Anthropic).
## =============================================================================
args <- commandArgs(TRUE)
for (a in args) {
  ## "ID=DATE"        as made, registered on DATE
  ## "KEY=TITLE@DATE" a compiled version of TITLE starting on DATE, saved as KEY
  id <- sub("=.*", "", a); dt <- sub(".*=", "", a)
  url <- if (grepl("@", dt)) sprintf("https://www.legislation.gov.au/%s/%s/%s/text/original/epub/OEBPS/document_1/document_1.html",
                                     sub("@.*", "", dt), sub(".*@", "", dt), sub(".*@", "", dt)) else
         sprintf("https://www.legislation.gov.au/%s/asmade/%s/text/original/epub/OEBPS/document_1/document_1.html", id, dt)
  doc <- tryCatch(xml2::read_html(url), error = function(e) NULL)
  if (is.null(doc)) { cat(sprintf("%s: FAILED %s\n", id, url)); next }
  ## block elements -> lines; table cells -> tab separated
  for (n in xml2::xml_find_all(doc, "//td|//th")) xml2::xml_add_child(n, "span", "\t")
  for (n in xml2::xml_find_all(doc, "//p|//div|//tr|//li|//h1|//h2|//h3|//h4|//br")) xml2::xml_add_sibling(n, "span", "\n")
  tx <- xml2::xml_text(xml2::xml_find_first(doc, "//body"))
  l <- strsplit(tx, "\n")[[1]]
  l <- gsub("[\t ]+", " ", trimws(gsub(" ", " ", l)))
  l <- gsub("º", "°", l)                         # masculine ordinal used as degree
  l <- gsub("[’‘′]", "'", l)
  l <- l[nzchar(l)]
  ## join "N" / "lat" / "lon" table rows split over three lines
  out <- character(); i <- 1
  isn <- function(x) grepl("^[0-9]{1,3}$", x)
  islat <- function(x) grepl("°.*(south|north)\\s*$", x, ignore.case = TRUE)
  islon <- function(x) grepl("°.*(east|west)\\s*$", x, ignore.case = TRUE)
  while (i <= length(l)) {
    if (i + 2 <= length(l) && isn(l[i]) && islat(l[i + 1]) && islon(l[i + 2])) {
      out <- c(out, paste(l[i], "   ", l[i + 1], "   ", l[i + 2])); i <- i + 3
    } else { out <- c(out, l[i]); i <- i + 1 }
  }
  f <- file.path("closures/text", paste0(id, ".txt"))
  writeLines(c("<<<PAGE 1>>>", paste0("SOURCE: ", url), out), f, useBytes = TRUE)
  cat(sprintf("%s: %d lines -> %s\n", id, length(out), f))
}
