## =============================================================================
## Closures review, step 22: where each closure's boundary is written in the
## authorised PDF. Extracts the PDF text page by page (closures/text_pdf/),
## splits it into schedules as the parser does, and records the PDF page
## range of each schedule (and of each named area within it).
## Instruments without schedules: the pages carrying their coordinates.
## Output: closures/output/boundary_pages.csv
##   key, schedule, schedule_title, area, pdf_page_from, pdf_page_to, pdf_pages, file, url
## Page numbers are PDF pages (page 1 = cover), not the printed page numbers.
## Written 17 October 2026 with the assistance of Claude (Anthropic).
## =============================================================================
source("closures/R/fun_closures.R")
pdfs <- read.csv("closures/output/legislation_pdfs.csv", stringsAsFactors = FALSE)
dir.create("closures/text_pdf", showWarnings = FALSE)
res <- list()
for (i in which(!is.na(pdfs$pages))) {
  k <- pdfs$register_id[i]
  tx <- pdftools::pdf_text(pdfs$file[i])
  writeLines(unlist(lapply(seq_along(tx), function(p) c(sprintf("<<<PAGE %d>>>", p), tx[p]))),
             file.path("closures/text_pdf", paste0(k, ".txt")), useBytes = TRUE)
  rt <- read_instrument(k, "closures/text_pdf")
  ## PDF layout: centred headings with blank lines between "SCHEDULE N" and its title
  rt <- gsub("(^|\n)[ \t]+", "\\1", rt); rt <- gsub("\n(\\s*\n)+", "\n", rt)
  sch <- tryCatch(split_schedules(rt), error = function(e) list())
  if (length(sch)) {
    for (s in sch) {
      ## area names inside the schedule: "The area of the X closure", "X Closure" lines
      txt <- strsplit(s$text, "\n")[[1]]
      pg_of <- function(pat) {
        hit <- which(vapply(seq_along(rt), function(p) grepl(pat, gsub("\\s+", " ", rt[p]), fixed = TRUE), TRUE))
        hit <- hit[hit >= s$page & hit <= s$page_end]; if (length(hit)) hit[1] else NA
      }
      res[[length(res) + 1]] <- data.frame(key = k, schedule = s$number, schedule_title = s$title, area = NA,
        pdf_page_from = s$page, pdf_page_to = s$page_end)
    }
  } else {
    hits <- which(vapply(rt, function(p) nrow(find_coords(p)) >= 2, TRUE))
    if (length(hits)) res[[length(res) + 1]] <- data.frame(key = k, schedule = 0, schedule_title = "(no schedules)", area = NA,
        pdf_page_from = min(hits), pdf_page_to = max(hits))
  }
}
res <- do.call(rbind, res)
res$pdf_pages <- ifelse(res$pdf_page_from == res$pdf_page_to, res$pdf_page_from, paste0(res$pdf_page_from, "-", res$pdf_page_to))
res$file <- pdfs$file[match(res$key, pdfs$register_id)]
res$url  <- pdfs$url[match(res$key, pdfs$register_id)]
write.csv(res, "closures/output/boundary_pages.csv", row.names = FALSE, na = "")
cat(nrow(res), "schedule rows from", length(unique(res$key)), "PDFs\n")
print(setdiff(pdfs$register_id, res$key))
