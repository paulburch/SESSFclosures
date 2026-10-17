## =============================================================================
## Shared functions for the closures review (SESSF_zones/closures)
## Written 17 October 2026 with the assistance of Claude (Anthropic).
## =============================================================================

## read an extracted instrument (closures/text/<key>.txt) as one string per page
read_instrument <- function(key, dir = "closures/text") {
  l <- readLines(file.path(dir, paste0(key, ".txt")), encoding = "UTF-8", warn = FALSE)
  pg <- cumsum(grepl("^<<<PAGE [0-9]+>>>$", l))
  out <- tapply(l, pg, function(x) paste(x[-1], collapse = "\n"))
  out <- unname(out)
  ## normalise the punctuation the PDFs use
  out <- gsub("[   ]", " ", out)      # non-breaking / thin spaces (Register HTML)
  ## 2008-2010 HTML tables: the hemisphere letter in a cell of its own, "151°23'21"\nE"
  out <- gsub("([0-9][\"'”’]?)[ \t]*\n[ \t]*([NSEW])[ \t]*(?=\n|$)", "\\1 \\2", out, perl = TRUE)
  out <- gsub("[’‘′`]", "'", out)
  out <- gsub("[“”″]", "\"", out)
  out <- gsub("''", "\"", out)                       # 2005: two apostrophes as the seconds mark
  out <- gsub("–|—", "-", out)
  out <- gsub("[º˚]", "°", out)   # ordinal / ring above / Symbol-font degree (2005, 2007) -> degree
  ## 2005 style: "37 degrees 9 minutes South", "36 degrees 51.5 minutes 20 seconds South"
  out <- gsub("([0-9]{1,3})\\s*degrees?\\s+([0-9]{1,2}(?:\\.[0-9]+)?)\\s*minutes?(?:\\s+([0-9]{1,2}(?:\\.[0-9]+)?)\\s*seconds?)?",
              "\\1\u00b0 \\2' \\3\"", out, perl = TRUE)
  out <- gsub("\\s\"", " ", gsub("' \"", "'", out))
  ## 2013: an apostrophe as the decimal point, "37° 31' 40'8"" = 40.8 seconds; "8'1 nautical miles"
  out <- gsub("('\\s*[0-9]{1,2})'([0-9]+)\"", "\\1.\\2\"", out)
  out <- gsub("\\b([0-9]{1,2})'([0-9]) nautical", "\\1.\\2 nautical", out)
  ## 2005 Pages Island: decimal minutes labelled "seconds", or with no unit:
  ## "35 degrees 45.54 second South", "138 degrees 17.49 East"
  out <- gsub("([0-9]{1,3})\\s*degrees?\\s+([0-9]{1,2}\\.[0-9]+)\\s*(?:seconds?\\s+)?(?=(?i:south|north|east|west)\\b)",
              "\\1\u00b0 \\2' ", out, perl = TRUE)
  ## 2013 style: the letter o as a degree sign, "38o 18.374'"
  out <- gsub("([0-9]{1,3})o\\s*([0-9]{1,2}(?:\\.[0-9]+)?\\s*')", "\\1\u00b0 \\2", out, perl = TRUE)
  ## typos in the instruments: "129° 00'.08' east" -> "129° 00.08' east"
  out <- gsub("([0-9]{1,2})'\\.([0-9]+)'", "\\1.\\2'", out)
  out <- gsub("([0-9])\\.\\.([0-9])", "\\1.\\2", out)          # "34..90'" -> "34.90'"
  out <- gsub("([0-9]\")\\s*00\"", "\\1", out)
  out <- gsub("\\b([0-9]+(?:\\.[0-9]+)?)\\s*nm\\b", "\\1 nautical miles", out, perl = TRUE)   # 2007: "1nm off"
  ## 2005: whole degrees without a degree sign, "latitude 41 South", "longitude 140 east"
  out <- gsub("\\b(latitude|longitude)\\s+([0-9]{1,3})\\s+(?=(?i:south|north|east|west)\\b)", "\\1 \\2° ", out, perl = TRUE)                  # 2008: "35' 12" 00" east"
  ## hemisphere word left out after "latitude"/"longitude" (2008-2013 Bass Strait):
  ## "meridian of longitude 147° 28' 00";" -> add east; latitude -> add south
  cre <- "\\s*([0-9]{1,3}\\s*°\\s*[0-9]{1,2}(?:\\.[0-9]+)?\\s*'(?:\\s*[0-9]{1,2}(?:\\.[0-9]+)?\\s*\")?)(?!\\s*(?i:east|west|south|north|E\\b|S\\b|[0-9]))"
  out <- gsub(paste0("(longitude(?:\\s+of)?)", cre), "\\1 \\2 east", out, perl = TRUE)
  out <- gsub(paste0("(latitude(?:\\s+of)?|parallel)", cre), "\\1 \\2 south", out, perl = TRUE)   # 2009 "latitude of 35° 50' 29.65""
  ## documented corrections of drafting the parser cannot read (closures/data/text_fixes.csv)
  fx <- "closures/data/text_fixes.csv"
  if (file.exists(fx)) {
    fx <- read.csv(fx, stringsAsFactors = FALSE, encoding = "UTF-8"); fx <- fx[fx$key == key, ]
    for (j in seq_len(nrow(fx))) {
      hit <- grepl(fx$pattern[j], out, perl = TRUE)
      if (!any(hit)) warning("text fix ", j, " for ", key, " did not match")
      out <- gsub(fx$pattern[j], gsub("\\\\n", "\n", fx$replacement[j]), out, perl = TRUE)
    }
  }
  out
}

## strip page headers/footers so tables run on across pages
clean_page <- function(p) {
  l <- strsplit(p, "\n")[[1]]
  drop <- grepl("Authorised Version F20|Compilation No\\.|Compilation date|^\\s*Fisheries Management \\(Southern|^\\s*Small Pelagic Fishery Closures\\) Direction|^\\s*Southern and Eastern Scalefish and Shark Fishery and Small Pelagic Fishery\\s*[0-9]*\\s*$|^\\s*\\(Closures\\) Direction [0-9]{4}\\s*$|^\\s*[0-9]{1,3}\\s*$", l)
  paste(l[!drop], collapse = "\n")
}

## split the body into schedules: headings "Schedule N-Title" at line start
split_schedules <- function(tx) {
  pages <- vapply(tx, clean_page, "")
  l <- unlist(lapply(seq_along(pages), function(i) {
    x <- strsplit(pages[i], "\n")[[1]]; paste0(i, "\t", x) }))
  pg <- as.integer(sub("\t.*", "", l)); l <- sub("^[0-9]+\t", "", l)
  ## schedule headings in the body (not the contents list: those end in a page number)
  ## older instruments put "SCHEDULE 12" alone on a line, the title below it
  bare <- grep("^\\s*SCHEDULE\\s+[0-9]+\\s*$", l, ignore.case = TRUE)
  for (i in bare) if (i < length(l)) l[i] <- paste(sub("^\\s*SCHEDULE", "Schedule", l[i], ignore.case = TRUE), "-", trimws(l[i + 1]))
  ## a title may start with a digit only after a dash (2005 "Schedule 4 - 41 South");
  ## not cross-references in the clauses ("Schedule 32.", "Schedule 38 in a season")
  h <- grep("^\\s*Schedule\\s+[0-9]+(?![0-9])\\s*(-\\s*)?[A-Z]|^\\s*Schedule\\s+[0-9]+\\s*-\\s*[0-9]", l, perl = TRUE)
  h <- h[!grepl("\\.{3,}|\\s[0-9]{1,3}\\s*$", l[h])]
  ## a body heading is followed within a few lines by the area description
  ## the body starts at the first heading followed by an area description;
  ## after that every new schedule number, in increasing order, is a heading
  nxt <- vapply(h, function(i) paste(l[(i + 1):min(length(l), i + 4)], collapse = " "), "")
  first <- h[grepl("[Tt]he areas? |area of|areas of|part of the AFZ", nxt)][1]
  if (!length(h)) return(list())
  if (is.na(first)) first <- h[1]
  h <- h[h >= first]
  num <- as.integer(sub("^\\s*Schedule\\s+([0-9]+).*", "\\1", l[h]))
  keep <- !duplicated(num) & num >= cummax(c(0, head(num, -1)))
  h <- h[keep]; num <- num[keep]
  ## the last schedule ends where the endnotes begin
  en <- grep("^\\s*Endnotes?\\b|^\\s*Endnote 1", l)
  last <- if (length(en) && any(en > max(h))) min(en[en > max(h)]) - 1 else length(l)
  ends <- c(h[-1] - 1, last)
  lapply(seq_along(h), function(i) list(
    number = num[i], title = trimws(sub("^\\s*Schedule\\s+[0-9]+\\s*-?\\s*", "", l[h[i]])),
    page = pg[h[i]], page_end = pg[ends[i]], text = paste(l[h[i]:ends[i]], collapse = "\n")))
}

## all latitude / longitude mentions, in text order
## forms: 33° 34.90' south | 33°34'54" S | 33 34 54 S (degrees minutes seconds)
find_coords <- function(b) {
  re <- paste0("([0-9]{1,3})\\s*°\\s*(?:([0-9]{1,2}(?:\\.[0-9]+)?)\\s*'?)?",
               "(?:\\s*([0-9]{1,2}(?:\\.[0-9]+)?)\\s*[\"']?)?\\s*",   # 2009: seconds without the \" mark; 2013: with '
               "((?i:south|north|east|west)|S|E)\\b")
  m <- gregexpr(re, b, perl = TRUE)[[1]]
  if (m[1] < 0) return(data.frame(pos = integer(), type = character(), value = numeric(), text = character()))
  s <- regmatches(b, list(m))[[1]]
  cap <- regmatches(s, regexec(re, s, perl = TRUE))
  d <- as.numeric(vapply(cap, `[`, "", 2)); mi <- suppressWarnings(as.numeric(vapply(cap, `[`, "", 3)))
  mi[is.na(mi)] <- 0
  se <- suppressWarnings(as.numeric(vapply(cap, `[`, "", 4))); se[is.na(se)] <- 0
  hemi <- tolower(vapply(cap, `[`, "", 5))
  type <- ifelse(hemi %in% c("south", "north", "s"), "lat", "lon")
  val <- d + mi / 60 + se / 3600
  val[hemi %in% c("south", "s")] <- -val[hemi %in% c("south", "s")]
  ## swapped hemisphere words (2008-2013 Sch 28 "34° 56' east, longitude 119° 30' south"):
  ## in SESSF waters a latitude is < 90 and a longitude > 90
  sw <- type == "lat" & abs(val) > 90; type[sw] <- "lon"; val[sw] <- abs(val[sw])
  sw <- type == "lon" & val < 90; type[sw] <- "lat"; val[sw] <- -abs(val[sw])
  data.frame(pos = as.integer(m), type = type, value = val, text = s, stringsAsFactors = FALSE)
}
