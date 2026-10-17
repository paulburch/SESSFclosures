#' Standard gear classes used to match gear to closures
#'
#' Maps free-text gear names (logbook gear codes or descriptions) to the
#' classes the closures directions use.
#'
#' @param gear Character vector of gear names or codes, e.g. `"TW"`,
#'   `"otter trawl"`, `"DS"`, `"GN"`, `"auto longline"`, `"dropline"`.
#' @return Character vector: `"otter trawl"`, `"Danish seine"`,
#'   `"midwater trawl"`, `"gillnet"`, `"automatic longline"`, `"hook"`,
#'   `"trap"` or `"other"`.
#' @examples
#' gear_class(c("TW", "Danish seine", "GN", "AL", "Dropline", "trap"))
#' @export
gear_class <- function(gear) {
  g <- tolower(trimws(gear))
  out <- rep("other", length(g))
  out[grepl("mid ?water|^tmo$|^mwt", g)] <- "midwater trawl"
  out[grepl("danish|seine|^ds$", g)] <- "Danish seine"
  out[grepl("otter|trawl|^tw$|^tdo|^ott|^tb", g) & out == "other"] <- "otter trawl"
  out[grepl("gill|mesh net|^gn$|^gns$", g)] <- "gillnet"
  out[grepl("auto|^al$|^lla$", g)] <- "automatic longline"
  out[grepl("hook|line|^dl$|^ll$|^hl$|^tl$|^lln$", g) & out == "other"] <- "hook"
  out[grepl("trap|pot|^fpo$", g)] <- "trap"
  out
}

#' Does a closure's rule apply to a gear?
#'
#' Compares the gear a closure closes (`gear_closed` in [closures]) with a
#' fishing gear.
#'
#' Rules: "all methods" closes every gear; "trawl" closes otter trawl, Danish
#' seine and midwater trawl (the Small Pelagic Fishery midwater exemptions in
#' the rule text are not modelled); "demersal otter trawl" closes otter trawl
#' only; "hook" closes all hook gear including automatic longline;
#' "automatic longline" closes automatic longline only; orange roughy
#' management areas close nothing.
#'
#' @param gear_closed `gear_closed` values from [closures].
#' @param gear Gear (recycled), any form accepted by [gear_class()].
#' @return Logical vector (`NA` where `gear_closed` is missing).
#' @examples
#' gear_applies(c("trawl", "demersal otter trawl", "gillnet", "all methods"), "Danish seine")
#' @export
gear_applies <- function(gear_closed, gear) {
  n <- max(length(gear_closed), length(gear))
  gc <- tolower(rep_len(gear_closed, n)); g <- gear_class(rep_len(gear, n))
  one <- function(rule, g) {
    if (is.na(rule)) return(NA)
    if (grepl("^none", rule)) return(FALSE)
    parts <- trimws(unlist(strsplit(rule, "\\+|;")))
    any(vapply(parts, function(p) switch(p,
      "all methods" = TRUE,
      "trawl" = g %in% c("otter trawl", "Danish seine", "midwater trawl"),
      "demersal otter trawl" = g == "otter trawl",
      "danish seine" = g == "Danish seine",
      "gillnet" = g == "gillnet",
      "hook" = g %in% c("hook", "automatic longline"),
      "automatic longline" = g == "automatic longline",
      FALSE), TRUE))
  }
  mapply(one, gc, g, USE.NAMES = FALSE)
}
