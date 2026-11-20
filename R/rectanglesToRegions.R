#' Convert codes between ICES spatial levels
#'
#' Converts ICES statistical rectangles and ICES area codes to a coarser
#' spatial level. The level of each input code is detected from its format
#' unless `from` is given, so one vector can mix levels.
#'
#' Each rectangle is assigned to a single ICES area, taken from column
#' `Area_27` of [icesRectSF] (as provided by ICES). Rectangles spanning
#' several areas list all of them in `AreasList` of [icesStatRecLookup].
#'
#' @param x Character vector of codes, e.g. "41F6" (rectangle) or "27.4.b"
#'   (ICES area; area codes include the "27." prefix).
#' @param to Level to convert to: one of "Region", "SubArea", "Division",
#'   "SubDivision", "Area", "FAOArea".
#' @param from Level of `x`: one of "StatRec", "Area", "SubDivision",
#'   "Division", "SubArea", "FAOArea". If `NULL` (default), detected for each
#'   code: rectangles by their pattern, area codes by their number of parts.
#' @return Character vector with the same length and order as `x`. Codes
#'   already at the `to` level are returned unchanged. Unknown codes, and areas
#'   with no code at the `to` level (e.g. division 27.4.a has no subdivision),
#'   give NA.
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Human review: mrska
#' @keywords internal
#' @md
#'
#' @examples
#' \dontrun{
#' rectanglesToRegions(c("41F6", "39G5"), "Region")
#' rectanglesToRegions(c("41F6", "27.7.e", "27.3.d"), "Region")  # mixed levels
#'
#' # Full area names from the ICES vocabulary
#' div <- rectanglesToRegions(c("37G6", "41F6"), "Division")
#' areaNames <- icesVocab::getCodeList("ICES_Area")
#' areaNames$Description[match(div, areaNames$Key)]
#' }
rectanglesToRegions <- function(x,
                                to = c("Region", "SubArea", "Division",
                                       "SubDivision", "Area", "FAOArea"),
                                from = NULL) {
  to <- match.arg(to)
  x <- as.character(x)
  levels <- c("StatRec", "Area", "SubDivision", "Division", "SubArea", "FAOArea")

  if (is.null(from)) {
    from <- detectIcesLevel(x)
  } else {
    from <- rep(match.arg(from, levels), length(x))
  }

  out <- rep(NA_character_, length(x))
  for (lvl in unique(from[!is.na(from)])) {
    idx <- which(from == lvl)
    out[idx] <- convertIcesLevel(x[idx], lvl, to)
  }
  out
}

# Level of each code: rectangle by pattern, ICES areas by number of parts
detectIcesLevel <- function(x) {
  nParts <- lengths(strsplit(x, ".", fixed = TRUE))
  level <- c("FAOArea", "SubArea", "Division", "SubDivision", "Area")[pmin(nParts, 5)]
  level[grepl("^[0-9]{2}[A-Z][0-9]$", x)] <- "StatRec"
  level[is.na(x) | x == ""] <- NA_character_
  level
}

# Conversion for codes all at one level. With from == to, checks that the
# codes exist and returns them unchanged.
convertIcesLevel <- function(x, from, to) {
  lookup <- RDBESvisualise::icesStatRecLookup
  pairs <- unique(lookup[!is.na(lookup[[from]]), unique(c(from, to)), drop = FALSE])

  # Each input code must map to one result. It does not when `to` is finer
  # than `from` (e.g. Division -> Area) or a level spans regions (SubArea 27.7)
  ambiguous <- intersect(unique(pairs[[from]][duplicated(pairs[[from]])]), x)
  if (length(ambiguous) > 0) {
    stop("Cannot convert from '", from, "' to '", to, "': these codes map ",
         "to more than one ", to, ": ", listCodes(ambiguous))
  }

  unknown <- setdiff(unique(x[!is.na(x)]), pairs[[from]])
  if (length(unknown) > 0) {
    detected <- detectIcesLevel(unknown)
    hint <- ifelse(!is.na(detected) & detected != from,
                   paste0(unknown, " (looks like ", detected, ")"),
                   unknown)
    warning("Codes not found at level '", from, "' (returned as NA): ",
            listCodes(hint))
  }

  pairs[[to]][match(x, pairs[[from]])]
}

# First few codes for an error message
listCodes <- function(codes, n = 5) {
  out <- paste(codes[seq_len(min(n, length(codes)))], collapse = ", ")
  if (length(codes) > n) paste0(out, ", ...") else out
}
