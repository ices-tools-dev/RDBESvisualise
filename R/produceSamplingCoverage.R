#' Compare the sampling with the landings or effort
#'
#' Sums a sampling variable and a landings or effort variable (the contrast
#' variable) at the chosen resolution and compares their relative values. Large
#' landings or effort with little sampling show gaps in the sampling coverage.
#'
#' Records with a missing value of `var` or `contrastVar` are excluded from
#' the sums, with a warning. Levels of `resolution` that occur only in the
#' sampling or only in the contrast data get a sum of 0 for the other data.
#' Records without a rectangle or area code are kept in the table but cannot be
#' drawn on the maps.
#'
#' @param RDBESobj An RDBESDataObject with sample data from a single hierarchy
#' and CL (or CE) data
#' @param var The sampling variable: "SAsampWtLive", "SAnumSamp",
#' "SAsampWtMes", or "SAamountSamp" for the number of samples
#' @param contrastVar The landings (e.g. "CLoffWeight") or effort (e.g.
#' "CEnumFracTrips") variable to compare with
#' @param resolution The level at which the sampling and the landings or effort
#' are compared: "ICES Rectangle", "ICES Area", "Year", "Semester", "Quarter",
#' "Month", "Gear", "Vessel length" or "Species" (landings only)
#' @param by (Optional) One or more additional levels from `resolution` used to
#' split the data, e.g. "Quarter". The relative values are calculated within
#' each level of `by`. Default is NULL.
#' @param year (Optional) Year to be assessed e.g 2021
#' @param quarter (Optional) Quarter to be assessed - possible choices 1,2,3 or 4
#' @param vesselFlag (Optional) Registered Country of Vessel - e.g "IE", "ES" or
#' "FR"
#' @param catchCat Sampling catch category - "Lan", "Dis" or "Catch"
#' @param showSamples (Optional) Show the sampling as circles on the maps.
#' Default is TRUE.
#' @param output_type (Optional) "plot" (default) or "table"
#' @param verbose (Optional) Set to TRUE to print more information. Default is
#' FALSE
#'
#' @return For `output_type = "table"` a data frame with one row per level of
#' `resolution` (and `by`) and the columns `sampling` and `contrast` (the sums
#' of `var` and `contrastVar`) and `samplingShare` and `contrastShare` (their
#' relative values). For `output_type = "plot"` a ggplot2 plot: a map for
#' "ICES Rectangle" and "ICES Area", otherwise a bar plot of the relative
#' values.
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: (pending)
#' - Notes/scope: Rewritten to a working proof of concept (#11). The `type`
#'   parameter of the earlier draft is implied by `resolution`. Sums checked
#'   against manual calculations with the H1 test data in
#'   `tests/testthat/h1_v_20250211`.
#' @export
#'
#' @examples
#' \dontrun{
#' myH1RawObject <- RDBEScore::createRDBESDataObject(
#'   input = "tests/testthat/h1_v_20250211"
#' )
#' produceSamplingCoverage(myH1RawObject,
#'   var = "SAnumSamp",
#'   contrastVar = "CLoffWeight",
#'   resolution = "Quarter",
#'   catchCat = "Lan"
#' )
#' }
produceSamplingCoverage <- function(RDBESobj,
                                    var = c("SAsampWtLive", "SAnumSamp",
                                            "SAsampWtMes", "SAamountSamp"),
                                    contrastVar = "CLoffWeight",
                                    resolution = c("ICES Rectangle",
                                                   "ICES Area", "Year",
                                                   "Semester", "Quarter",
                                                   "Month", "Gear",
                                                   "Vessel length",
                                                   "Species"),
                                    by = NULL,
                                    year = NA,
                                    quarter = NA,
                                    vesselFlag = NA,
                                    catchCat = c("Lan", "Dis", "Catch"),
                                    showSamples = TRUE,
                                    output_type = c("plot", "table"),
                                    verbose = FALSE) {

  var <- match.arg(var)
  resolution <- match.arg(resolution)
  output_type <- match.arg(output_type)

  # Columns of each resolution in the sample, landings and effort data
  resolutions <- data.frame(
    resolution = c("ICES Rectangle", "ICES Area", "Year", "Semester",
                   "Quarter", "Month", "Gear", "Vessel length", "Species"),
    SA = c("SAstatRect", "SAarea", "year", "semester", "quarter", "month",
           "SAgear", "VDlenCat", "species"),
    CL = c("CLstatRect", "CLarea", "year", "semester", "quarter", "month",
           "CLGear", "CLvesLenCat", "species"),
    CE = c("CEstatRect", "CEarea", "year", "semester", "quarter", "month",
           "CEGear", "CEvesLenCat", NA)
  )
  groups <- c(resolution, by)
  contrastTable <- substr(contrastVar, 1, 2)

  if (length(catchCat) != 1 ||
      !catchCat %in% RDBESvisualise::allowedCatchCat) {
    stop("You must provide one catchCat: ",
         paste(RDBESvisualise::allowedCatchCat, collapse = ", "))
  }
  if (!contrastVar %in% c(RDBESvisualise::allowedLandingsVariable,
                          RDBESvisualise::allowedEffortVariable)) {
    stop("Invalid contrastVar value: ", contrastVar)
  }
  if (!all(by %in% resolutions$resolution) || resolution %in% by) {
    stop("by must be one or more of: ",
         paste(setdiff(resolutions$resolution, resolution), collapse = ", "))
  }
  if (anyNA(resolutions[match(groups, resolutions$resolution), contrastTable])) {
    stop("Species is not available for effort data")
  }

  RDBEScore::validateRDBESDataObject(RDBESobj, verbose = verbose)
  coverageData <- prepareCoverageData(dataToPlot = RDBESobj,
                                      year = year,
                                      quarter = quarter,
                                      vesselFlag = vesselFlag,
                                      catchCat = catchCat,
                                      landingsVariable = contrastVar,
                                      effortVariable = contrastVar,
                                      samplingVariable = var,
                                      includeLandings = contrastTable == "CL",
                                      includeEffort = contrastTable == "CE",
                                      includeSamples = TRUE,
                                      verbose = verbose)
  sa <- as.data.frame(coverageData$samples)
  ct <- as.data.frame(
    if (contrastTable == "CL") coverageData$landings else coverageData$effort
  )

  # Derived columns: semester, and species name (or code if it has no name)
  sa$semester <- ceiling(sa$quarter / 2)
  ct$semester <- ceiling(ct$quarter / 2)
  sa$species <- ifelse(is.na(sa$SAspeciesName), sa$SAspeCode, sa$SAspeciesName)
  if (contrastTable == "CL") {
    ct$species <- ifelse(is.na(ct$CLspeciesName), ct$CLspecCode,
                         ct$CLspeciesName)
  }

  samplingValues <- if (var == "SAamountSamp") rep(1, nrow(sa)) else sa[[var]]
  sampling <- sumByGroups(sa, resolutions$SA[match(groups, resolutions$resolution)],
                          groups, samplingValues, "sampling")
  contrast <- sumByGroups(ct,
                          resolutions[match(groups, resolutions$resolution), contrastTable],
                          groups, ct[[contrastVar]], "contrast")

  # A level without records has a sum of 0
  coverage <- merge(sampling, contrast, by = groups, all = TRUE)
  coverage$sampling[is.na(coverage$sampling)] <- 0
  coverage$contrast[is.na(coverage$contrast)] <- 0

  # Relative values within each level of by
  byLevels <- if (is.null(by)) {
    rep(1, nrow(coverage))
  } else {
    do.call(paste, coverage[by])
  }
  coverage$samplingShare <- coverage$sampling /
    stats::ave(coverage$sampling, byLevels, FUN = sum)
  coverage$contrastShare <- coverage$contrast /
    stats::ave(coverage$contrast, byLevels, FUN = sum)

  if (output_type == "table") {
    return(coverage)
  }

  contrastLabel <- if (contrastTable == "CL") "Landings" else "Effort"
  myTitle <- paste0("Sampling catchCat=", catchCat, " (", var, ") vs ",
                    contrastLabel, " (", contrastVar, ")")
  mySubtitle <- paste0("Vessel Flag: ", ifelse(is.na(vesselFlag), "All",
                                               vesselFlag))

  if (resolution %in% c("ICES Rectangle", "ICES Area")) {
    p <- mapSamplingCoverage(coverage, resolution, var, contrastVar,
                             showSamples)
  } else {
    shares <- rbind(
      data.frame(coverage[groups], data = paste0("Sampling (", var, ")"),
                 share = coverage$samplingShare, check.names = FALSE),
      data.frame(coverage[groups],
                 data = paste0(contrastLabel, " (", contrastVar, ")"),
                 share = coverage$contrastShare, check.names = FALSE)
    )
    p <- ggplot2::ggplot(shares, ggplot2::aes(
      x = factor(.data[[resolution]]), y = .data$share, fill = .data$data
    )) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_fill_manual(values = getPaletteForPlotting(2)) +
      ggplot2::labs(x = resolution, y = "Relative value", fill = NULL) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")
  }
  if (!is.null(by)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(by)))
  }
  p + ggplot2::labs(title = myTitle, subtitle = mySubtitle)
}

#' Internal function to sum a variable by groups, keeping missing group values
#' as their own group
#'
#' @param data A data frame
#' @param columns The columns of `data` to group by
#' @param groupNames The names of the group columns in the output
#' @param values The values to sum
#' @param valueName The name of the sum column in the output
#'
#' @return A data frame with the group columns and the sums
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: (pending)
#' @keywords internal
sumByGroups <- function(data, columns, groupNames, values, valueName) {
  groupData <- lapply(data[columns], function(x) {
    if (is.factor(x)) as.character(x) else x
  })
  names(groupData) <- groupNames
  groupData <- as.data.frame(groupData, check.names = FALSE)
  groupData$value <- values
  sums <- dplyr::summarise(
    dplyr::group_by(groupData, dplyr::across(dplyr::all_of(groupNames))),
    value = sum(.data$value, na.rm = TRUE),
    .groups = "drop"
  )
  sums <- as.data.frame(sums, check.names = FALSE)
  names(sums)[names(sums) == "value"] <- valueName
  sums
}

#' Internal function to map the sampling coverage by ICES rectangle or area
#'
#' @param coverage The coverage table from [produceSamplingCoverage()]
#' @param resolution "ICES Rectangle" or "ICES Area"
#' @param var Name of the sampling variable, for the legend
#' @param contrastVar Name of the contrast variable, for the legend
#' @param showSamples Show the sampling as circles?
#'
#' @return A ggplot2 plot
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: (pending)
#' @keywords internal
mapSamplingCoverage <- function(coverage, resolution, var, contrastVar,
                                showSamples) {
  if (resolution == "ICES Rectangle") {
    layer <- RDBESvisualise::icesRectSF[, "ICESNAME"]
  } else {
    # icesAreaSF is in EPSG:3857, the other spatial data in EPSG:4326
    layer <- sf::st_transform(RDBESvisualise::icesAreaSF[, "Area_Full"], 4326)
  }
  names(layer)[1] <- resolution
  layer <- layer[!duplicated(layer[[resolution]]), ]
  mapData <- merge(layer, coverage, by = resolution)
  if (nrow(mapData) == 0) {
    stop("None of the ", resolution, " codes in the data could be mapped")
  }

  plotExtent <- sf::st_bbox(mapData)
  p <- ggplot2::ggplot(mapData) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$contrast), color = "white") +
    ggplot2::geom_sf(data = cropToExtent(RDBESvisualise::shoreline, plotExtent),
                     fill = "gray80", color = "gray50") +
    ggplot2::scale_fill_viridis_c(name = contrastVar, labels = formatNumber)
  if (showSamples) {
    points <- suppressWarnings(sf::st_point_on_surface(mapData))
    p <- p +
      ggplot2::geom_sf(data = points[points$sampling > 0, ],
                       ggplot2::aes(size = .data$sampling), shape = 1) +
      ggplot2::scale_size(name = var, labels = formatNumber)
  }
  p +
    ggplot2::coord_sf(xlim = plotExtent[c("xmin", "xmax")],
                      ylim = plotExtent[c("ymin", "ymax")]) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
