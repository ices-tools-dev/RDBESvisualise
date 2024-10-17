#'Function perprocessing the Sampling data for coverage. Function based on
#'the function with the same name in utils.R. This function extended of data
#'validation and add variables from Biological Variables table, Frequency
#'Measure Table and general information occure in DE and SD ex. SamplingCountry,
#'hierarchy.The function works only for RDBESDataObject of one hierarchy.
#'
#' @param RDBESDataObject An RDBESDataObject.
#' @param validate Set to TRUE if you want validation to be carried out. The
#' default if TRUE.
#' @param verbose (Optional) Set to TRUE if you want informative text on
#' validation and function steps printed out, or FALSE if you don't.
#' The default is FALSE.
#' @param strict (Optional) This function can validate its input data - should
#' the validation be strict? The default is TRUE.
#' @param bioVar set TRUE to add biological variable ex. Length, Weight, Age,
#' Sex, Maturity to the sample data. The default is FALSE.
#' @param generalVar set TRUE to add information about SamplingCountry, Hierarchy
#' and Sampling scheme (ex.Baltic SPF regional) (DE, SD table). The default is FALSE.
#'
#' @return Function returns table.
#'
#' @example
#' RDBESDataObject <- createRDBESDataObject(input = "data-raw/exampleData/H1_2024_10_14.zip")
#' SA<-preprocessSamplingDataForCoverage(RDBESDataObject)

preprocessSamplingDataForCoverage <- function(RDBESDataObject,
                                              validate = TRUE,
                                              verbose = FALSE,
                                              strict = TRUE,
                                              bioVar = FALSE,
                                              generalVar = FALSE) {
  require(RDBEScore)
  require(dplyr)

  if (validate) {
    validateRDBESDataObject(RDBESDataObject, verbose = verbose, strict = strict)
  }

  if (verbose) {
    print("Preparing sample data")
  }
#check hierarchy
  H <- unique(RDBESDataObject[["DE"]]$DEhierarchy)
  if (verbose) {
    print(paste0("Upper hierarchy: ", H))
  }

  if (H %in% c(1, 2, 3, 13)) {
    if (verbose) {
      print("Samples are distributed in time according to FO table `FOendDate` column")
    }
    # obtain the key to FO, in order to extract time information from there
    SA <- merge(
      RDBESDataObject[["SA"]],
      RDBEScore::createTableOfRDBESIds(RDBESDataObject) |> dplyr::select(SAid, FOid, BVid, FMid, DEid, SDid) |> dplyr::distinct()
    )
    SA <- merge(SA, RDBESDataObject[["FO"]] |> dplyr::select(FOid, FOendDate), by = "FOid")
    SA$month <- stringr::str_sub(SA$FOendDate, 6, 7)
    SA <- SA |> select(-FOid, -FOendDate)
  }
  if (H %in% c(4, 5, 6, 7)) {
    if (verbose) {
      print(
        "According to upper hierarchy samples are distributed in time according to OS table `OSsamDate` column"
      )
    }
    # obtain the key to FO, in order to extract time information from there
    SA <- merge(
      RDBESDataObject[["SA"]],
      RDBEScore::createTableOfRDBESIds(RDBESDataObject) |> dplyr::select(SAid, OSid, BVid, FMid, DEid, SDid) |> dplyr::distinct()
    )
    SA <- merge(SA, RDBESDataObject[["OS"]] |> dplyr::select(OSid, OSsamDate), by = "OSid")
    SA$month <- stringr::str_sub(SA$OSsamDate, 6, 7)
    SA <- SA |> select(-OSid, -OSsamDate)
  }
  if (H %in% c(8, 9, 11, 12)) {
    if (verbose) {
      print("For Hiererchy 8,9,11,12 will present only year")
    }
    SA <- merge(
      RDBESDataObject[["SA"]],
      RDBEScore::createTableOfRDBESIds(RDBESDataObject) |> dplyr::select(SAid, BVid, FMid, DEid, SDid) |> dplyr::distinct()
    )
  }

  #year from DE
  SA <- merge(SA,
              RDBESDataObject[["DE"]] |> dplyr::select(DEid, DEsampScheme, DEyear, DEhierarchy),
              by = "DEid")

  if (generalVar) {
    #add general information
    SD <- merge(SA, RDBESDataObject[["SD"]] |> dplyr::select(SDid, SDctry), by = "SDid")
  }
  if (bioVar) {
    #add biological variables and frequancy measures
    #lower hierarchy: A
    if (length(RDBESDataObject[["FM"]]) != 0 &&
        length(RDBESDataObject[["BV"]]) != 0) {
      FM <- SA |> dplyr::mutate(SAFMid = paste0(SAid, FMid)) |> # I add new variable to don't lose intomation about lower hierarchy D
        dplyr::left_join(
          RDBESDataObject[["FM"]] |>
            dplyr::mutate(SAFMid = paste0(SAid, FMid)) |>
            dplyr::select(SAFMid, FMclassMeas, FMnumAtUnit, FMtypeMeas,FMmethod),
          by = "SAFMid"
        )
      BVar <- FM |>
        dplyr::left_join(
          RDBESDataObject[["BV"]] |> dplyr::select(BVid, BVfishId, BVtypeMeas, BVvalueMeas, BVvalUnitScale,BVmethod),
          by = "BVid"
        )
      #SD information
      if (generalVar) {
        SA <- merge(SD |> select(BVid, SDctry), BVar, by = "BVid")
      }
    }
    if (length(RDBESDataObject[["FM"]]) != 0 &&
        length(RDBESDataObject[["BV"]]) == 0) {
      #lower hierarchy: B
      BVar <- SA |> dplyr::mutate(SAFMid = paste0(SAid, FMid)) |> # I add new variable to don't lose intomation about lower hierarchy D
        dplyr::left_join(
          RDBESDataObject[["FM"]] |>
            dplyr::mutate(SAFMid = paste0(SAid, FMid)) |>
            dplyr::select(SAFMid, FMclassMeas, FMnumAtUnit, FMtypeMeas, FMmethod),
          by = "SAFMid"
        )
      #SD information
      if (generalVar) {
        SA <- merge(SD |> dplyr::mutate(SAFMid = paste0(SAid, FMid)), BVar, by = "SAFMid")
      }
    }
    if (length(RDBESDataObject[["FM"]]) == 0 &&
        length(RDBESDataObject[["BV"]]) != 0) {
      #lower hierarchy: C
      BVar <- SA |> dplyr::mutate(SABVid = paste0(SAid, BVid)) |> # I add new variable to don't lose intomation about lower hierarchy
        dplyr::left_join(
          RDBESDataObject[["BV"]] |>
            dplyr::mutate(SABVid = paste0(SAid, BVid)) |>
            dplyr::select(
              SABVid,
              BVid,
              BVfishId,
              BVtypeMeas,
              BVvalueMeas,
              BVmethod,
              BVvalUnitScale
            ),
          by = "SABVid"
        )
      #SD information
      if (generalVar) {
        SA <- merge(SD |> dplyr::mutate(paste0(SAid, BVid)), BVar, by = "SABVid")
      }
    }
    if (length(RDBESDataObject[["FM"]]) == 0 &&
        length(RDBESDataObject[["BV"]]) == 0) {
      #lower hierarchy: D
      print(
        "No frequency measure and biological variables data.
            For RDBESObejct lower hierarchy: D."
      )
      if (generalVar) {
        SA <- SD
      }
    }
  }
  #remove unnecessary columns
  if (bioVar == FALSE && generalVar == FALSE) {
    SA <- SA |> dplyr::select(-BVid, -FMid, -DEid, -SDid, -DEsampScheme, -DEhierarchy) |> dplyr::distinct()
  }
  if (bioVar == FALSE && generalVar == TRUE) {
    SA <- SD |> dplyr::select(-DEid, -SDid, -BVid, -FMid) |> dplyr::distinct()
  }
  if (bioVar == TRUE && generalVar == FALSE) {
    SA <- BVar |> dplyr::select(-DEid, -SDid) |> dplyr::distinct()
  }
  # Split time information in the different components
  if (!(H %in% c(8, 9, 11, 12))){
  SA <- SA |>
    dplyr::mutate(
      year = DEyear,
      quarter = dplyr::case_when(
        month == "01" ~ 1,
        month == "02" ~ 1,
        month == "03" ~ 1,
        month == "04" ~ 2,
        month == "05" ~ 2,
        month == "06" ~ 2,
        month == "07" ~ 3,
        month == "08" ~ 3,
        month == "09" ~ 3,
        month == "10" ~ 4,
        month == "11" ~ 4,
        month == "12" ~ 4
      ),
      semester = dplyr::case_when(
        quarter %in% c(1, 2) ~ 1,
        quarter %in% c(3, 4) ~ 2)
    )
  }else{
    SA <- SA |>
      dplyr::mutate(
        year = DEyear,
        quarter = NA,
        semester = NA,
        month = NA
      )
  }
  SA
}
