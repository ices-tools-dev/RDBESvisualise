#'Function perprocessing the Sampling data for coverage. Function based on
#'the function with the same name in utils.R. This function extended of data
#'validation and add variables from Biological Variables table, Frequency
#'Measure Table and general information occure in DE and SD ex. flagCountry,
#'hierarchy.
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
#' @param generalVar set TRUE to add information about FlagCountry, Hierarchy
#' and Sampling scheme (ex.Baltic SPF regional) (DE table). The default is FALSE.
#'
#' @return Function returns RDBESEstObject.
#'
#' @example ADD EXAMPLE!!! prepare vignettes
#'
preprocessSamplingDataForCoverage <- function(RDBESDataObject, validate, verbose, strict,
                                              bioVar, generalVar) {
  if(validate){
    validateRDBESDataObject(RDBESDataObject,
                            verbose = verbose,
                            strict = strict
    )
  }

  if (verbose) {
    print("Preparing sample data")
  }

  if(bioVar==FALSE && generalVar==FALSE){
  # obtain the key to FO, in order to extract time information from there
  SA <- merge(
    RDBESDataObject[["SA"]],
    RDBEScore::createTableOfRDBESIds(RDBESDataObject) |> dplyr::select(SAid, FOid) |> dplyr::distinct()
  )

  # merge the SA with FO keys to the FO, we use FOendDate to extract time information

  if (verbose) {
    print("Samples are distributed in time according to FO table `FOendDate` column")
  }

  SA <- merge(
    SA,
    RDBESDataObject[["FO"]] |> dplyr::select(FOid, FOendDate),
    by = "FOid"
  )

  # Split time information in the different components
  SA <- SA |>
    dplyr::mutate(
      year = stringr::str_sub(FOendDate, 1,4),
      month = stringr::str_sub(FOendDate, 6,7),
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
        quarter %in% c(1,2) ~ 1,
        quarter %in% c(3,4) ~ 2
      )
    )
  SA
  }
}
