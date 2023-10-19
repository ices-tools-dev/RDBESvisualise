#' Provides tabular or graphical outputs which compare sampling versus 
#' landing coverage across the level of a factor variable specified by the user. 
#'
#' @param RDBESobj RDBES data to be plotted (as an RDBESDataObject)
#' @param var The unit of the variable of interest. Possible values are "WeightSampled", "NumberSamples", "IndividualsSampled". 
#' @param contrastVar A variable to be selected by the user.  landing or effort selected by the user. 
#' @param by (Optional) One (or more) factor(s) to split the data. 
#' @param type Type of comparison to be represented by the plot. Possible values are "Spatial", "Time", "Fleet", "Species". 
#' @param resolution The resolution at which the type is required. Possible values: "ICES Subdivision", "ICES Rectangle", "Year", "Semester", "Quarter", "Month", "ScaleFishery", "SpeciesList", "SpeciesGroupList".
#' @param year (Optional) Year to be assessed. Default is all years contained in RDBESobj data. 
#' @param quarter (Optional) Quarter to be assessed - possible choices 1,2,3 or 4
#' @param vesselFlag (Optional) Registered Country of Vessel - e.g "IE", "ES" or "FR"
#' @param output_type (Optional) Determines whether the output is tabular or a graphical plot. Possible values are "table" or "plot". Default is "plot". 
#' @param verbose (Optional) Set to TRUE to print more information. Default is FALSE
#'
#' @return table or plot depending on user selection. 
#' @export
#'
#' @examples
#' \dontrun{
#'
#' 
#' }

coverageSpatial <- function(
    RDBESobj,
    var = c(
        "CLoffWeight",
        "CLsciWeight",
        "CEnumFracTrips",
        "CEnumDomTrip", 
        "SAsampWtLive",
        "SAnumSamp",
        "SAsampWtMes"
    ),
    contrastVar,
    by = NA,
    type,
    resolution,
    year = NA,
    quarter = NA, 
    vesselFlag = NA, 
    output_type = NA, 
    verbose = NA
    ) {

    ##################################################
    ### Data preparation
    ##################################################
    ### S1: Here we prepare the data in order to plot them. 
    ## P1: Prepare the data in case the variable of interest is related to landing. 
    if(contrastVar == "CLoffWeight", "CLsciWeight", "CLtotalOfficialLandingsValue") {
    ## P1.1: Extract the CL table from the RDBES object. 
    # Through preprocessLandingsDataForCoverage() extract CL from RDBESobject and merge with wormsSpecies.rda to obtain latin name from Aphia codes.
    contrastDf <- preprocessLandingsDataForCoverage( 
        RDBESobj,  
        verbose = verbose 
        )

    ## P1.2: Additional subsetting of data [year, quarter, vesselFlag currently available]
    # Through filterLandingsDataForCoverage() we filter the data based on the function's input parameters. 
    contrastDf <- filterLandingsDataForCoverage(
        landingsData = contrastDf, 
        yearToFilter = year,
        quarterToFilter = quarter,
        vesselFlag = vesselFlag,
        verbose = verbose
        )

    }

    ## P2: Prepare the data in case the variable of interest is related to effort. 
    # In this case we need to extract the CE table from the RDBES object. 
    if(contrastVar == "CEnumFracTrips", "CEnumDomTrip") {
    print("work in progress")
    }

    
    ##################################################
    ## Data plotting.
    ##################################################
    ### Here we plot the data conditionally on type and resolution.     
}

