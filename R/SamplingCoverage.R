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
#' @param showSamples (Optional) Determines whether the samples data should be displayed or not. Default is TRUE. 
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


###################################################################################
# 
# Note: This function is a generalization of different comparisons between sampling
# and landings data and between sampling and effort data. 
# The function code was in part built relying on or merging pre-existing functions, 
# including: 
# 
# - coverageSpatial.R: from FishNCo project. Author: unknown. 
# - preprocessLandingsDataForCoverage: from RDBES project. Author: unknown. 
# - filterLandingsDataForCoverage: from RDBES project. Author: unknown. 
# 
#
###################################################################################

SamplingCoverage <- function(
    RDBESobj,
    var = c(
        "SAsampWtLive",
        "SAnumSamp",
        "SAsampWtMes"
    ),
    contrastVar = c(
        "CLoffWeight",
        "CLsciWeight",
        "CLtotalOfficialLandingsValue", 
        "CEnumFracTrips",
        "CEnumDomTrip"
    ),
    by = NA,
    type = c(
        "Spatial",
        "Time", 
        "Fleet", 
        "Species"
    ),
    resolution = c(
        "ICES Subdivision",
        "ICES Rectangle", 
        "Year", 
        "Semester", 
        "Quarter", 
        "Month",
        "Scale of fishery",
        "Species unit",
        "Species group" 
    ),
    year = NA,
    quarter = NA, 
    vesselFlag = NA, 
    showSamples = T
    output_type = NA, 
    verbose = T
    ) {

    ##################################################
    ### Data preparation
    ##################################################
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
    
    ## P3: Define the data we have been given
    # Define set of directions we received from the user in general terms. 
    if(contrastVar %in% c("CEnumFracTrips", "CEnumDomTrip")) {

        contrast = paste("Effort")

    } else {

        if(contrastVar %in% c("CLoffWeight", "CLsciWeight")) {

            contrast = paste("Landing - Weight")

        } else {

            contrast = paste("Landing - Value")

        } 
    }

    # Print a summary message, if selected. 
    if(verbose == TRUE) {
        print(paste0(
            "Preparing the comparison of sampling data versus ", 
            contrast, 
            " data.")
            )
    } 
    
    if (is.na(vesselFlag)) {
        flagLabel <- "All"
    } else {
        flagLabel <- vesselFlag
    }

    ##################################################
    ## Data plotting.
    ##################################################
    ## P3: Plot in case a spatial comparison is required
    if(type == "Spatial"){

        ## P3.1 Plot in case a spatial comparison at the ICES Subdivision resolution is required
        if(resolution == "ICES Subdivision") {

        }
    
        ## P3.2 Plot in case a spatial comparison at the ICES Subdivision resolution is required
        if(resolution == "ICES Rectangle") {
            
            ## P3.2.1 Load shapefile of ICES Subdivision
            ices_rects <- RDBESvisualise::icesRectSF
            
            ## P3.2.2 
            

        }

    }
    if(type == "Time"){
        print("work in progress")
    }
    if(type == "Fleet"){
        print("work in progress")
    }
    if(type == "Species"){
        print("work in progress")
    }
      
}

