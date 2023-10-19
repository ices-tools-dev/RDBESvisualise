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
#' @param output_type (Optional) Determines whether the output is tabular or a graphical plot. Possible values are "table" or "plot". Default is "plot". 
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
    output_type = NA
    ) {
    ##################################################
    ## Data preparation
    ##################################################
    ### Here we prepare the data in order to plot them. 
    ## P1: Prepare the data in case the variable of interest is related to landing. 
    # In this case we need to extract the CL table from the RDBES object. 
    if(contrastVar == "CLoffWeight", "CLsciWeight") {

    }

    ## P2: Prepare the data in case the variable of interest is related to effort. 
    # In this case we need to extract the CE table from the RDBES object. 
    if(contrastVar == "CEnumFracTrips", "CEnumDomTrip")

    
    ##################################################
    ## Data plotting.
    ##################################################
    ### Here we plot the data conditionally on type and resolution.     
}

