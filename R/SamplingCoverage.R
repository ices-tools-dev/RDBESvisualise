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
# This function is a generalization of different comparisons between sampling
# and landings data and between sampling and effort data. 
# The function code was in part built relying on and/or merging pre-existing 
# functions, including: 
# 
# - coverageSpatial.R: from FishNCo project. 
# - preprocessLandingsDataForCoverage: from RDBES project.
# - filterLandingsDataForCoverage: from RDBES project. 
#
# More info: 
#
# - FishNCo report, Annex 1, Section 3 "Biological Data Quality" https://datacollection.jrc.ec.europa.eu/documents/d/dcf/annex-01-deliverable-1-overview-of-the-state-of-play-data-gaps-and-needs-
# - Second Workshop on Estimation with the RDBES data model, page105, Annex "A3.5" https://doi.org/10.17895/ices.pub.7915
# 
# Inquiries: Eros Quesada SLU Aqua Sweden @RDBESvisualise
#
###################################################################################

SamplingCoverage <- function(
    RDBESobj,
    var = c(
        "SAsampWtLive",
        "SAnumSamp",
        # ! N. samples, 
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
    showSamples = T,
    output_type = NA, 
    verbose = T
    ) {

    ##################################################
    ### Data preparation
    ##################################################

    ## P1: Extract the sampling data from the RDBES object. 
    # Through preprocessSampleDataForCoverage() extract sampling data from RDBESobject. 
    #samplingDf_Rect <- preprocessSampleDataForCoverage( 
    #    )
        
    ## P1.2: Additional subsetting of data [year, quarter, vesselFlag currently available]
    # Through filterLandingsDataForCoverage() we filter the data based on the function's input parameters. 
    #samplingDf_Rect <- filterSamplingDataForCoverage(
    #)

    ## P1: Prepare the data in case the variable of interest is related to landing. 

    if(contrastVar %in% c("CLoffWeight", "CLsciWeight", "CLtotalOfficialLandingsValue")) {
        
        ## P1.2: Extract the CL table from the RDBES object. 
        # Through preprocessLandingsDataForCoverage() extract CL from RDBESobject and merge with wormsSpecies.rda to obtain latin name from Aphia codes.
        contrastDf <- preprocessLandingsDataForCoverage( 
            RDBESobj,  
            verbose = verbose 
            )

        ## P1.3: Additional subsetting of data [year, quarter, vesselFlag currently available]
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
    if(contrastVar %in% c("CEnumFracTrips", "CEnumDomTrip")) {
    print("work in progress")
    }
    
    ## P3: Define the data we have been given
    # Define set of directions we received from the user in general terms. 
    if(contrastVar %in% c("CEnumFracTrips", "CEnumDomTrip")) {

        contrast = paste("Effort")

    } else {

        if(contrastVar %in% c("CLoffWeight", "CLsciWeight")) {

            contrast = paste("Landing - Weight")

        } else if (contrastVar == "CLtotalOfficialLandingsValue") {

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

    ## P4: Prep. in case a spatial comparison is required
    if(type == "Spatial"){

        ## P4.1 Prep in case a spatial comparison at the ICES Subdivision resolution is required
        if(resolution == "ICES Subdivision") {

        }
    
        ## P4.2 Prep. in case a spatial comparison at the ICES Rectangles resolution is required
        if(resolution == "ICES Rectangle") {
            
            ## P4.2.1 Load shapefile of ICES Rectangles
            ices_rects <- RDBESvisualise::icesRectSF
            
            ## P4.2.2 Calculate the amount of samples per ICES Rectangle (and other vars, if any))
            if(all(is.na(by))) {
                    grp_vars = c("SA") 
                } else {
                    grp_vars = c("SArectangle", by) 
                }
            
            # Then, we aggregate the sampling data accordingly.     
            samplingDf_Rect <- samplingDf_Rect %>% 
                    group_by_at(grp_vars) %>%
                    dplyr::summarize(
                        varInt = sum(eval(as.name(var)), na.rm = T)
                ) 
            
            print(head(samplingDf_Rect))

            # Specify the name of the variable of interest
            #names(samplingDf_Rect)[names(samplingDf_Rect) == "varInt"] <- var

            ## P4.2.2 Merge the result with the spatial object
            # Merge the resulting dataframe with the shapefile relative to the ICES Rectangles
            contrastDf_Rect <- merge(ices_rects, contrastDf_Rect, by.x = "ICESNAME", by.y = "CLstatRect", all.y = T)

            if(grepl(contrast, "Landing")) # If the contrast variable is landing
            
            ## P4.2.3 Remove those records not having an associated rectangle 
            contrastDf_Rect <- contrastDf %>% 
                dplyr::filter(!(CLstatRect %in% c("-9") | is.na(CLstatRect)))
            
            ## P4.2.4 Aggregate the data by ICES Rectangle (and other vars, if any)
            # First we define the grouping variable as the Statistical Rectangle + variables spec. by user
            if(all(is.na(by))) {
                    grp_vars = c("CLstatRect") 
                } else {
                    grp_vars = c("CLstatRect", by) 
                }
            
            # Then, we aggregate the landing data accordingly.     
            contrastDf_Rect <- contrastDf_Rect %>% 
                    group_by_at(grp_vars) %>%
                    dplyr::summarize(
                        varInt = sum(eval(as.name(contrastVar)), na.rm = T)
                    ) 
            
            # Specify the name of the variable of interest
            #names(contrastDf_Rect)[names(contrastDf_Rect) == "varInt"] <- contrastVar

            ## P4.2.5 Merge the result with the spatial object
            # Merge the resulting dataframe with the shapefile relative to the ICES Rectangles
            contrastDf_Rect <- merge(ices_rects, contrastDf_Rect, by.x = "ICESNAME", by.y = "CLstatRect", all.y = T)
        }
    }

    ## P5: Prep. in case a temporal comparison is required
    if(type == "Time"){

        ## P5.1 Prep in case a temporal comparison at the year resolution is required
        if(resolution == "Year") {
            print("work in progress")
        }

        ## P5.2 Prep in case a temporal comparison at the semester resolution is required
        if(resolution == "Semester") {
            print("work in progress")
        }

        ## P5.3 Prep in case a temporal comparison at the quarter resolution is required
        if(resolution == "Quarter") {
            print("work in progress")
        }

        ## P5.4 Prep in case a temporal comparison at the month resolution is required
        if(resolution == "Month") {
            print("work in progress")
        }
    
    }
    
    ## P5: Prep. in case a fleet comparison is required
    if(type == "Fleet"){
        print("work in progress")
    }

    ## P6: Prep. in case a species comparison is required
    if(type == "Species"){

        ## P6.1 Prep in case a species comparison at the species unit resolution is required
        if(resolution == "Species unit") {
            print("work in progress")
        }

        ## P6.2 Prep in case a species comparison at the species group resolution is required
        if(resolution == "Species group") {
            print("work in progress")
        }
    
    }
    
    ##################################################
    ## Data plotting.
    ##################################################

    ## P7: Plotting in case a spatial comparison is required
    if(type == "Spatial"){

        ## P7.1 Define study area
        # Define countries map 
        countries <- ne_countries(scale = "medium",
                       type = 'map_units',
                       returnclass = "sf")

        # Define study area extremes
        study_area <- c(st_bbox(contrastDf_Rect))
        
        ## P7.1 Plotting in case a spatial comparison at the ICES Subdivision resolution is required
        if(resolution == "ICES Subdivision") {
            print("work in progress")
        }
    
        ## P7.2 Plotting in case a spatial comparison at the ICES Rectangles resolution is required
        if(resolution == "ICES Rectangle") {
            
            # P7.2.1 Define a resolution factor 
            rf = 1 # Value to enlarge the spatial window at which the data are shown. 
        
            print( 
            ggplot() + 
            geom_sf(
                data = contrastDf_Rect, 
                aes(fill = varInt)
                ) +
            geom_sf(data = countries, fill = "gray90", color = "black") + 
            xlim(study_area[1]-rf, study_area[3]+rf) + 
            ylim(study_area[2]-rf, study_area[4]+rf) + 
            labs(x = "Lon", y = "Lat", fill = paste(as.name(contrastVar))) + 
            scale_fill_viridis(option = "viridis") + 
            theme_bw() 
           )
        }
    }

    ## P8: Prep. in case a temporal comparison is required
    if(type == "Time"){

        ## P8.1 Prep in case a temporal comparison at the year resolution is required
        if(resolution == "Year") {
            print("work in progress")
        }

        ## P8.2 Prep in case a temporal comparison at the semester resolution is required
        if(resolution == "Semester") {
            print("work in progress")
        }

        ## P8.3 Prep in case a temporal comparison at the quarter resolution is required
        if(resolution == "Quarter") {
            print("work in progress")
        }

        ## P8.4 Prep in case a temporal comparison at the month resolution is required
        if(resolution == "Month") {
            print("work in progress")
        }
    
    }
    
    ## P9: Prep. in case a fleet comparison is required
    if(type == "Fleet"){
        print("work in progress")
    }

    ## P9: Prep. in case a species comparison is required
    if(type == "Species"){

        ## P9.1 Prep in case a species comparison at the species unit resolution is required
        if(resolution == "Species unit") {
            print("work in progress")
        }

        ## P9.2 Prep in case a species comparison at the species group resolution is required
        if(resolution == "Species group") {
            print("work in progress")
        }
    
    }
      
}

SamplingCoverage(
    RDBESobj = rdbesobj, 
    var = "SAsampWtLive", 
    contrastVar = "CLoffWeight",
    #by = c("CLyear", "CLmonth"), 
    type = "Spatial", 
    resolution = "ICES Rectangle"
    )
rm(SamplingCoverage)