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

produceSamplingCoverage <- function(
    RDBESobj,
    var = c(
        "SAsampWtLive",
        "SAnumSamp",
        "SAamountSamp", 
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
    catchCat = NA, 
    showSamples = T,
    output_type = NA, 
    verbose = T
    ) {

    ##################################################
    ### Data preparation
    ##################################################

    ## P1: Extract the sampling data from the RDBES object. 
    # Through preprocessSampleDataForCoverage() extract sampling data from RDBESobject. 
    samplingDf <- preprocessSamplingDataForCoverage( 
        rdbesobj, 
        verbose = T
    )
        
    ## P1.2: Additional subsetting of data [year, quarter, vesselFlag currently available]
    # Through filterSampleDataForCoverage() we filter the data based on the function's input parameters. 
    samplingDf <- filterSampleDataForCoverage(
        sampleData = samplingDf, 
        yearToFilter = year,
        quarterToFilter = quarter,
        vesselFlag = vesselFlag, 
        catchCat = catchCat,
        verbose = verbose
    )
    
    ## P2: Prepare the data in case the variable of interest is related to landing. 

    if(contrastVar %in% c("CLoffWeight", "CLsciWeight", "CLtotalOfficialLandingsValue")) {
        
        ## P2.2: Extract the CL table from the RDBES object. 
        # Through preprocessLandingsDataForCoverage() extract CL from RDBESobject and merge with wormsSpecies.rda to obtain latin name from Aphia codes.
        contrastDf <- preprocessLandingsDataForCoverage( 
            RDBESobj,  
            verbose = verbose 
            )

        ## P2.3: Additional subsetting of data [year, quarter, vesselFlag currently available]
        # Through filterLandingsDataForCoverage() we filter the data based on the function's input parameters. 
        contrastDf <- filterLandingsDataForCoverage(
            landingsData = contrastDf, 
            yearToFilter = year,
            quarterToFilter = quarter,
            vesselFlag = vesselFlag,
            verbose = verbose
            )

    }

    ## P3: Prepare the data in case the variable of interest is related to effort. 
    # In this case we need to extract the CE table from the RDBES object. 
    if(contrastVar %in% c("CEnumFracTrips", "CEnumDomTrip")) {
    print("work in progress")
    }
    
    ## P4: Define the data we have been given
    # Define set of directions we received from the user in general terms. 
    if(contrastVar %in% c("CEnumFracTrips", "CEnumDomTrip")) {

        contrast_type = paste("Effort")
        contrast = paste("Effort")

    } else {

        if(contrastVar %in% c("CLoffWeight", "CLsciWeight")) {
            
            contrast_type = paste("Landing")
            contrast = paste("Landing - Weight")

        } else if (contrastVar == "CLtotalOfficialLandingsValue") {
            
            contrast_type = paste("Landing")
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

    
    ## P5: Prepare the by variable depending on case 
    # P5.1: For sampling 
      byVar_SA <- case_when(
        
        resolution == "ICES Subdivision" ~ "SAarea", 
        resolution == "ICES Rectangle" ~ "SAstatRect",
        resolution == "Year" ~ NA, 
        resolution == "Semester" ~ NA, 
        resolution == "Quarter" ~ NA, 
        resolution == "Month" ~ NA, 
        resolution == "Scale of fishery" ~ NA, 
        resolution == "Species unit" ~ NA, 
        resolution == "Species group" ~ NA 
      
        )
    
    # P5.2 For contrast
    if(grepl("Landing", contrast)) { # If the contrast variable is landing
      
      byVar_CT <- case_when(
        
        resolution == "ICES Subdivision" ~ "CLarea", 
        resolution == "ICES Rectangle" ~ "CLstatRect",
        resolution == "Year" ~ NA, 
        resolution == "Semester" ~ NA, 
        resolution == "Quarter" ~ NA, 
        resolution == "Month" ~ NA, 
        resolution == "Scale of fishery" ~ NA, 
        resolution == "Species unit" ~ NA, 
        resolution == "Species group" ~ NA 
        
      )
    
    } else { # If the contrast variable is effort
      
      print("work in progress")
      
    }
    
    ## 5.3 Finally define the grouping variables for sampling and contrast 
    if(all(is.na(by))) { # If only one is present
      grp_vars_SA = byVar_SA
      grp_vars_CT = byVar_CT
    } else { # If more than one is present. 
      grp_vars_SA = c(byVar_SA, by)
      grp_vars_CT = c(byVar_CT, by)
    }
  
    
    ## P7: Prepare sampling and contrast df. 
    ## P7.1: Sampling data
    
      # Aggregate the sampling data according to the variable of interest     
      if(var %in% c( # If the variable is in the columns already present in the RDBES SA table,
          "SAsampWtLive",
          "SAnumSamp",
          "SAsampWtMes"
          )
         ){
        
          samplingDf <- samplingDf %>%  # Then we performed grouped sum of the values appearing in them. 
              group_by_at(grp_vars_SA) %>%
              dplyr::summarize(
                  varInt = sum(eval(as.name(var)), na.rm = T)
              )
          
      } else if (var == "SAamountSamp") { # Else, if the variable is SAamountSamp which is not present in the RDBES SA table
          
          samplingDf <- samplingDf %>%  # We must count the amount of records 
              dplyr::select(c(grp_vars_SA, SSid, SAspeCode)) %>% # We do it taking into account sample, spatial and species information + the variables selected by the user !check 
              distinct() %>% # Avoid duplicates !check 
              group_by_at(grp_vars_SA) %>%
              dplyr::summarize(
                  varInt = n()
              )
          
      }    
    
    ## P7.2: Contrast data
    # Aggregate the contrast data according to the variable of contrast     
    if(grepl("Landing", contrast)){ # If the contrast variable is landing
    
    ## P7.2.1 Remove those records not having an associated rectangle 
    contrastDf <- contrastDf %>% 
      dplyr::filter(!(grp_vars_CT %in% c("-9") | is.na(grp_vars_CT))) # To be edited in case length of by > 1
          
    ## P7.2.2 Aggregate the data by the grp_vars_CT variable (any other vars, if any)
    contrastDf <- contrastDf %>% 
            group_by_at(grp_vars_CT) %>%
            dplyr::summarize(
                varInt = sum(eval(as.name(contrastVar)), na.rm = T)
            ) 
            
        } else if (contrast == "Effort"){

            print("work in progress")
        
        }
        
    # 5.3: Finalize the preparation 
    # Merge the two 
    Sampling_vs_Contrast_plg <- merge(
      samplingDf, 
      contrastDf, 
      suffix = c("_sampling","_contrast"), 
      by.x = grp_vars_SA, 
      by.y = grp_vars_CT, 
      all = T
      ) 
        
    ## Any additional merge 
    if(type == "Spatial"){
    
      if(resolution == "ICES Rectangle") {
        
        ## P5.2.1 Load shapefile of ICES Rectangles
        ices_rects <- RDBESvisualise::icesRectSF
        
        ## Rename the spatial column
        Sampling_vs_Contrast_plg <- Sampling_vs_Contrast_plg %>% 
        dplyr::rename(
          statRect = 1
        )
        
        ## P5.2.5 Merge the result with the spatial object
        # Merge the resulting dataframe with the shapefile relative to the ICES Rectangles
        Sampling_vs_Contrast_plg <- merge(ices_rects, Sampling_vs_Contrast_plg, by.x = "ICESNAME", by.y = "statRect", all.y = T)
        
        
        ## We can use the result to extract centroids [later used to center the sampling info]
        Sampling_vs_Contrast_cntr <- Sampling_vs_Contrast_plg %>% 
          st_centroid()
        
      }
      
    }
      
    # ## P6: Prep. in case a temporal comparison is required
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

    # 
    # ## P7: Prep. in case a fleet comparison is required
    # if(type == "Fleet"){
    #     print("work in progress")
    # }
    # 
    # ## P8: Prep. in case a species comparison is required
    # if(type == "Species"){
    # 
    #     ## P8.1 Prep in case a species comparison at the species unit resolution is required
    #     if(resolution == "Species unit") {
    #         print("work in progress")
    #     }
    # 
    #     ## P8.2 Prep in case a species comparison at the species group resolution is required
    #     if(resolution == "Species group") {
    #         print("work in progress")
    #     }
    # 
    # }
    
    ##################################################
    ## Data plotting.
    ##################################################

    ## P9: Plotting in case a spatial comparison is required
    if(type == "Spatial"){

        ## P9.1 Define study area
        # Define countries map 
        countries <- ne_countries(
          scale = "medium",
          type = 'map_units',
          returnclass = "sf"
          )

        # Define study area extremes
        study_area <- c(st_bbox(Sampling_vs_Contrast_cntr))
        
        ## P9.2 Plotting in case a spatial comparison at the ICES Subdivision resolution is required
        if(resolution == "ICES Subdivision") {
            print("work in progress")
        }
    
        ## P9.3 Plotting in case a spatial comparison at the ICES Rectangles resolution is required
        if(resolution == "ICES Rectangle") {
            
            # P9.3.1 Define a resolution factor 
            rf = 1 # Value to enlarge the spatial window at which the data are shown. 
        
            print( 
            ggplot() + 
            geom_sf(
                data = Sampling_vs_Contrast_plg, 
                aes(fill = varInt_contrast)
                ) +
            geom_sf(
                data = Sampling_vs_Contrast_cntr, 
                aes(size = varInt_sampling),
                color = "white",
                shape = 1
            ) + 
            geom_sf(data = countries, fill = "gray90", color = "black") + 
            xlim(study_area[1]-rf, study_area[3]+rf) + 
            ylim(study_area[2]-rf, study_area[4]+rf) + 
            labs(
                x = "Lon", 
                y = "Lat", 
                fill = paste0(
                    "Contrast variable:", as.name(contrastVar), " (", contrast_type, ")"), 
                size = paste("Sampling variable:", as.name(var))
                ) + 
            scale_fill_viridis(option = "viridis") + 
            theme_bw() + 
            theme(
                legend.position = "bottom",
                legend.title.align = 0.5, 
                legend.key.width = unit(1, 'cm'),
            ) + 
            guides(
                fill = guide_colourbar(
                    title.position="top", title.hjust = 0.5
                    ),
                size = guide_legend(
                    title.position="top", title.hjust = 0.5,
                    override.aes = list(color = "black")
                    )
                )
           ) 
        }
    }

    ## P10: Prep. in case a temporal comparison is required
    if(type == "Time"){

        ## P10.1 Prep in case a temporal comparison at the year resolution is required
        if(resolution == "Year") {
            print("work in progress")
        }

        ## P10.2 Prep in case a temporal comparison at the semester resolution is required
        if(resolution == "Semester") {
            print("work in progress")
        }

        ## P10.3 Prep in case a temporal comparison at the quarter resolution is required
        if(resolution == "Quarter") {
            print("work in progress")
        }

        ## P10.4 Prep in case a temporal comparison at the month resolution is required
        if(resolution == "Month") {
            print("work in progress")
        }
    
    }
    
    ## P11: Prep. in case a fleet comparison is required
    if(type == "Fleet"){
        print("work in progress")
    }

    ## P12: Prep. in case a species comparison is required
    if(type == "Species"){

        ## P9.13 Prep in case a species comparison at the species unit resolution is required
        if(resolution == "Species unit") {
            print("work in progress")
        }

        ## P9.14 Prep in case a species comparison at the species group resolution is required
        if(resolution == "Species group") {
            print("work in progress")
        }
    
    }
      
}
