#' rectanglesToRegions
#' @param data A vector of input to be transformed
#' @param from The spatial resolution of the input. Possible values are "ICES Rectangles", "ICES Subdivision", "ICES Regions". 
#' @param to The spatial resolution of the output. Possible values are "ICES Rectangles", "ICES Subdivision", "ICES Regions". 
#' @param type Should the conversion provide the a code (e.g. 27.1.a) or full name (e.g. Barents Sea - NEAFC Regulatory Area; available for all levels, except ICES Rectangle)?  
#' @param sf Dichotomous, should the result include geometries? Possible values are TRUE or FALSE. 
#' @param plot Dichotomous, should the result be plotted? Possible values are TRUE or FALSE. 
#' @return A vector and a plot, the latter depending on user selection. 
#'
#' @examples
#' \dontrun{
#'
#' 
#' }
#' 

###################################################################################
# 
# This internal function is aimed at geographical conversion between ICES 
# spatial units, mainly regarding to the conversion from ICES Rectangles to 
# ICES "Regions". 
# 
#
# Inquiries: Eros Quesada SLU Aqua Sweden @RDBESvisualise
#
###################################################################################

rectanglesToRegions <- function(
    data, 
    from = c("ICES Rectangles", "ICES DivAndSubdivision", "ICES Area", "ICES Subarea", "ICES Division", "ICES Subdivision", "ICES Regions"), 
    to =  c("ICES Rectangles", "ICES DivAndSubdivision",  "ICES Area", "ICES Subarea", "ICES Division", "ICES Subdivision", "ICES Regions"), 
    type = c("code", "name"),
    sf = c(TRUE, FALSE), 
    plot = c(TRUE, FALSE)
){
  
  #################################
  # Check input
  #################################
  
  ## 1. The conversion needs from and to to differ. 
  if(from == to){
    stop("The parameters 'from' and 'to' cannot coincide")
  }
  
  if(
    
    (from == "ICES Regions" & to %in% c("ICES Rectangles", "ICES DivAndSubdivision",  "ICES Area", "ICES Subarea", "ICES Division", "ICES Subdivision")) |
    (from == "ICES Area" & to %in% c("ICES Rectangles", "ICES DivAndSubdivision", "ICES Subarea", "ICES Division", "ICES Subdivision")) |
    (from == "ICES Subarea" & to %in% c("ICES Rectangles", "ICES DivAndSubdivision", "ICES Division", "ICES Subdivision")) |
    (from == "ICES Division" & to %in% c("ICES Rectangles", "ICES DivAndSubdivision",  "ICES Area", "ICES Subarea")) |
    (from == "ICES Subdivision" & to %in% c("ICES Rectangles", "ICES DivAndSubdivision", "ICES Division")) |
    (from == "ICES DivAndSubdivision" & to %in% c("ICES Rectangles"))
    
  ){
    
    stop("Cannot convert to a lower resolution! Check your 'to' resolution")
  
  }
  
  #################################
  # Load and wrangle data to convert
  #################################
  
  ## 2. Load the data containing information on ICES Rectangles and ICES Subdivisions
  ices_rects <- RDBESvisualise::icesRectSF

  ## 3. Select columns of interest in the ices_rects data 
  ices_recToReg <- ices_rects %>% dplyr::select(ICESNAME, Area_27)
  
  ## 4. Format the names 
  ices_recToReg <- ices_recToReg %>% dplyr::rename(statRec = ICESNAME)  
  
  ## 5: Format Area_27 
  ices_recToReg <- ices_recToReg %>% dplyr::mutate(Area_27 = paste0("27.", Area_27))
  
  
  ## 6: Produce upper spatial resolutions
  ices_recToReg <- ices_recToReg %>% 
    mutate(
      
      statSubDivision = case_when(
        
        Area_27 %in% c("27.1.a", "27.1.b") ~ NA, # These are Divisions
        Area_27 %in% c("27.2.a.1", "27.2.a.2", "27.2.b.1", "27.2.b.2") ~ paste(Area_27), # These are Subdivisions
        Area_27 %in% c("27.3.a.20", "27.3.a.21", "27.3.b.23", "27.3.c.22", "27.3.d.24", "27.3.d.25", "27.3.d.27", "27.3.d.30", "27.3.d.26", "27.3.d.28.2", "27.3.d.29", "27.3.d.31", "27.3.d.28.1", "27.3.d.32") ~ paste(Area_27), # These are Subdivisions 
        Area_27 %in% c("27.4.a", "27.4.b", "27.4.c") ~ NA, # These are Divisions
        Area_27 %in% c("27.5.a.1", "27.5.a.2") ~ paste(Area_27), # These are Subdivisions
        Area_27 %in% c("27.5.b.1.a", "27.5.b.1.b") ~ paste("27.5.b"), # These are some unit below Subdivisions
        Area_27 %in% c("27.5.b.2") ~ paste(Area_27), # These are Subdivisions
        Area_27 %in% c("27.6.a") ~ NA, # These are Divisions
        Area_27 %in% c("27.6.b.2", "27.6.b.1") ~ paste(Area_27), # These are Subdivisions
        Area_27 %in% c("27.7.a") ~ NA,
        Area_27 %in% c("27.7.b") ~ NA,
        Area_27 %in% c("27.7.c.1", "27.7.c.2") ~ paste(Area_27), # These are Subdivisions
        Area_27 %in% c("27.7.d") ~ NA,
        Area_27 %in% c("27.7.e") ~ NA,
        Area_27 %in% c("27.7.f") ~ NA,
        Area_27 %in% c("27.7.g") ~ NA,
        Area_27 %in% c("27.7.h") ~ NA,
        Area_27 %in% c("27.7.k.1", "27.7.k.2") ~ paste(Area_27), # These are Subdivisions
        Area_27 %in% c("27.7.j.1", "27.7.j.2") ~ paste(Area_27), # These are Subdivisions
        Area_27 %in% c("27.8.a") ~ NA,
        Area_27 %in% c("27.8.b") ~ NA,
        Area_27 %in% c("27.8.c") ~ NA,
        Area_27 %in% c("27.8.d.2", "27.8.d.1") ~ paste(Area_27), # These are Subdivisions
        Area_27 %in% c("27.8.e.1", "27.8.e.2") ~ paste(Area_27), # These are Subdivisions
        Area_27 %in% c("27.9.a") ~ NA,
        Area_27 %in% c("27.9.b.1", "27.9.b.2") ~ paste(Area_27), # These are Subdivisions
        Area_27 %in% c("27.10.a.1", "27.10.a.2") ~ paste(Area_27), # These are Subdivisions
        Area_27 %in% c("27.10.b") ~ NA,  # These are Divisions
        Area_27 %in% c("27.12.a.1", "27.12.a.2", "27.12.a.3", "27.12.a.4") ~ paste(Area_27),  # These are Subdivisions
        Area_27 %in% c("27.12.b") ~ NA,  # These are Divisions
        Area_27 %in% c("27.12.c") ~ NA,  # These are Divisions
        Area_27 %in% c("27.14.a") ~ NA,  # These are Divisions
        Area_27 %in% c("27.14.b.1", "27.14.b.2")  ~ NA  # These are Subdivisions
        
      ), 
      
      statDivision = case_when(
        
        Area_27 %in% c("27.1.a", "27.1.b") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.2.a.1", "27.2.a.2") ~ paste("27.2.a"), # These are Subdivisions
        Area_27 %in% c("27.2.b.1", "27.2.b.2") ~ paste("27.2.b"), # These are Subdivisions
        Area_27 %in% c("27.3.a.20", "27.3.a.21") ~ paste("27.3.a"), # These are Subdivisions 
        Area_27 %in% c("27.3.b.23", "27.3.c.22") ~ paste("27.3.b,c"), # These are Subdivisions
        Area_27 %in% c("27.3.d.24", "27.3.d.25", "27.3.d.27", "27.3.d.30", "27.3.d.26", "27.3.d.28.2", "27.3.d.29", "27.3.d.31", "27.3.d.28.1", "27.3.d.32") ~ paste("27.3.d"), # These are Subdivisions
        Area_27 %in% c("27.4.a", "27.4.b", "27.4.c") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.5.a.1", "27.5.a.2") ~ paste("27.5.a"), # These are Subdivisions
        Area_27 %in% c("27.5.b.1.a", "27.5.b.1.b", "27.5.b.2") ~ paste("27.5.b"), # These are Subdivisions
        Area_27 %in% c("27.6.a") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.6.b.2", "27.6.b.1") ~ paste("27.6.b"), # These are Subdivisions
        Area_27 %in% c("27.7.a") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.7.b") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.7.c.1", "27.7.c.2") ~ paste("27.7.c"), # These are Subdivisions
        Area_27 %in% c("27.7.d") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.7.e") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.7.f") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.7.g") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.7.h") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.7.k.1", "27.7.k.2") ~ paste("27.7.k"), # These are Subdivisions
        Area_27 %in% c("27.7.j.1", "27.7.j.2") ~ paste("27.7.j"), # These are Subdivisions
        Area_27 %in% c("27.8.a") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.8.b") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.8.c") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.8.d.2", "27.8.d.1") ~ paste("27.8.d"), # These are Subdivisions
        Area_27 %in% c("27.8.e.1", "27.8.e.2") ~ paste("27.8.e"), # These are Subdivisions
        Area_27 %in% c("27.9.a") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.9.b.1", "27.9.b.2") ~ paste("27.9.b"), # These are Subdivisions
        Area_27 %in% c("27.10.a.1", "27.10.a.2") ~ paste("27.10.a"), # These are Subdivisions
        Area_27 %in% c("27.10.b") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.12.a.1", "27.12.a.2", "27.12.a.3", "27.12.a.4") ~ paste("27.12.a"),  # These are Subdivisions
        Area_27 %in% c("27.12.b") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.12.c") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.14.a") ~ paste(Area_27),  # These are Divisions
        Area_27 %in% c("27.14.b.1", "27.14.b.2")  ~ paste("27.14.b")  # These are Subdivisions
        
      ), 
      
      statSubArea = case_when(
        
        Area_27 %in% c("27.1.a", "27.1.b") ~ paste("27.1"), 
        Area_27 %in% c("27.2.a.1", "27.2.a.2", "27.2.b.1", "27.2.b.2") ~ paste("27.2"), 
        Area_27 %in% c("27.3.a.20", "27.3.a.21", "27.3.b.23", "27.3.c.22", "27.3.d.24", "27.3.d.25", "27.3.d.27", "27.3.d.30", "27.3.d.26", "27.3.d.28.2", "27.3.d.29", "27.3.d.31", "27.3.d.28.1", "27.3.d.32") ~ paste("27.3"), # These are Subdivisions 
        Area_27 %in% c("27.4.a", "27.4.b", "27.4.c") ~ paste("27.4"),
        Area_27 %in% c("27.5.a.1", "27.5.a.2", "27.5.b.1.a", "27.5.b.1.b", "27.5.b.2") ~ paste("27.5"), # These are Subdivision
        Area_27 %in% c("27.6.a", "27.6.b.2", "27.6.b.1") ~ paste("27.6"),
        Area_27 %in% c("27.7.a", "27.7.b", "27.7.c.1", "27.7.c.2", "27.7.d", "27.7.e", "27.7.f", "27.7.g", "27.7.h", "27.7.k.1", "27.7.k.2", "27.7.j.1", "27.7.j.2") ~ paste("27.7"), 
        Area_27 %in% c("27.8.a", "27.8.b", "27.8.c", "27.8.d.2", "27.8.d.1", "27.8.e.1", "27.8.e.2") ~paste("27.8"), 
        Area_27 %in% c("27.9.a", "27.9.b.1", "27.9.b.2") ~ paste("27.9"),
        Area_27 %in% c("27.10.a.1", "27.10.a.2", "27.10.b") ~ paste("27.10"), 
        Area_27 %in% c("27.12.a.1", "27.12.a.2", "27.12.a.3", "27.12.a.4", "27.12.b", "27.12.c") ~ paste("27.12"),  
        Area_27 %in% c("27.14.a", "27.14.b.1", "27.14.b.2") ~ paste("27.14")
        
      ), 
      
      statArea = paste("27")
      
    )
    
  ## 7. Add region
  ices_recToReg <- ices_recToReg %>% 
    mutate(
      statRegion = case_when(
        statSubArea %in% c("27.3") & statDivision %in% c("27.3.b,c", "27.3.d") ~ "BA", 
        
        (statSubArea %in% c("27.1")) |
        (statSubArea %in% c("27.2")) |
        (statSubArea %in% c("27.3") & statDivision %in% c("27.3.a")) |
        (statSubArea %in% c("27.4")) |
        (statSubArea %in% c("27.5") & statDivision %in% c("27.5.a")) |
        (statSubArea %in% c("27.7") & statDivision %in% c("27.7.d")) |
        (statSubArea %in% c("27.12")) |
        (statSubArea %in% c("27.14")) ~ "NSEA", 
        
        (statSubArea %in% c("27.5") & statDivision %in% c("27.5.b")) |
        (statSubArea %in% c("27.6") & statDivision %in% c("27.6.a", "27.6.b")) |
        (statSubArea %in% c("27.7") & statDivision %in% c("27.7.k", "27.7.c", "27.7.j", "27.7.b", "27.7.h", "27.7.g", "27.7.a", "27.7.e", "27.7.f")) | 
        (statSubArea %in% c("27.8") & statDivision %in% c("27.8.a", "27.8.b", "27.8.c", "27.8.d", "27.8.d", "27.8.e", "27.8.e")) | 
        (statSubArea %in% c("27.9") & statDivision %in% c("27.9.a", "27.9.b")) | 
        (statSubArea %in% c("27.10") & statDivision %in% c("27.10.a", "27.10.b")) ~ paste("NAtl"), 
        
        statArea %in% c("21") ~ paste("NAFO")
      )
    )

  ## 8. Give names
  ices_recToReg <- ices_recToReg %>% 
    dplyr::mutate(
      statAreaPlainName = case_when(
        statArea == "21" ~ paste("Atlantic Northwest"), 
        statArea == "27" ~ paste("Atlantic Northeast")
      ), 
      statSubAreaPlainName = case_when(
        statSubArea == "27.1" ~ paste("Barents Sea"), 
        statSubArea == "27.2" ~ paste("Norwegian Sea, Spitzbergen, and Bear Island"), 
        statSubArea == "27.3" ~ paste("Skagerrak, Kattegat, Sound, Belt Sea, and Baltic Sea"), 
        statSubArea == "27.4" ~ paste("North Sea"), 
        statSubArea == "27.5" ~ paste("Iceland and Faroes Grounds"), 
        statSubArea == "27.6" ~ paste("Rockall, Northwest Coast of Scotland and North Ireland"), 
        statSubArea == "27.7" ~ paste("Irish Sea, West of Ireland, Porcupine Bank, Eastern and Western English Channel, etc "), 
        statSubArea == "27.8" ~ paste("Bay of Biscay"), 
        statSubArea == "27.9" ~ paste("Portuguese Waters"), 
        statSubArea == "27.10" ~ paste("Azores Grounds and Northeast Atlantic South"), 
        statSubArea == "27.12" ~ paste("North of Azores"), 
        statSubArea == "27.14" ~ paste("East Greenland")
      ), 
      statDivisionPlainName = case_when(
        statDivision == "27.1.a" ~ paste("Barents Sea - NEAFC Regulatory Area"),
        statDivision == "27.1.b" ~ paste("Barents Sea Non-NEAFC Regulatory Area (Division 27.1.b)"),
        statDivision == "27.2.a" ~ paste("Norwegian Sea"),
        statDivision == "27.2.b" ~ paste("Spitzbergen and Bear Island"),
        statDivision == "27.3.a" ~ paste("Skagerrak and Kattegat"),
        statDivision == "27.3.b,c" ~ paste("Sound and Belt Sea or the Transition Area"),
        statDivision == "27.3.d" ~ paste("Baltic Sea"),
        statDivision == "27.4.a" ~ paste("Northern North Sea"),
        statDivision == "27.4.b" ~ paste("Central North Sea"),
        statDivision == "27.4.c" ~ paste("Southern North Sea"),
        statDivision == "27.5.a" ~ paste("Iceland Grounds"),
        statDivision == "27.5.b" ~ paste("Faroes Grounds "),
        statDivision == "27.6.a" ~ paste("Northwest Coast of Scotland and North Ireland or as the West of Scotland"),
        statDivision == "27.6.b" ~ paste("Rockall"),
        statDivision == "27.7.a" ~ paste("Irish Sea"),
        statDivision == "27.7.b" ~ paste("West of Ireland"),
        statDivision == "27.7.c" ~ paste("Porcupine Bank"),
        statDivision == "27.7.d" ~ paste("Eastern English Channel"),
        statDivision == "27.7.e" ~ paste("Western English Channel"),
        statDivision == "27.7.f" ~ paste("Bristol Channel"),
        statDivision == "27.7.g" ~ paste("Celtic Sea North"),
        statDivision == "27.7.h" ~ paste("Celtic Sea South"),
        statDivision == "27.7.j" ~ paste("Southwest of Ireland - East"),
        statDivision == "27.7.k" ~ paste("Southwest of Ireland - West"), 
        statDivision == "27.8.a" ~ paste("Bay of Biscay - North"), 
        statDivision == "27.8.b" ~ paste("Bay of Biscay - Central"), 
        statDivision == "27.8.c" ~ paste("Bay of Biscay - South"), 
        statDivision == "27.8.d" ~ paste("Bay of Biscay - Offshore"), 
        statDivision == "27.8.e" ~ paste("West of Bay of Biscay"),
        statDivision == "27.9.a" ~ paste("Portuguese Waters - East"),
        statDivision == "27.9.b" ~ paste("Portuguese Waters - West"),
        statDivision == "27.10.a" ~ paste("Azores Grounds"), 
        statDivision == "27.10.b" ~ paste("Northeast Atlantic South"),
        statDivision == "27.12.a" ~ paste("Southern mid-Atlantic Ridge"),
        statDivision == "27.12.b" ~ paste("Western Hatton Bank"),
        statDivision == "27.12.c" ~ paste("Central Northeast Atlantic - South"),
        statDivision == "27.14.a" ~ paste("Northeast Greenland"),
        statDivision == "27.14.b" ~ paste("Southeast Greenland")
      ), 
      statSubDivisionPlainName = NA # To be completed. 
    )
  
  ## 9. Rename the Area_27 so that it has a more general name 
  ices_recToReg <- ices_recToReg %>% 
    dplyr::rename(
      genGeoICESName = Area_27
    )
  
  ## 10. Reorder the columns 
  ices_recToReg <- ices_recToReg %>% 
    dplyr::select(
      statRec, genGeoICESName, statSubDivision, statDivision, statSubArea, statArea, statRegion, 
      statAreaPlainName, statSubAreaPlainName, statDivisionPlainName, statSubDivisionPlainName
    )

  #################################
  # Convert
  #################################  
  
  ## 11: Convert user inputs in the name of the variables used in the function 
  # For the from parameter 
  varToConvertFrom <- case_when(
    from == "ICES Rectangles" ~ paste("statRec"), 
    from == "ICES DivAndSubdivision" ~ paste("genGeoICESName"), 
    from == "statArea" ~ paste("statArea"), 
    from == "statSubArea" ~ paste("statSubArea"), 
    from == "ICES Division" ~ paste("statDivision"), 
    from == "ICES Subdivision" ~ paste("statSubDivision"), 
    from == "ICES Regions" ~ paste("statRegion")
  )
    
  # For the to parameter
  varToConvertTo <- case_when(
    to == "ICES Rectangles" ~ paste("statRec"), 
    to == "ICES DivAndSubdivision" ~ paste("genGeoICESName"), 
    to == "statArea" ~ paste("statArea"), 
    to == "statSubArea" ~ paste("statSubArea"), 
    to == "ICES Division" ~ paste("statDivision"), 
    to == "ICES Subdivision" ~ paste("statSubDivision"), 
    to == "ICES Regions" ~ paste("statRegion")
  )
  
  ## 12 Build the input data
  inputData <- data.frame(varToConvertFrom = data)
  names(inputData) <- varToConvertFrom
  
  ## 13 Convert the data
  outputData <- merge(ices_recToReg, inputData, all.y = T, all.x = F)
  

  ## 14 Select the column of interest
  outputData <- outputData %>% 
    dplyr::select(
      as.name(varToConvertFrom), as.name(varToConvertTo)
    )
  
  ## 15 Adjust the geometry
  if(varToConvertTo != "statRec"){
    
    if(varToConvertTo == "genGeoICESName"){
      
      ## Need to merge with an sf reporting the general division/subdivision (Area_27) here
      
    }
    
    if(varToConvertTo == "statArea"){
      
      ## Need to merge with an sf reporting the area here
      
    }
    
    if(varToConvertTo == "statSubArea"){
      
      ## Need to merge with an sf reporting the subarea here
      
    }
    
    if(varToConvertTo == "statDivision"){
      
      ## Need to merge with an sf reporting the divisions here
      
    }
    
    if(varToConvertTo == "statSubDivision"){
      
      ## Need to merge with an sf reporting the subdivisions here
      
    }
    
    if(varToConvertTo == "statRegion"){
      
      ## Need to merge with an sf reporting the regions here
      
    }
    
  }

  
  #################################
  # plot
  #################################
  
  ## 16 Plot, according to user selection
  if(plot){
    
    ggplot() + 
      geom_sf(data = outputData, aes(fill = as.name(varToConvertTo))) + 
      theme_bw()
    
  }
  
  
  #################################
  # Output 
  #################################
  
  ## 17 Select the var of interest for output
  output <- outputData %>% 
    dplyr::select(varToConvertTo) 
  
  ## 18: Provide or not geometry and pull
  if(!sf){
    
    output <- output %>% 
      st_drop_geometry() %>% 
      pull
    
  } 

  ## 18 Return the output
  output
   
  }
