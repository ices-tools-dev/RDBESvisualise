#'Function which create palette
library(RColorBrewer)
library(ggplot2)

#define number of extend colors on palette n
cols <- 10
#define vector of colours on pallete
n<-49
#combine palette
palette1 <- colorRampPalette(brewer.pal(9, "Set1"), interpolate = "spline")(cols)
palette2 <- colorRampPalette(brewer.pal(10, "Set3"), interpolate = "spline")(cols)
palette3 <- colorRampPalette(brewer.pal(8, "Set2"), interpolate = "spline")(cols)
palette4 <- colorRampPalette(brewer.pal(8, "Dark2"), interpolate = "spline")(cols)
palette5 <- colorRampPalette(brewer.pal(9, "Pastel1"), interpolate = "spline")(cols)
combined_palette <- c(palette1, palette2, palette3, palette4, palette5)

#barplot(rep(1, length(combined_palette)), col = combined_palette, border = NA, space = 0, main = "Pallete")

# reads RDBES data
# library(RDBEScore)
# RDBESDataObject <- createRDBESDataObject(input = "D:/RCG-RDBES-Overviews/RegionalOverviews/data_RDBES/001_raw/CL Landing RDBES RCG NANASEA Baltic year 2021-2023 2024_05_28.zip")
RDBESDataObject <- data.table::fread(input = "D:/RCG-RDBES-Overviews/RegionalOverviews/data_RDBES/001_raw/CL Landing RDBES RCG NANASEA Baltic year 2021-2023 2024_05_28/CommercialLanding.csv")

#CL <- RDBESDataObject[["CL"]]
CL <- RDBESDataObject
#more countries
country_outside27<- c('AO','CA','CG','CI','CL','CV','FK','FO','GA','GF','GL','GN',
                      'GP','GW','IS','IT','MA','MQ','MR','MU','NA','NG','NZ','PA',
                      'PE','RE','SC','SN','UY','YT','ZA'
)

countries <- c(unique(CL$CLvesselFlagCountry), country_outside27)

#non-EU countries
country_nonEU <- c('GB','NO')

countries <- c(countries, country_nonEU)

names(combined_palette) <- levels(as.factor(countries))

#extend vector
country_extended <- c(levels(factor(countries)), rep(NA, length(combined_palette)-length(levels(factor(countries)))))

#palette in data frame
palette <- data.frame(country = country_extended,
                      color = combined_palette)
# save colours
write.table(palette,file = 'data/colourCountryTab.txt', sep = "\t",row.names = FALSE, col.names = TRUE)


#save definiction of colScale
colScale <- scale_fill_manual(name = "Country",values = combined_palette)

color_info <- paste("scale_fill_manual(name = 'Country', values =list(",
                    paste(sprintf("'%s' = '%s'", names(combined_palette), combined_palette), collapse = ", "),
                    "))")
# save .txt
writeLines(color_info, con = "data/colorCountryColScale.txt")

# #How to use in your script
# #1.load colScale from text file
# color_country <- readLines("data/color_country.txt")
#
# #2. convert text from text into an R object
# colScale <- eval(parse(text = color_country))
#
# #3. Create a plot using the loaded colScale

