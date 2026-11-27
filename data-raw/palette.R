#'Function which create palette
library(RColorBrewer)
library(ggplot2)

#define number of extend colors on palette n
cols <- 10

#combine palette
palette1 <- colorRampPalette(brewer.pal(9, "Set1"), interpolate = "spline")(cols)
palette2 <- colorRampPalette(brewer.pal(10, "Set3"), interpolate = "spline")(cols)
palette3 <- colorRampPalette(brewer.pal(8, "Set2"), interpolate = "spline")(cols-1)
palette4 <- colorRampPalette(brewer.pal(8, "Dark2"), interpolate = "spline")(cols)
palette5 <- colorRampPalette(brewer.pal(9, "Pastel1"), interpolate = "spline")(cols-2)
colourPalette <- c(palette1, palette2, palette3, palette4, palette5)

countryInside27 <- c('BE','DE','DK','EE','ES','FI','FR','IE','LT','LV','NL',
                     'PL','PT','SE')

#more countries
countryOutside27<- c('AO','CA','CG','CI','CL','CV','FK','FO','GA','GF','GL','GN',
                     'GP','GW','IS','IT','MA','MQ','MR','MU','NA','NG','NZ','PA',
                     'PE','RE','SC','SN','UY','YT','ZA')

#non-EU countries
countryNonEU <- c('GB','NO')

countries <- sort(unique(c(countryInside27, countryOutside27, countryNonEU)))

names(colourPalette) <- levels(as.factor(countries))

# save .rda
usethis::use_data(colourPalette, overwrite = TRUE)
