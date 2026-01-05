#' Provides barplots of the inputted table column, summed by up til 4 factors,
#' inserted as the column names from the same table.
#'
#' @param RDBESobj an RDBESDataObject)
#' @param var one column name from the RDBESDataObject)
#' @param valBy one or more column name from the same table as the 'var' variable)
#' The order of the column names put in the 'valBy' determines how the plot is arranged
#' columns with many factorlevels can be put in the end to get a new plot pr level.
#' @param output_type Should the output be a bar plot or a table, written to the environment)

amountByGroup <- function(RDBESobj = myH1RawObject,
                          var = "CEnumDomTrip",
                          valBy = c("CEmetier6"),
                          output_type = "plot"){


  #Check that all inputted columns are from the same table
  if (length(unique(c(substr(var, 1, 2), substr(valBy, 1, 2)))) > 1){
    print("Only Columns from the Same Table is Allowed in this Function")
  } else{

    #pick data
    dat <- RDBESobj[[substr(var, 1, 2)]]

    #if data is from these two hierarchies, the occurrence frequency is outputted, else normal barplot of data
    if (substr(var, 1, 2) %in% c("SA", "BV")){
      dat <- data.frame(dat)
      dat <- data.frame(table(dat[, c(var, valBy)]))
      valBy <- c("Freq", valBy)
    } else{
      dat <- dat[, lapply(.SD, sum), by = valBy, .SDcols = var]

      #Format columns specefically for SA
      if(substr(var, 1, 2) == "SA"){
        dat$CLspecCode <- as.factor(dat$CLspecCode)
        dat$CLoffWeight <- dat$CLoffWeight/1000
        dat$CLsciWeight <- dat$CLsciWeight/1000
      }
    }

    if (output_type == "plot") {

      #plot theme
      thm <- theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),)
      lbs <- labs(title = paste0(var, " by ", toString(valBy)), x = valBy[1], y = var)

      #plot the data
      dat <- data.frame(dat)
      p <- ggplot(data=dat, aes(x=dat[, valBy[1]], y=dat[, var])) +
        geom_bar(stat="identity")+
        lbs +
        thm

      #sequence to build a more complex plot depending on number of inputted 'valBy' variables
      if (length(valBy) == 1){
        p

      } else if (length(valBy) == 2){
        p <- p + facet_wrap(~dat[, valBy[2]])

        p

      } else if (length(valBy) == 3 & #only use facet grid if factor level is fewer then 5, else loop over
                 length(unique(dat[, valBy[2]])) < 5 &
                 length(unique(dat[, valBy[3]])) < 5){

        p <- p + facet_grid(dat[, valBy[3]] ~ dat[, valBy[2]])

        p

      } else if (length(valBy) > 2){

        for(i in sort(unique(dat[, valBy[length(valBy)]]))) {
          dat$v3 <- dat[, valBy[length(valBy)]]
          dat2 <- dat[dat$v3 == i, ]

          p <- ggplot(data=dat2, aes(x=dat2[, valBy[1]], y=dat2[, var])) +
            geom_bar(stat="identity")+
            lbs +
            ggtitle(paste0(var, " by ", toString(valBy[1:(length(valBy)-1)]),
                           " for ", valBy[length(valBy)], " ", i)) +
            thm

          if (length(valBy) == 3)
            p <- p + facet_wrap(~dat2[, valBy[2]])

          if (length(valBy) == 4)
            p <- p + facet_grid(dat2[, valBy[3]] ~ dat2[, valBy[2]])

          print(p)
        }
        print("Multiple Plots May Have Been Generated")

      }

    } else if(output_type == "table") {
      assign(
        x = paste0("tbl_", var, "_", toString(valBy, sep = "_") ),
        value = dat,
        envir = .GlobalEnv)

    } else {
      print("Output Type Not Available")
    }
  }
}
