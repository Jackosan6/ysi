# toolkit

#' Sample view of a interesing cohort in set
#'
#' @name inspect_ysi
#' @param df A dataframe of interest
#' @return A data frame
#' @export

inspect_ysi <- function(df = NULL){
  select(df, PersonID, HouseID,PersonNo, Sex, Age, State, GCC,TotalInc_p, WageIncImp, MortgageRepay, HouseValTodayImp, HomeEquity, InvestInc_p,BusinessInc_p, Super, NoInHouse)
}

#' YSI Editing Master Variable Name Sheet
#'
#'
#' @name inspect_ysi
#' @param new_vars Argument is intended to be a survey with the Tax_Matrix set, as a data frame
#' @param set  Argument is intended to be a survey with the Tax_Matrix set, as a data frame
#' @param names_bol  True if input is the variable names, False if adding variables using the variable code
#' @param new_names  If the variables that you are adding are unnamed put the new names here
#' @param wave_id  True if input has the wave letter as a prefix
#' @return A data frame of the new Master
#' @export
#


# This script takes the master sheet, and splits it into branches

#            Master - user_subset----------\
#            /                              \
#  Master -<                empty----------------> Master
#           \              /                   /
#            user_subset -<                  /
#                          \               /
#                           named--------/


add_more_vars <- function(new_vars = NULL, set = NULL,  name = T, new_names = NULL, wave_id = F) {

  require(dplyr)

  Master <<- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/R Dev/Hdata Variable Names Code/Hdata_Master_Names.csv", na.strings=c("","NA"))

  if (!is.character(new_vars))
    stop("new_vars needs to be a list or vector of strings")
  if (!is.character(set))
    stop("set is not a string")

  if (!is.logical(name))
    stop("names_T_F needs to be True or False")


  ReducedSet <<- c(new_vars)
  if (wave_id == T) {
    ReducedSet <- sub('.', '', ReducedSet)
  }

  if (name == F) {
    user_subset <- subset(Master, Code %in% ReducedSet)
  }
  if (name == T) {
    user_subset <- subset(Master, Name %in% ReducedSet)
  }


  user_subset<- subset(user_subset, !duplicated(Code))
  user_subset$Set <- set

  user_subset <- user_subset

  # Check for missing names
  if (is.na(user_subset$Name) == T) {
        print("Found Variables without a name")

      empty <- filter(user_subset, is.na(user_subset$Name))
      user_subset <- filter(user_subset, !is.na(user_subset$Name))

      if (length(new_names) < 1 ) {
        print("new_names is empty :( ")
        stop()

      }
      # if (!is.character(new_names))
      #   stop("new_names needs to be a list or vector of strings")

      full_names <- Master$Name
       if (new_names %in% full_names)
         stop("One of the names you want to use is already in use")

      # stopifnot()
      empty$Name <- new_names
      suppressWarnings(user_subset <- bind_rows(user_subset, empty))

   }


  # Bind with
  Master <- suppressMessages(bind_rows(Master, user_subset))
  Master$Descript <- as.character(Master$Descript)
  Master$Set <- as.factor(Master$Set)
  print(paste0("Added ", length(user_subset$Name), " row/s to the data frame"))
  print("PLEASE CHECK THAT THE FUNCTION HAS DONE WHAT YOU EXPECTED before running write_master_names()")

  return(Master)
}


#' YSI Writing Master Variable Name Sheet
#'
#' Write your updated Master to file. Be careful to check that you are happy, and that you're not destroying everything!
#'
#' @name write_master_names
#' @param df Argument is intended to be the REVIEWED updated master table ready to write to file
#' @return A data frame
#' @export
#

write_master_names <- function(df = NULL) {
  Master <<- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/R Dev/Hdata Variable Names Code/Hdata_Master_Names.csv", na.strings=c("","NA"))

  write.csv(Master, file = "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/R dev/Hdata Variable Names Code/Archived files/Hdata_Master_Names_most_recent_archive.csv", row.names = F)

  write.csv(df, file = "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/R dev/Hdata Variable Names Code/Hdata_Master_Names.csv", row.names = F)
}
