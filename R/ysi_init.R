#' YSI Startup Function
#'
#' This function is for the star of your script, bringing in the data and setting the data up for your analysis.
#' There is no need to be concerned that your varible needs are not in every wave, as this function will just find whatever is avilable.
#'
#' @name ysi_init
#' @param set This argument needs to be a string that is one of the set names in the YSI master sheet.
#' @param xwave Expects a logical input, xwave data (T) or single wave data (F)
#' @param wave_n If you want a single wave of data, specify which year in the form of an integer between 1 and 14
#' @return A set of tables, including Master (the variable master sheet) and your_data (your data subset), and user_subset (filtered version of Master with your set's variables)
#' @export

ysi_init <- function(set = "key", xwave = F, wave_n = 14) {


  list.of.packages <- c("tidyr", "feather","dplyr", "data.table","devtools", "ggplot2","scales", "ggrepel","DataCombine")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)

  suppressWarnings(suppressPackageStartupMessages(library(tidyr)))
  suppressWarnings(suppressPackageStartupMessages(library(feather)))
  suppressWarnings(suppressPackageStartupMessages(library(dplyr)))
  suppressWarnings(suppressPackageStartupMessages(library(data.table)))
  suppressWarnings(suppressPackageStartupMessages(library(devtools)))
  suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))
  suppressWarnings(suppressPackageStartupMessages(library(scales)))
  suppressWarnings(suppressPackageStartupMessages(library(ggrepel)))
  suppressWarnings(suppressPackageStartupMessages(library(DataCombine)))


  Master <<- read.csv("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/R Dev/Hdata Variable Names Code/Hdata_Master_Names.csv", na.strings=c("","NA"))
  Sets <- levels(Master$Set)

  if (!is.logical(xwave))
    stop("xwave is not TRUE/FALSE")
  if (!is.character(set))
    stop("set is not a string")
  if (!set %in% Sets)
    stop("set is not in the master... ")
   if (xwave == F & !wave_n %in% c(1:14))
    stop("wave_n is not in correct form")

  loc <- transpose(read.csv("C:/Users/User/Documents/list.csv")) # list of names...wink wink nudge nudge

  # Picks right data from inputs
  if (xwave == F) {
    print("Reading data ... ")
    loc <- filter(loc, row_number() == wave_n +1)
    loc <- loc[1,1]

    user_subset <- Master %>% filter(Set == set)
    user_subset <-user_subset  %>% transform(Code =sprintf("n%s",Code))
    Col_Names <-user_subset$Name; Col_Names <-factor(Col_Names) ;user_subset <- user_subset$Code
    df <- feather::read_feather(paste0(loc))
    df_subset <- subset(df, select = user_subset)
    colnames(df_subset) <- Col_Names; colnames(df_subset)
    your_data <- tbl_df(df_subset)

    year_sets <-c("1" = "2000-01",
                  "2" = "2001-02",
                  "3" = "2002-03",
                  "4" = "2003-04",
                  "5" = "2004-05",
                  "6" = "2005-06",
                  "7" = "2006-07",
                  "8" = "2007-08",
                  "9" = "2008-09",
                  "10" = "2009-10",
                  "11" = "2010-11",
                  "12" = "2011-12",
                  "13" = "2012-13",
                  "14" = "2013-14")
    your_data <- mutate(your_data, Wave_year = wave_n)

    your_data$Wave_year <- year_sets[your_data$Wave_year]

    assign(paste0(set,"_tbl"),your_data,  envir = .GlobalEnv)

  }

  # Loop for reading data, also writes vector of data set names: Hdata_sets ==============================================
  if (xwave == T) {

    loc <- loc[-1,]

              i <- 1;Hdata_sets <- c(Characters=character())
            print("Reading data ... Please wait")
            for (n in loc) {
              print(length(loc)-i)

              df <- feather::read_feather(n)
              assign(sprintf("%s%02d","WAVE",i), df)

              temp <-c(sprintf("%s%02d","WAVE",i))
              Hdata_sets <- append(Hdata_sets,temp)
              rm(df)
              i <- i + 1

            }

            # Loop generating reduced set data frames: Wave1   ====================================================================================
            Reduced_sets <- c(Characters=character());i <- 1; f <- factor(letters); hol <- select(Master, 8:21); Master_waves <- colnames(hol)
            print("Constraining data")

            for (n in Hdata_sets) {
              print(length(Hdata_sets)-i)

              WAVE <- tbl_df(get(n))

              user_subset <- Master %>% filter(Set == set) %>%
                filter_(paste0("is.na(",Master_waves[i],") == F"))
              user_subset <<- user_subset %>% transform(Code = sprintf("%s%s",f[[i]] , Code))
              Col_Names <-user_subset$Name
              Col_Names <- as.character(Col_Names)
              Col_Names <-factor(Col_Names) ;user_subset <- user_subset$Code
              user_subset <- as.character(user_subset)
              # print(user_subset)
              WAVE<- subset(WAVE, select = user_subset)
              colnames(WAVE) <- Col_Names

              # bring in NA filled variable for non existant variables in that wave
              missing_subset <- Master %>% filter(Set == set) %>%
                filter_(paste0("is.na(",Master_waves[i],") == T"))
              namevector <- as.character(missing_subset$Name)
              WAVE[,namevector] <- NA
              WAVE <- WAVE %>%  mutate(Wave_n = paste(n))
              assign(sprintf("%s%02d","RedWAVE",i), WAVE)

              temp <-c(sprintf("%s%02d","RedWAVE",i))
              Reduced_sets <- append(Reduced_sets,temp)

              i <- i + 1
            }
            print("Building...")

            for (n in Reduced_sets) {
              get(n)[ , order(names(get(n)))]
            }

            xwave <- bind_rows(RedWAVE01,
                               RedWAVE02,
                               RedWAVE03,
                               RedWAVE04,
                               RedWAVE05,
                               RedWAVE06,
                               RedWAVE07,
                               RedWAVE08,
                               RedWAVE09,
                               RedWAVE10,
                               RedWAVE11,
                               RedWAVE12,
                               RedWAVE13,
                               RedWAVE14)

            xwave <- mutate(xwave, Wave_year = Wave_n)
            year_sets <-c("WAVE01" = "2000-01",
                          "WAVE02" = "2001-02",
                          "WAVE03" = "2002-03",
                          "WAVE04" = "2003-04",
                          "WAVE05" = "2004-05",
                          "WAVE06" = "2005-06",
                          "WAVE07" = "2006-07",
                          "WAVE08" = "2007-08",
                          "WAVE09" = "2008-09",
                          "WAVE10" = "2009-10",
                          "WAVE11" = "2010-11",
                          "WAVE12" = "2011-12",
                          "WAVE13" = "2012-13",
                          "WAVE14" = "2013-14")

            xwave$Wave_year <- year_sets[xwave$Wave_year]
            xwave <<- xwave
            xwave_samp <<- xwave[sample(nrow(xwave), 3000), ]
  }

  print(paste0("Done. See ",set,"_tbl or xwave")
  )
}
