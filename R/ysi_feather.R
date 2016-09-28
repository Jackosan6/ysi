#' ysi_feather
#'
#' Converting HILDA SPSS files into Hadleys Feather files
#' @export


# // Cross Wave Feathering //
ysi_feather <- function(){
            install.packages("feather")

            library(haven)
            library(data.table)
            library(ggplot2)
            library(dplyr)
            library(feather)


            ## Load SPSS data file for combined households and individuals from First to 14th Wave

            read_path_list <- rev(list("C:/Users/User/Documents/HILDA/SPSS 140c/Combined n140c.sav",
                                        "C:/Users/User/Documents/HILDA/SPSS 140c/Combined m140c.sav",
                                        "C:/Users/User/Documents/HILDA/SPSS 140c/Combined l140c.sav",
                                        "C:/Users/User/Documents/HILDA/SPSS 140c/Combined k140c.sav",
                                        "C:/Users/User/Documents/HILDA/SPSS 140c/Combined j140c.sav",
                                        "C:/Users/User/Documents/HILDA/SPSS 140c/Combined i140c.sav",
                                        "C:/Users/User/Documents/HILDA/SPSS 140c/Combined h140c.sav",
                                        "C:/Users/User/Documents/HILDA/SPSS 140c/Combined g140c.sav",
                                        "C:/Users/User/Documents/HILDA/SPSS 140c/Combined f140c.sav",
                                        "C:/Users/User/Documents/HILDA/SPSS 140c/Combined e140c.sav",
                                        "C:/Users/User/Documents/HILDA/SPSS 140c/Combined d140c.sav",
                                        "C:/Users/User/Documents/HILDA/SPSS 140c/Combined c140c.sav",
                                        "C:/Users/User/Documents/HILDA/SPSS 140c/Combined b140c.sav",
                                        "C:/Users/User/Documents/HILDA/SPSS 140c/Combined a140c.sav"))


            # Loop for reading SPSS data, also writes vector of data set names: HILDA_sets ==============================================
            i <- 1;HILDA_sets <- c(Characters=character())

            for (n in read_path_list) {
              print(length(read_path_list)-i)

              df <- read_spss(n)
              assign(sprintf("%s%02d","WAVE",i), df)

              temp <-c(sprintf("%s%02d","WAVE",i))
              HILDA_sets <- append(HILDA_sets,temp)
              rm(df)
              i <- i + 1

            }

            ## Write Feather data file for combined households and individuals from First to 14th Wave

            write_path_list <- rev(list("C:/Users/User/Documents/HILDA/Feather sets/Combined n140c.feather",
                                          "C:/Users/User/Documents/HILDA/Feather sets/Combined m140c.feather",
                                          "C:/Users/User/Documents/HILDA/Feather sets/Combined l140c.feather",
                                          "C:/Users/User/Documents/HILDA/Feather sets/Combined k140c.feather",
                                          "C:/Users/User/Documents/HILDA/Feather sets/Combined j140c.feather",
                                          "C:/Users/User/Documents/HILDA/Feather sets/Combined i140c.feather",
                                          "C:/Users/User/Documents/HILDA/Feather sets/Combined h140c.feather",
                                          "C:/Users/User/Documents/HILDA/Feather sets/Combined g140c.feather",
                                          "C:/Users/User/Documents/HILDA/Feather sets/Combined f140c.feather",
                                          "C:/Users/User/Documents/HILDA/Feather sets/Combined e140c.feather",
                                          "C:/Users/User/Documents/HILDA/Feather sets/Combined d140c.feather",
                                          "C:/Users/User/Documents/HILDA/Feather sets/Combined c140c.feather",
                                          "C:/Users/User/Documents/HILDA/Feather sets/Combined b140c.feather",
                                          "C:/Users/User/Documents/HILDA/Feather sets/Combined a140c.feather"))


            # Loop for reading SPSS data, also writes vector of data set names: HILDA_sets ==============================================
            i <- 1

            for (n in HILDA_sets) {
              print(length(HILDA_sets)-i)

              df <- get(n)
              path <- write_path_list[[i]]
              write_feather(df, path)

              rm(df)
              i <- i + 1

            }

}


