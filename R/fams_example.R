#' @name fams_example
#' @docType data
#' @keywords datasets
#' @title fams_example data for small worked example in Hamilton (2020) Optimal  contributions in highly fecund species with overlapping generations
#' @description A data frame defining sex-age groups
#' @usage fams_example
#' @format Data frame
#' @author Matthew Hamilton <matthewhamilton75@gmail.com>
#' @source Hamilton (2020) Optimal  contributions in highly fecund species with overlapping generations
#' @references Hamilton (2020) Optimal  contributions in highly fecund species with overlapping generations

fams_example <- data.frame(family_id  = c("4_3", "19_5", "19_6", "8_7", "10_9", "20_21", "10_14", "22_11", "8_12", "8_23", "1_15", "20_30", "24_26", "25_27", "4_13", "19_17", "19_29", "2_16", "20_28", "22_18", "22_29", "24_34", "24_37", "31_32", "31_35", "31_38", "33_36", "19_39", "22_23", "24_36", "31_44", "40_45", "40_46", "41_42", "41_43", "31_47", "33_48", "49_51", "49_52", "31_53", "31_54", "49_48", "49_53"),
                           sire = as.character(c(4, 19, 19, 8, 10, 20, 10, 22, 8, 8, 1, 20, 24, 25, 4, 19, 19, 2, 20, 22, 22, 24, 24, 31, 31, 31, 33, 19, 22, 24, 31, 40, 40, 41, 41, 31, 33, 49, 49, 31, 31, 49, 49)),
                           dam = as.character(c(3, 5, 6, 7, 9, 21, 14, 11, 12, 23, 15, 30, 26, 27, 13, 17, 29, 16, 28, 18, 29, 34, 37, 32, 35, 38, 36, 39, 23, 36, 44, 45, 46, 42, 43, 47, 48, 51, 52, 53, 54, 48, 53)),
                           selection_round = c(1, 2, 2, 2, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10, 10 ) )

usethis::use_data(fams_example, overwrite = TRUE)
