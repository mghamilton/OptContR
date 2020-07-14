#' @name ped_example
#' @docType data
#' @keywords datasets
#' @title ped_example data for small worked example in Hamilton (2020) Optimal  contributions in highly fecund species with overlapping generations
#' @description A data frame defining sex-age groups
#' @usage ped_example
#' @format Data frame
#' @author Matthew Hamilton <matthewhamilton75@gmail.com>
#' @source Hamilton (2020) Optimal  contributions in highly fecund species with overlapping generations
#' @references Hamilton (2020) Optimal  contributions in highly fecund species with overlapping generations

ped_example <- data.frame(id = as.character(1:65),
                                         sire = as.character(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 8, 19, 19, 10, 20, 22, 8, 8, 10, 4, 1, 24, 20, 25, 20, 22, 22, 19, 19, 2, 31, 31, 31, 33, 24, 24, 24, 22, 19, 40, 41, 40, 41, 31, 31, 33, 49, 49, 49, 31, 49, 31, 49, 31, 49, 31)),
                                         dam = as.character(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 7, 5, 6, 9, 21, 11, 12, 23, 14, 13, 15, 26, 30, 27, 28, 29, 18, 17, 29, 16, 38, 35, 32, 36, 34, 37, 36, 23, 39, 46, 43, 45, 42, 44, 47, 48, 51, 52, 53, 54, 48, 53, 53, 54, 48, 53)),
                                         ebv = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 11.1, 13.5, 11.4, 12.7, NA, NA, 11.6, 11.6, 12, 12.7, 12.1, 12.9, 12.2, 13, NA, NA, NA, NA),
                                         selection_round = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 2, 2, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10),
                                         sex = c("male", "male", "female", "male", "female", "female", "female", "male", "female", "male", "female", "female", "female", "female", "female", "female", "female", "female", "male", "male", "female", "male", "female", "male", "male", "female", "female", "female", "female", "female", "male", "female", "male", "female", "female", "female", "female", "female", "female", "male", "male", "female", "female", "female", "female", "female", "female", "female", "male", "female", "male", "female", "female", "female", "male", "female", "male", "female", "female", "male", "male", "female", "female", "male", "male"))

usethis::use_data(ped_example, overwrite = TRUE)

