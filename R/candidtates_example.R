#' @name candidates_example
#' @docType data
#' @keywords datasets
#' @title candidates_example data for small worked example in Hamilton (2020) Optimal  contributions in highly fecund species with overlapping generations
#' @description A data frame defining sex-age groups
#' @usage candidates_example
#' @format Data frame
#' @author Matthew Hamilton <matthewhamilton75@gmail.com>
#' @source Hamilton (2020) Optimal  contributions in highly fecund species with overlapping generations
#' @references Hamilton (2020) Optimal  contributions in highly fecund species with overlapping generations

candidates_example <- data.frame(id  = as.character(c(48, 49, 50, 51, 54, 55, 56, 57, 58, 59, 60, 61)),
                                  cprev_fam = c(rep(0,10),1,1),
                                  cmax_fam = c(rep(10,10),1,1),
                                  cmin_fam = c(rep(0,10),1,1),
                                  ebv = c(11.1, 13.5, 11.4, 12.7, 11.6, 11.6, 12, 12.7, 12.1, 12.9, 12.2, 13),
                                  sex = c("female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male"),
                                  age = c(3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1)
                                  )

usethis::use_data(candidates_example, overwrite = TRUE)



