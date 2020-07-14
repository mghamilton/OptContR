#' @name group_contribution_example
#' @docType data
#' @keywords datasets
#' @title ped_example data for small worked example in Hamilton (2020) Optimal  contributions in highly fecund species with overlapping generations
#' @description A data frame defining sex-age groups
#' @usage group_contribution_example
#' @format Data frame
#' @author Matthew Hamilton <matthewhamilton75@gmail.com>
#' @source Hamilton (2020) Optimal  contributions in highly fecund species with overlapping generations
#' @references Hamilton (2020) Optimal  contributions in highly fecund species with overlapping generations

group_contribution_example <- data.frame(sex = c("male", "male", "male", "female", "female", "female"),
                                 age = c(3,2,1,3,2,1),
                                 fams_n = c(1,2,3,1,2,3))

usethis::use_data(group_contribution_example, overwrite = TRUE)

