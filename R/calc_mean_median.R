# Function to calculate median age for a given combination
#' Compute the mean and meadian age of the data
#'
#' @param combination variabes names to consider in the data frame.
#' @param data an object (dataframe).
#' @param variable focus variable a numeric vector.
#' @param output_var output variable.
#' @param weight_var weight variable (must be numeric).
#'
#' @return data frame
#' @import dplyr

calc_mean <- function(combination, data, variable, output_var, weight_var) {
  if (!(variable %in% names(data))) {
    stop(paste("Variable", variable, "not found in the data"))
  }
  # Filter data for the current combination
  sub_data <- data
  for (col in names(disaggregations)) {
    if (!col %in% names(data)) {
      stop(paste("Column", col, "not found in the data"))
    }
    if (combination[[col]] != "overall") {
      sub_data <- sub_data %>% filter((.data[[col]] == combination[[col]]))
    }
  }


  mean_age <- sub_data %>%
    summarise(weight_mean := weighted.mean(x = .data[[variable]],.data[[weight_var]])) %>%
    pull(weight_mean)

  # Return a tibble with the combination and calculated median age
  return(tibble(combination, output_var := mean_age) %>% rename({{output_var}} := output_var))
}



# Function to calculate median age for a given combination
calc_median<- function(combination, data, variable, output_var, weight_var) {
  if (!(variable %in% names(data))) {
    stop(paste("Variable", variable, "not found in the data"))
  }
  # Filter data for the current combination
  sub_data <- data
  for (col in names(disaggregations)) {
    if (!col %in% names(data)) {
      stop(paste("Column", col, "not found in the data"))
    }
    if (combination[[col]] != "overall") {
      sub_data <- sub_data %>% filter((.data[[col]] == combination[[col]]))
    }
  }

  sub_data <- sub_data %>%
    mutate(weighted_median = matrixStats::weightedMedian(.data[[variable]], wt, na.rm=TRUE)) %>%
    mutate(dummy = ifelse(.data[[variable]]  <weighted_median , 1, 0)) %>%
    mutate(sL = weighted.mean(dummy, .data[[weight_var]], na.rm=TRUE)) %>%
    mutate(dummy = ifelse(.data[[variable]]<=weighted_median , 1, 0)) %>%
    mutate(sU = weighted.mean(dummy, .data[[weight_var]], na.rm=TRUE))  %>%
    mutate(smedian = round((weighted_median+(0.5-sL)/(sU-sL)),2))%>%
    summarise(MEDIANS = round(mean(smedian, na.rm=TRUE), 2)) %>%
    pull(MEDIANS)


  # Return a tibble with the combination and calculated median age
  return(tibble(combination, output_var := sub_data) %>% rename({{output_var}} := output_var))
}
