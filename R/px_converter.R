
#' Convert data frame to px file
#'
#' @param data an object (dataframe)
#' @param var_names disaggregation variables
#' @param var_totals character for totals in all disaggregates.
#' @param var_display_names variable names to replace the disaggregation variables.
#' @param weight_var weight variable (must be numeric).
#' @param title title of the statistical table being generated.
#' @param matrix summary of the table or source.
#' @param u_of_m unit of measure.
#' @param subject_code subject code.
#' @param subject_area subject area.
#' @param content similar to title.
#' @param showdecimals numeric value showing the decimal place.
#' @param decimals numeric value showing the decimal place.
#' @param language en.
#' @param source source of data eg. GDHS and GLSS.
#' @param note notes.
#' @param save_location the directory to save the returned output.
#' @param min_cells minimum frequency for suppression (default is 5)
#' @param summary_type type of summary to make. Only accepts:
#' \itemize{
#'   \item **freq:** frequencies or counts
#'   \item **rate:** computes rates for dimensions.
#' }
#' @param numericVar a numeric or character vector defining the column indices or variable names of additional numeric variables with respect to data.
#'
#' @return px file
#' @export
process_data <- function(data, var_names, var_totals,var_display_names, weight_var, title, matrix,
                         u_of_m ="percentage of population",
                         subject_code, subject_area,
                         content,
                         showdecimals,
                         decimals,
                         language,
                         source,
                         note,
                         save_location,
                         min_cells = 5,
                         summary_type = "freq", # freq, numSum, prop, single_rate
                         rate_element = NULL,
                         numericVar = NULL
) {


  my_tibs <- tibble(var_names,var_display_names, var_totals)

  print("using these variables")
  print(my_tibs)

  # Extracting variables and creating hierarchies
  data_selected <- data %>% select(!!!syms(var_names), all_of({{ weight_var }}))
  data_selected <- data_selected %>% sjlabelled::as_label()

  hier_list <- lapply(1:length(var_names), function(i) {
    var <- var_names[i]
    total <- var_totals[i]
    hier_create(root = total, nodes = levels(data_selected[[var]]))
  })
  names(hier_list) <- var_names

  prob <- makeProblem(data_selected, dimList = hier_list, sampWeightInd = weight_var, numVarInd = numericVar)

  # Calculating percentages
  percentages <- sdcProb2df(prob, addNumVars  = ifelse(is.null(numericVar), FALSE, TRUE))

  for (i in 1:length(var_names)) {
    var_name <- var_names[i]
    var_display_name <- var_display_names[i]
    label_var <- paste0(var_name, "_o")
    percentages <- percentages %>%
      mutate(!!var_display_name := factor(as.numeric(.data[[var_name]]), labels = unique(.data[[label_var]])))
  }

  group_vars <- var_display_names[-1]  # Excludes the first variable from grouping

  if (summary_type == "rate") {
    percentages <- percentages %>%
      select(!!!syms(var_display_names), value = freq) %>%
      group_by(across(all_of(group_vars))) %>%
      mutate(value = value / value[.data[[var_display_names[1]]] == var_totals[1]]) %>%
      mutate(value = round(value * 100, 1)) %>%
      ungroup() %>%
      filter(!!rlang::sym(var_display_names[1]) %in% c(rate_element)) %>%
      select(!!!syms(group_vars), value)
  }else if(summary_type == "percent"){
    percentages <- percentages %>%
      select(!!!syms(var_display_names), value = freq) %>%
      group_by(across(all_of(group_vars))) %>%
      mutate(value = value / value[.data[[var_display_names[1]]] == var_totals[1]]) %>%
      mutate(value = round(value * 100, 1)) %>%
      ungroup()
  }else if(summary_type == "numSum"){
    percentages <- percentages %>%
      select(!!!syms(var_display_names), value = !!numericVar) %>%
      group_by(across(all_of(group_vars))) %>%
      summarize(value = sum(value)) %>%
      mutate(value = round(value, 1)) %>%
      ungroup()
  }else{
    percentages <- percentages %>%
      select(!!!syms(var_display_names), value = freq) %>%
      mutate(value = round(value, 0)) %>%
      ungroup()
  }





  #### Remove cells that are based on too small values

  prob_dummy <- makeProblem(data_selected, dimList = hier_list, numVarInd = numericVar)

  prob_dummy <- primarySuppression(prob_dummy, type = "freq", maxN = min_cells)

  percentages_dummy <- sdcProb2df(prob, addNumVars  = ifelse(is.null(numericVar), FALSE, TRUE))

  for (i in 1:length(var_names)) {
    var_name <- var_names[i]
    var_display_name <- var_display_names[i]
    label_var <- paste0(var_name, "_o")
    percentages_dummy <- percentages_dummy %>%
      mutate(!!var_display_name := factor(as.numeric(.data[[var_name]]), labels = unique(.data[[label_var]])))
  }

  percentages_dummy <- percentages_dummy %>%
    filter(sdcStatus  == "s") %>%
    select(!!!syms(var_display_names), n = freq)

  if(summary_type == "rate"){
    percentages <- percentages
  } else{
    percentages <- percentages %>% left_join(percentages_dummy,  by = var_display_names) %>%
      mutate(value = ifelse(is.na(n), NA, value)) %>%
      select(-n)
  }

  # Creating px table
  #### creating px table
  px_table <- as.px.data.frame(percentages,
                               list.keys = list(TITLE = title,
                                                MATRIX = matrix,
                                                # units used
                                                UNITS =u_of_m,
                                                ## subject code. Note the name is in apostrophes as R doesn't accept '-' in variable names.
                                                ## The apostrophes however define that it's a variable name.
                                                'SUBJECT-CODE' = subject_code,
                                                #Subject area
                                                'SUBJECT-AREA' = subject_area,
                                                # contents
                                                CONTENTS = content,
                                                SHOWDECIMALS = showdecimals,
                                                DECIMALS = decimals,
                                                LANGUAGE=language,
                                                #`HIERARCHIES("x")` = x_px,
                                                SOURCE =source,
                                                NOTEX = note
                               ))


  elimination_vars <- var_totals[-1]  # Excludes the first variable total from ELIMINATION
  px_table$ELIMINATION <- setNames(elimination_vars, var_display_names[-1])
  px_table$CODES <- setNames(lapply(var_names, function(var) levels(percentages[[var_display_names[which(var_names == var)]]])), var_display_names)

  write.px(px_table, save_location)

  print("done processing")
  print("Saved data by assigning to an object.")
  return(percentages)

}
