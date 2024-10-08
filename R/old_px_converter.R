
#' Old function that both processes data and converts it to a px file
#'
#' @param data an object (dataframe)
#' @param var_names disaggregation variables
#' @param var_totals character for totals in all disaggregates.
#' @param var_display_names variable names to replace the disaggregation variables.
#' @param weight_var weight variable (must be numeric).
#' @param save_location the directory to save the returned output.
#' @param min_cells minimum frequency for suppression (default is 5)
#' @param summary_type type of summary to make. Only accepts:
#' \itemize{
#'   \item **freq:** frequencies or counts
#'   \item **percent:** computes percenatages for dimensions.
#'   \item **numFreq:** computes frequency of numeric variable by dimensions. Eg. Number of livestock raised by households/individuals.
#'   \item **numSum:** computes sum of numeric variable by dimensions.
#'   \item **single_rate:** computes rates for dimensions. eg. unemployment rate without the inverse (employment).
#' }
#' @param rate_element focus element to retain under single_rate summary_type
#' @param numericVar a numeric or character vector defining the column indices or variable names of additional numeric variables with respect to data.
#' @param add_hier update the list of hierarchie created by this function with a list of different hierarchies
#' @param ... additional variables for as.px.data.frame
#'
#' @return pxfile
#' @export
old_process_data <- function(data, var_names, var_totals,var_display_names, weight_var, save_location, min_cells = 5, rate_element = NULL, numericVar = NULL,
  add_hier = NULL, summary_type = "freq", # freq, numSum, percent, single_rate
  ...
) {


  my_tibs <- tibble(var_names,var_display_names, var_totals)

  print("using these variables")
  print(my_tibs)

  # Extracting variables and creating hierarchies
  if (!is.null(numericVar)) {
    data_selected <- data %>% select(!!!syms(var_names), {{ weight_var }}, {{ numericVar }})
  } else {
    data_selected <- data %>% select(!!!syms(var_names), {{ weight_var }})
  }
  # data_selected <- data %>% select(!!!syms(var_names), all_of({{ weight_var }}))
  data_selected <- data_selected %>% sjlabelled::as_label()

  hier_list <- lapply(1:length(var_names), function(i) {
    var <- var_names[i]
    total <- var_totals[i]
    hier_create(root = total, nodes = levels(data_selected[[var]]))
  })
  names(hier_list) <- var_names

  # adds a list of different hierarchies to the hier_list created in this function
  update_hier_list <- function(hier_name) {
    hier_list[[sym(hier_name)]] <<- add_hier[[sym(hier_name)]]

    main_var_unique <- add_hier[[hier_name]] |> filter(level == 2) |> pull(leaf) |> as.character()
    data_select <- data %>% select_if(negate(is.numeric))
    unique_list <- map(data_select, unique)
    rm(data_select)

    # we need to remove hierarchies for variables that are not needed
    # Identify which elements in unique_list are exactly equal to nm
    matches <- map_lgl(unique_list, ~ identical(sort(.x), sort(main_var_unique)))
    matching_names <- names(unique_list)[matches]

    # we remove hierarchies for variables that are not needed if
    if (length(matching_names) > 0) {
      hier_list <<- hier_list[!names(hier_list) %in% matching_names]

      # remove matching_names from params
      indx <- which(var_names %in% matching_names)
      var_display_names <<- var_display_names[-indx]
      var_names <<- var_names[-indx]
      var_totals <<- var_totals[-indx]
    }

    my_tibs <- tibble(var_names,var_display_names, var_totals)

    print("using these updated variables")
    print(my_tibs)

    return(hier_list)
  }

  # apply the update_hier_list function to the hier_list
  if(!is.null(add_hier)){
    walk(names(add_hier), update_hier_list)
  }


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

  if (summary_type == "single_rate") {
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
  }else if(summary_type == "numFreq"){
    percentages <- percentages %>%
    select(!!!syms(var_display_names), value = !!numericVar) %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(value = round(value, 0)) %>%
    ungroup()
  }else if(summary_type == "numSum"){
    percentages <- percentages %>%
    select(!!!syms(var_display_names), value = !!numericVar) %>%
    group_by(across(all_of(var_display_names))) %>%
    summarize(value = sum(value)) %>%
    mutate(value = round(value, 0)) %>%
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

  var_final <- var_names
  var_display_names_final <- var_display_names

  if(summary_type == "single_rate"){
    var_final <- var_final[-1]
    var_display_names_final <- var_display_names_final[-1]

    percentages_dummy <- percentages_dummy |>
      filter(!!rlang::sym(var_display_names[1]) %in% c(rate_element)) %>%
      select(!!!syms(group_vars), n)
    percentages <- percentages %>% left_join(percentages_dummy,  by = group_vars) %>%
      mutate(value = ifelse(is.na(n), NA, value)) %>%
      select(-n)

  }else{
    percentages <- percentages %>% left_join(percentages_dummy,  by = var_display_names) %>%
    mutate(value = ifelse(is.na(n), NA, value)) %>%
    select(-n)
  }

  # Creating px table
  #### creating px table
  px_table <- as.px.data.frame(percentages, list.keys = list(...))


  elimination_vars <- var_totals[-1]  # Excludes the first variable total from ELIMINATION
  px_table$ELIMINATION <- setNames(elimination_vars, var_display_names[-1])
  px_table$CODES <- setNames(lapply(var_final, function(var) levels(percentages[[var_display_names_final[which(var_final == var)]]])), var_display_names_final)

  write.px(px_table, save_location)

  print("done processing")
  print("Saved data to assigning to an object.")
  return(percentages)

}
