#' Clean Age Variable of Data Create Age Hierarchy
#'
#' @param data Data with the age variable that you want to clean and create the hierarchy for
#' @param age_var Variable name of the age variable (e.g. "age")
#' @param min_age Minimum age the data should start at
#' @param max_age Maximum age the data should stop at
#' @param max_age_type cutoff OR max: A cutoff labels values above the maximum as 'max+' (e.g., 80+), while a max drops all the values above the max value.
#' @param age_group TRUE or FALSE, whether age should be replaced by groups, or single ages should be preserved
#' @param print_hier TRUE or FALSE, whether the resulting hierarchy should be printed
#'
#' @return List with cleaned data and age hierarchy
#' @export
#'
clean_age_data <- function(data, age_var = "age", min_age = 0, max_age = 100,
                           max_age_type = "cutoff", age_group = FALSE,
                           print_hier = FALSE) {

  ### Checks
  {
    # Do not allow cutoff below 65, only max can be below 65
    if(max_age_type == "cutoff" & max_age < 65){
      stop("Error: Cutoff age cannot be below 65, either increase cutoff age or change the max_age_type to max to drop
          all ages below this value.")
    }

    # Do not allow max age to be below min age
    if(max_age < min_age){
      stop("Error: Max age cannot be lower than min age, change the values.")
    }
  }

  ### PART 1: Data cleaning and mutating
  {
    # Convert the age variable to numeric (if not already)
    data <- data %>%
      mutate(original_age = !!sym(age_var),
             age = as.numeric(original_age))

    # 1. Drop rows with age below the min_age
    data <- data %>%
      filter(age >= min_age)

    # 2. Handle max_age based on max_age_type
    {
      if (max_age_type == "cutoff") {
        # Change all ages >= max_age to "max_age+"
        data <- data %>%
          mutate(!!age_var := if_else(age >= max_age, paste0(max_age, "+"), as.character(age)))

      } else if (max_age_type == "max") {
        # Drop rows with age above max_age
        data <- data %>%
          filter(age <= max_age)
      }
      }

    # 3. Handle single_age vs age grouping:
    # First create age groups
    # Create age groups - age_labels_final
    {
      # Define the default age groups
      age_breaks <- seq(0, 100, by = 5)
      age_labels <- paste0(age_breaks[-length(age_breaks)], "-", age_breaks[-1] - 1)

      # Create new first group
      new_first_group <- ifelse(min_age == age_breaks[min_age < age_breaks][1]-1,
                                min_age,
                                paste0(min_age, "-", age_breaks[min_age < age_breaks][1]-1))
      # Drop age groups below min age
      age_labels_bottomdrop <- as.vector(na.omit(c(new_first_group,
                                                   age_labels[min_age < age_breaks])))
      # Extract the highest numbers after the hyphen to create new age breaks
      new_age_breaks <- as.numeric(gsub(".*-(\\d+)", "\\1", age_labels_bottomdrop))

      # Now adjust the top groups based on max_age
      if(max_age_type == "max"){
        # Create new max group
        new_max_group <- ifelse(new_age_breaks[max_age < new_age_breaks][1]-4 == max_age,
                                max_age,
                                paste0(new_age_breaks[max_age-1 < new_age_breaks][1]-4, "-", max_age))
        # Drop age groups above max age
        age_labels_final <- as.vector(na.omit(c(age_labels_bottomdrop[max_age > new_age_breaks],
                                                new_max_group)))

      }else if(max_age_type == "cutoff"){
        # Create new max group
        new_max_group <- ifelse(new_age_breaks[max_age < new_age_breaks][1]-4 == max_age,
                                NA,
                                paste0(new_age_breaks[max_age-1 < new_age_breaks][1]-4, "-", max_age-1))

        # Drop age groups above max age
        age_labels_final <- as.vector(na.omit(c(age_labels_bottomdrop[max_age > new_age_breaks],
                                                new_max_group,
                                                paste0(max_age, "+"))))

      }
    }

    # Get new age breaks
    {
      # Define the breaks for the age groups
      get_age_breaks <- function(labels) {
        breaks <- c()
        for (label in labels) {
          if (grepl("\\+", label)) {
            # For labels with "+", just add a large number as the upper bound
            upper_bound <- Inf
          } else {
            # Extract the upper bound of the range
            upper_bound <- as.numeric(sub(".*-(\\d+)", "\\1", label))
          }
          # Add breaks for the lower and upper bounds
          lower_bound <- min_age
          breaks <- c(breaks, lower_bound, upper_bound + 1)
        }
        # Remove duplicate breaks and return the unique sorted values
        unique(sort(breaks))
      }

      # Final breaks
      final_breaks <- get_age_breaks(age_labels_final)
    }

    # Then only for age groups we create new group varibale
    if (age_group) {

      # Define the function to get age groups
      assign_age_group <- function(age, breaks, labels) {

        # Cut ages into age groups
        cut(age, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)
      }

      # Create age groups in the data
      # Use mutate to create age group column
      data <- data |>
        mutate(age_single = as.numeric(str_replace_all(age, "\\+", ""))) |>
        mutate(age = assign_age_group(age_single,
                                      final_breaks,
                                      age_labels_final))
    }

  }

  ### PART 2: Create hierarchy aligned with data
  {
    # Define the function to create the age hierarchy
    create_age_hierarchy <- function(min_age, max_age, max_age_type,
                                     age_labels_final,
                                     age_group) {

      # Initialize the base node
      if (min_age == 0 & max_age_type == "cutoff") {
        base_node <- "All ages"
      } else if (min_age != 0 & max_age_type == "cutoff") {
        base_node <- paste0("All ages (", min_age, " years and older)")
      } else if (min_age == 0 & max_age_type == "max") {
        base_node <- paste0("All ages (", max_age, " years and younger)")
      } else if (min_age != 0 & max_age_type == "max") {
        base_node <- paste0("All ages (", min_age, "-", max_age, " years old)")
      }

      # Create the hierarchy with the base node
      hier_age <- sdcHierarchies::hier_create(root = base_node)

      # Function to add age groups based on the conditions
      add_age_groups <- function(hier_age, min_age, max_age, max_age_type) {
        if (min_age == 0 & max_age_type == "cutoff") {
          hier_age <- sdcHierarchies::hier_add(hier_age,
                                               root = "All ages",
                                               nodes = c("0-14 years",
                                                         "15-64 years",
                                                         "65 years and older"))
        } else if (min_age != 0 & max_age_type == "cutoff") {
          if (min_age <= 14) {
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = paste0("All ages (", min_age, " years and older)"),
                                                 nodes = c(paste0(min_age, "-14 years"),
                                                           "15-64 years",
                                                           "65 years and older"))

            # Set min ages of all groups
            lowages <- as.numeric(gsub("-.*", "", substr(age_labels_final, 1, 3)))
            # Add sub-groups
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = c(paste0(min_age, "-14 years")),
                                                 nodes = age_labels_final[lowages < 15])
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = "15-64 years",
                                                 nodes = age_labels_final[lowages >= 15 &
                                                                            lowages < 65])
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = "65 years and older",
                                                 nodes = age_labels_final[lowages >= 65])
          } else if (min_age > 14 & min_age <= 64) {
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = paste0("All ages (", min_age, " years and older)"),
                                                 nodes = c(paste0(min_age, "-64 years"),
                                                           "65 years and older"))
            # Set min ages of all groups
            lowages <- as.numeric(gsub("-.*", "", substr(age_labels_final, 1, 3)))
            # Add sub-groups
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = paste0(min_age, "-64 years"),
                                                 nodes = age_labels_final[lowages >= min_age &
                                                                            lowages < 65])
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = "65 years and older",
                                                 nodes = age_labels_final[lowages >= 65])
          } else {
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = paste0("All ages (", min_age, " years and older)"),
                                                 nodes = c(paste0(min_age, " years and older")))

            # Add sub-groups
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = paste0(min_age, " years and older"),
                                                 nodes = age_labels_final)
          }
        } else if (min_age == 0 & max_age_type == "max") {
          if (max_age <= 14) {
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = paste0("All ages (", max_age, " years and younger)"),
                                                 nodes = c(paste0("0-", max_age, " years")))

            # Add sub-groups
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = c(paste0("0-", max_age, " years")),
                                                 nodes = age_labels_final)
          } else if (max_age > 14 & max_age <= 64) {
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = paste0("All ages (", max_age, " years and younger)"),
                                                 nodes = c("0-14 years",
                                                           paste0("15-", max_age, " years")))

            # Set min ages of all groups
            lowages <- as.numeric(gsub("-.*", "", substr(age_labels_final, 1, 3)))
            # Add sub-groups
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = "0-14 years",
                                                 nodes = age_labels_final[lowages < 15])
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = paste0("15-", max_age, " years"),
                                                 nodes = age_labels_final[lowages >= 15])
          } else {
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = paste0("All ages (", max_age, " years and younger)"),
                                                 nodes = c("0-14 years",
                                                           "15-64 years",
                                                           paste0("65-", max_age, " years")))

            # Set min ages of all groups
            lowages <- as.numeric(gsub("-.*", "", substr(age_labels_final, 1, 3)))
            # Add sub-groups
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = "0-14 years",
                                                 nodes = age_labels_final[lowages < 15])
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = "15-64 years",
                                                 nodes = age_labels_final[lowages >= 15 &
                                                                            lowages < 65])
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = paste0("65-", max_age, " years"),
                                                 nodes = age_labels_final[lowages >= 65])
          }
        } else if (min_age != 0 & max_age_type == "max") {
          if (min_age <= 14) {
            if (max_age <= 14) {
              hier_age <- sdcHierarchies::hier_add(hier_age,
                                                   root = paste0("All ages (", min_age, "-", max_age, " years old)"),
                                                   nodes = c(paste0(min_age, "-14 years")))

              # Set min ages of all groups
              lowages <- as.numeric(gsub("-.*", "", substr(age_labels_final, 1, 3)))
              # Add sub-groups
              hier_age <- sdcHierarchies::hier_add(hier_age,
                                                   root = paste0(min_age, "-14 years"),
                                                   nodes = age_labels_final)
            }else if (max_age > 14 & max_age <= 64) {
              hier_age <- sdcHierarchies::hier_add(hier_age,
                                                   root = paste0("All ages (", min_age, "-", max_age, " years old)"),
                                                   nodes = c(paste0(min_age, "-14 years"),
                                                             paste0("15-", max_age, " years")))

              # Set min ages of all groups
              lowages <- as.numeric(gsub("-.*", "", substr(age_labels_final, 1, 3)))
              # Add sub-groups
              hier_age <- sdcHierarchies::hier_add(hier_age,
                                                   root = paste0(min_age, "-14 years"),
                                                   nodes = age_labels_final[lowages < 15])
              hier_age <- sdcHierarchies::hier_add(hier_age,
                                                   root = paste0("15-", max_age, " years"),
                                                   nodes = age_labels_final[lowages >= 15 &
                                                                              lowages < 65])
            } else {
              hier_age <- sdcHierarchies::hier_add(hier_age,
                                                   root = paste0("All ages (", min_age, "-", max_age, " years old)"),
                                                   nodes = c(paste0(min_age, "-14 years"),
                                                             "15-64 years",
                                                             paste0("65-", max_age, " years")))

              # Set min ages of all groups
              lowages <- as.numeric(gsub("-.*", "", substr(age_labels_final, 1, 3)))
              # Add sub-groups
              hier_age <- sdcHierarchies::hier_add(hier_age,
                                                   root = paste0(min_age, "-14 years"),
                                                   nodes = age_labels_final[lowages < 15])
              hier_age <- sdcHierarchies::hier_add(hier_age,
                                                   root = "15-64 years",
                                                   nodes = age_labels_final[lowages >= 15 &
                                                                              lowages < 65])
              hier_age <- sdcHierarchies::hier_add(hier_age,
                                                   root = paste0("65-", max_age, " years"),
                                                   nodes = age_labels_final[lowages >= 65])
            }
          } else if (min_age > 14 & min_age <= 64) {
            if (max_age <= 64) {
              hier_age <- sdcHierarchies::hier_add(hier_age,
                                                   root = paste0("All ages (", min_age, "-", max_age, " years old)"),
                                                   nodes = c(paste0(min_age, "-", max_age, " years")))

              # Set min ages of all groups
              lowages <- as.numeric(gsub("-.*", "", substr(age_labels_final, 1, 3)))
              # Add sub-groups
              hier_age <- sdcHierarchies::hier_add(hier_age,
                                                   root = "15-64 years",
                                                   nodes = age_labels_final)
            } else {
              hier_age <- sdcHierarchies::hier_add(hier_age,
                                                   root = paste0("All ages (", min_age, "-", max_age, " years old)"),
                                                   nodes = c(paste0(min_age, "-", max_age, " years"),
                                                             "65 years and older"))

              # Set min ages of all groups
              lowages <- as.numeric(gsub("-.*", "", substr(age_labels_final, 1, 3)))
              # Add sub-groups
              hier_age <- sdcHierarchies::hier_add(hier_age,
                                                   root = paste0(min_age, "-", max_age, " years"),
                                                   nodes = age_labels_final[lowages >= 15 &
                                                                              lowages < 65])
              hier_age <- sdcHierarchies::hier_add(hier_age,
                                                   root ="65 years and older",
                                                   nodes = age_labels_final[lowages >= 65])
            }
          } else {
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = paste0("All ages (", min_age, "-", max_age, " years old)"),
                                                 nodes = c(paste0(min_age, " years and older")))

            # Add sub-groups
            hier_age <- sdcHierarchies::hier_add(hier_age,
                                                 root = paste0(min_age, " years and older"),
                                                 nodes = age_labels_final)
          }
        }

        return(hier_age)
      }

      # Add age groups based on conditions
      hier_age <- add_age_groups(hier_age, min_age, max_age, max_age_type)
    }

    # Add age groups based on conditions
    clean_age_hier <- create_age_hierarchy(min_age, max_age, max_age_type,
                                           age_labels_final,
                                           age_group)

    # Add single ages if not age_group
    if(!age_group){
      # Select maximum level of age hierarchy
      max_level = max(clean_age_hier$level)

      # Loop over the max level groups
      groups <- clean_age_hier |>
        filter(level == max_level) |>
        pull(leaf)

      for(g in groups){
        # Select single ages in the group
        # If it is the + group, don't add anything, but it needs to have -
        #     otherwise add all values in group
        if(!grepl("\\+", g) & grepl("\\-", g)){

          # Split the values by the hyphen
          split_values <- strsplit(g, "-")

          # Extract the minimum and maximum
          min_values <- sapply(split_values, function(x) as.numeric(x[1]))
          max_values <- sapply(split_values, function(x) as.numeric(x[2]))

          # add single ages
          clean_age_hier <- sdcHierarchies::hier_add(clean_age_hier,
                                                     root = g,
                                                     nodes = as.character(min_values:max_values))
        }
      }
    }

    # Print hierarchical structure (for inspection)
    if(print_hier){
      hier_display(clean_age_hier)
    }


  }

  ### Create list to return
  return_list <- list("data" = data,
                      "hier" = clean_age_hier)
  return(return_list)
}
