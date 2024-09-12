#' Creating GSS Age Hierarchy
#'
#' @param cutoff_age the maximum age to be used in the age groups (e.g. 65, 80, 100)
#' @return Output is a sdc hierarchy of the age groups used by GSS.
#' @export
#'
#' @examples
#' age_hier()
age_hier <- function(cutoff_age = 80){
  # Check if cutoff age is at least 65
  if(cutoff_age < 65){
    stop("cutoff_age has to be at least 65")
  }

  # Check if cutoff_age is divisible by 5
  if (cutoff_age %% 5 != 0) {
    stop("cutoff_age must be divisible by 5")
  }

  # Create base node
  hier_age <- sdcHierarchies::hier_create(root = "All ages")

  # Add main age groups
  hier_age <- sdcHierarchies::hier_add(hier_age,
                                       root = "All ages",
                                       nodes = c("0 years to 14 years",
                                                 "15 years to 64 years",
                                                 "65 years and older"))

  # Add 0-14 5-year age groups
  hier_age <- sdcHierarchies::hier_add(hier_age,
                                       root = "0 years to 14 years",
                                       nodes = c("0-4",
                                                 "5-9",
                                                 "10-14"))

  # Add single ages
  hier_age <- sdcHierarchies::hier_add(hier_age, root = "0-4", nodes = as.character(0:4))
  hier_age <- sdcHierarchies::hier_add(hier_age, root = "5-9", nodes = as.character(5:9))
  hier_age <- sdcHierarchies::hier_add(hier_age, root = "10-14", nodes = as.character(10:14))

  # Add 15-64 5-year age groups
  hier_age <- sdcHierarchies::hier_add(hier_age,
                                       root = "15 years to 64 years",
                                       nodes = c("15-19",
                                                 "20-24",
                                                 "25-29",
                                                 "30-34",
                                                 "35-39",
                                                 "40-44",
                                                 "45-49",
                                                 "50-54",
                                                 "55-59",
                                                 "60-64"))

  # Add single ages
  hier_age <- sdcHierarchies::hier_add(hier_age, root = "15-19", nodes = as.character(15:19))
  hier_age <- sdcHierarchies::hier_add(hier_age, root = "20-24", nodes = as.character(20:24))
  hier_age <- sdcHierarchies::hier_add(hier_age, root = "25-29", nodes = as.character(25:29))
  hier_age <- sdcHierarchies::hier_add(hier_age, root = "30-34", nodes = as.character(30:34))
  hier_age <- sdcHierarchies::hier_add(hier_age, root = "35-39", nodes = as.character(35:39))
  hier_age <- sdcHierarchies::hier_add(hier_age, root = "40-44", nodes = as.character(40:44))
  hier_age <- sdcHierarchies::hier_add(hier_age, root = "45-49", nodes = as.character(45:49))
  hier_age <- sdcHierarchies::hier_add(hier_age, root = "50-54", nodes = as.character(50:54))
  hier_age <- sdcHierarchies::hier_add(hier_age, root = "55-59", nodes = as.character(55:59))
  hier_age <- sdcHierarchies::hier_add(hier_age, root = "60-64", nodes = as.character(60:64))

  # Add 65+ 5-year age groups up to the cutoff_age
  if (cutoff_age == 65) {
    # If cutoff_age is 65, only create "65+"
    group_labels <- paste0(cutoff_age, "+")

    hier_age <- sdcHierarchies::hier_add(hier_age,
                                         root = "65 years and older",
                                         nodes = group_labels)
  } else {
    # If cutoff_age > 65, create 5-year groups and the final age group as "cutoff_age+"
    groups <- seq(65, cutoff_age - 5, by = 5)
    group_labels <- c(paste(groups, groups + 4, sep = "-"), paste0(cutoff_age, "+"))

    hier_age <- sdcHierarchies::hier_add(hier_age,
                                         root = "65 years and older",
                                         nodes = group_labels)

    # Add single ages within 5-year groups
    for(i in seq_along(groups)) {
      single_ages <- as.character(groups[i]:(groups[i] + 4))

      # Add single ages
      hier_age <- sdcHierarchies::hier_add(hier_age, root = group_labels[i], nodes = single_ages)
    }

  }

  return(hier_age)

}
