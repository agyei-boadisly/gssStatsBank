#' Create Sub-Hierarchies
#'
#' This function creates a hierarchical structure using the `sdcHierarchies` package. It dynamically adds child nodes for specified parent nodes based on some specified variables and object.
#'
#' @param data A data frame containing the data to be used for creating the hierarchy.
#' @param var_total A string specifying the root node for the hierarchy.
#' @param var_main A string specifying the column name in `data` containing the main categories (roots).
#' @param sub_var A string specifying the column name in `data` containing the sub-categories (children).
#' @param main_groups A vector specifying the categories under `var_main` for which child levels should be created.
#'
#' @return A hierarchical structure with the specified root and child nodes.
#' @export

# Function to create sub-hierarchy
make_sub_hier <- function(data,
                          var_total,  # String for root hier_create
                          var_main,   # Colname containing the roots
                          sub_var,    # Colname containing the child
                          main_groups # Specify the categories under var_main to create child levels for
                          ) {
  # Ensure column names are provided as strings
  var_main <- sym(var_main)
  sub_var <- sym(sub_var)

  #select variables
  data <- data %>% select({{var_main}}, {{sub_var}})

  # Create Root Hierarchy
  make_hier <- hier_create(root = var_total, nodes = unique(data %>% pull(!!var_main)))

  # Function to add child levels dynamically
  add_children <- function(parent_node) {
    child_nodes <- data %>%
      filter(!!var_main == parent_node) %>%
      pull(!!sub_var) %>%
      unique() %>%
      as.character()

    if (length(child_nodes) > 0) {
      make_hier <<- hier_add(make_hier, root = parent_node, nodes = child_nodes)
    }
  }

  # Dynamically adding child levels based on specified main_groups
  walk(main_groups, add_children)

  return(make_hier)
}
