
#' Convert Processed Data into PX File
#'
#' @param data Processed data that should be turned into a PX file
#' @param title Title of the table
#' @param matrix Matrix?
#' @param units Unit of values in the table
#' @param contents Content of the table
#' @param source Source of the data
#' @param path Path and name under which the px file will be saved (e.g. output/table1.px)
#' @param note Notes for underneath the table
#' @param hier_variables Vector of variable names for which a hierarchy should be included
#' @param hierarchies List of hierarchies of previously listed variables (in same order)
#' @param showdecimals Number of decimals to be shown on the StatsBank
#' @param decimals Number of decimals to be seen when data is downloaded from the StatsBank
#' @param ... Additional variables used to make px file
#'
#' @return pxfile
#' @export
px_converter <- function(data, title, matrix, units,
                         contents, source,
                         save_location,
                         note = NULL,
                         hier_variables = NULL,
                         hierarchies = NULL,
                         showdecimals = 0, decimals = 0,

                         ...
) {

  #### creating px table
  px_table <- as.px.data.frame(data,
                               list.keys = list(TITLE = title,
                                                MATRIX = matrix,
                                                UNITS = units,
                                                CONTENTS = contents,
                                                SHOWDECIMALS = showdecimals,
                                                DECIMALS = decimals,
                                                SOURCE = source,
                                                NOTEX = note,
                                                LANGUAGE = "en",
                                                ...))

  # Set elimination except for first variable (and value)
  px_table$ELIMINATION <- sapply(data, function(x) if(is.factor(x)) levels(x)[1] else NA)[1:(ncol(data)-2)]
  px_table$CODES <-  sapply(data, levels)[-ncol(data)]

  ### Add hierarchies
  if(length(hier_variables) > 0){
    for(i in 1:length(hier_variables)){
      # Create into px type hierarchie_product
      hier_px <- hierarchies[[i]] %>%
        #Making the variable test where levels are divided by:
        mutate(test=if_else(level==1, unclass(root), paste(unclass(root),unclass(leaf), sep = "\":\"" ))) %>%
        ## Saves the variable as a vector, so just a long text string
        pull(test)

      # Add hierarchy to px file
      px_table$HIERARCHIES[[hier_variables[i]]] <- hier_px
    }
  }

  # Save px file
  write.px(px_table, save_location)

  return(px_table)

  print(paste("Saved data in", save_location))
}
