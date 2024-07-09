
#' Set right format for px file titler
#'
#' @param s A string as title of the px file.
#'
#' @return string
#' @import stringr
#' @export

toTitleCase <- function(s) {
  # Convert the string to title case
  titleCase <- str_to_title(s)

  # List of words to replace back to lowercase (if not at the beginning)
  lowercaseWords <- c("A", "An", "And", "As", "At", "But", "By", "For", "In", "Nor", "Of", "On", "Or", "The", "Up")

  # Function to replace words with their lowercase versions
  replaceToLower <- function(word) {
    # Replace word with lowercase version if it's not at the beginning of the string
    fixedWord <- tolower(word)
    pattern <- paste0("(?<!^)\\b", word, "\\b")
    str_replace_all(titleCase, pattern, fixedWord)
  }

  # Apply the function to each word in the list
  for (word in lowercaseWords) {
    titleCase <- replaceToLower(word)
  }

  return(titleCase)
}
