#' @problems The problems data frame is created to store information about data issues checked.
#' @description The DataCheck function helps to identify various data issues in the data uploaded by the user. 
#' It iterates over each column of the data and checks for:
#' 1) missing values, 2) symbols, 3) zero values, 4) data range, 5) duplicate records,
#' 6) data format, 7) data completeness, 8) data imbalances, 9) values with commas, and 10) white spaces. 
#' It populates the problems data frame with the name of the variable and the identified data issues. 
#' Finally, it generates a dialogue summarizing the identified problems.

DataCheck <- function(data_list) {
  
  # 1) Problems. ---------------------------------------------------------------
  problems <- data.frame(Variable = character(),
                         Missing_Values = integer(),
                         Proportion_Missing_Values = integer(),
                         Symbols = integer(),
                         Proportion_Zero_Values = integer(),
                         Data_Range = character(),
                         Duplicate_Records = integer(),
                         Data_Format = integer(),
                         Data_Completeness = character(),
                         Data_Imbalances = character(),
                         Values_with_Commas = integer(),
                         White_Spaces = integer(),
                         stringsAsFactors = FALSE)
  
  for (data in data_list) {
    for (col in names(data)) {
      current_col <- data[[col]]
      
      ## 1.1) Check missing values. --------------------------------------------
      missing_values <- if (any(is.na(current_col))) {
        TRUE
      } else {
        FALSE
      }
      
      prop_missing_values <- if (is.numeric(current_col) || is.integer(current_col)) {
        total_values <- length(current_col)
        na_count <- sum(is.na(current_col))
        if (total_values > 0) {
          prop_na <- na_count / total_values * 100
          if (prop_na >= 50) {
            paste0(round(prop_na, 2), "%")
          } else {
            FALSE
          }
        } else {
          FALSE
        }
      } else {
        FALSE
      }
      
      ## 1.2) Check symbols. ---------------------------------------------------
      symbols <- if (is.numeric(current_col) || is.character(current_col)) {
        symbols_count <- sum(grepl("[*\\-]", as.character(current_col)))
        if (symbols_count > 0) {
          TRUE
        } else {
          FALSE
        }
      } else {
        FALSE
      }
      
      ## 1.3) Check zero values. -----------------------------------------------
      zero_values <- if (is.numeric(current_col) || is.integer(current_col)) {
        total_values <- length(current_col)
        zero_count <- sum(current_col == 0, na.rm = TRUE)  # note the na.rm = TRUE
        if (total_values > 0) {
          prop_zero <- zero_count / total_values * 100
          if (!is.na(prop_zero) && prop_zero >= 50) {
            paste0(round(prop_zero, 2), "%")
          } else {
            FALSE
          }
        } else {
          FALSE
        }
      } else {
        FALSE
      }
      
      ## 1.4) Check data range. ------------------------------------------------
      data_range <- if (is.numeric(current_col))
        paste0("(", min(current_col), " - ", max(current_col), ")")
      else
        ""
      
      ## 1.5) Check for duplicate records. -------------------------------------
      duplicate_records <- sum(duplicated(current_col))
      
      ## 1.6) Check for data format. -------------------------------------------
      data_format <- if (is.character(current_col)) {
        if (any(grepl("\\d{4}-\\d{2}-\\d{2}", as.character(current_col)))) {
          # Check for date format (YYYY-MM-DD)
          expected_format <- "\\d{4}-\\d{2}-\\d{2}"
          !all(grepl(expected_format, as.character(current_col)))
        } else if (all(grepl("\\d+", as.character(current_col)))) {
          # Check for age format (numeric values only)
          expected_format <- "\\d+"
          !all(grepl(expected_format, as.character(current_col)))
        } else {
          0
        }
      } else {
        0
      }
      
      ## 1.7) Check data completeness. -----------------------------------------
      data_completeness <- if (length(current_col) > 0)
        paste0(round((1 - (missing_values / length(current_col))) * 100, 2), "%")
      else
        ""
      
      ## 1.8) Check data imbalances. -------------------------------------------
      data_imbalances <- if (is.factor(current_col)) {
        freq_table <- table(current_col, useNA = "ifany")
        imbalance_ratio <- max(freq_table) / length(current_col)
        if (imbalance_ratio > 0.8){
          TRUE
        } else {
          FALSE
        }
      } else {
        FALSE
      }
      
      ## 1.9) Check numeric values with commas. --------------------------------
      values_with_commas <- if (is.character(current_col)) {
        if (sum(grepl(",", as.character(current_col))) > 0) {
          TRUE
        } else {
          FALSE
        }
      } else {
        FALSE
      }
      
      ## 1.10) Check white spaces. ---------------------------------------------
      white_spaces <- if (any(grepl("^\\s+|\\s+$", current_col))) {
        TRUE
      } else {
        FALSE
      }
      
      problems <- rbind(problems, data.frame(Variable = col,
                                             Missing_Values = missing_values,
                                             Proportion_Missing_Values = prop_missing_values,
                                             Symbols = symbols,
                                             Proportion_Zero_Values = zero_values,
                                             Data_Range = data_range,
                                             Duplicate_Records = duplicate_records,
                                             Data_Format = data_format,
                                             Data_Completeness = data_completeness,
                                             Data_Imbalances = data_imbalances,
                                             Values_with_Commas = values_with_commas,
                                             White_Spaces = white_spaces,
                                             stringsAsFactors = FALSE))
    }
  }
  
  # 2) Generate dialogue based on identified problems. #########################
  dialogue <- character()
  
  if (missing_values != FALSE) {
    dialogue <- paste(dialogue, "There are missing values (NA) in the dataset.")
  }
  if (any(problems$Proportion_Missing_Values != FALSE)) {
    dialogue <- paste(dialogue, "More than 50% of values are missing in one or more columns of the dataset.")
  }
  if (any(problems$Symbols != FALSE)) {
    dialogue <- paste(dialogue, "Symbols (* or -) were found in one or more columns of the dataset.")
  }
  if (any(problems$Proportion_Zero_Values != FALSE)) {
    dialogue <- paste(dialogue, "More than 50% of values are zero in one or more columns of the dataset.")
  }
  if (any(problems$Duplicate_Records > 0)) {
    dialogue <- paste(dialogue, "Duplicate records were found in one or more columns of the dataset.")
  }
  if (any(problems$Data_Format > 0)) {
    dialogue <- paste(dialogue, "Data format issues were found in one or more columns of the dataset.")
  }
  if (any(problems$Data_Completeness < 100)) {
    dialogue <- paste(dialogue, "Data completeness issues were found in one or more columns of the dataset.")
  }
  if (any(problems$Data_Imbalances != FALSE)) {
    dialogue <- paste(dialogue, "Data imbalances were found in one or more columns of the dataset.")  # Add specific details about the imbalances
  }
  if (any(problems$Values_with_Commas != FALSE)) {
    dialogue <- paste(dialogue, "Data values with commas were found in one or more columns of the dataset.")
  }
  if (any(problems$White_Spaces != FALSE)) {
    dialogue <- paste(dialogue, "White spaces were found in one or more columns of the dataset.")
  }
  if (length(dialogue) == 0) {
    dialogue <- "No issues were found in the dataset."
  }

  result <- return(list(problems = problems[-1, ], dialogue = dialogue))
  return(result)
}






