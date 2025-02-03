#' @param x numeric vector
#' @param set_max upper limit of returned vector
#' @param direction (defauls to 1) setting to -1 reverse the order
#' of the scaling. 
#' @return vector with min of 0 and max `set_max`
forceLimits <- function(x, set_max = 1, direction = 1){
  if(direction == 1){
    x_temp <- (x - min(x))
    x_temp <- x_temp/max(x_temp)
    # apply scaling
    return(set_max*x_temp)
  }
  if(direction == -1){
    x_temp <- (x - max(x))
    x_temp <- x_temp/min(x_temp)
    # apply scaling
    return(set_max*x_temp)
  }
}

# ------------------------------------------------------------------------------
#' @param df_input subsetted df with only numeric columns
#' @param flips integer vector with nrow(df_input)
#' In most cases flips = rep(1, nrow(df_input))
#' @returns numeric column between 0 and 1 of length nrow(df_input)
combineVars <- function(df_input, flips = NULL){
  # drop any geometry
  if(class(df_input)[1] == "sf"){
    df_input <- st_drop_geometry(df_input)
  }
  if(is.null(flips)){
    flips <- rep(1, ncol(df_input))
  }
  if(ncol(df_input) != length(flips)){
    stop("flips should have same number of entries as the number of columns of df_input")
  }
  upper_limit_per_var <- 1/ncol(df_input)
  # Make all columns numeric
  df_input <- mutate_all(df_input, ~as.numeric(.))
  # workhorse of function
  stand_mat <- mapply(forceLimits, as.data.frame(df_input),
                      set_max = upper_limit_per_var,
                      direction = flips)
  # equal weighting for each variable
  return(forceLimits(rowSums(stand_mat)))
}

# ------------------------------------------------------------------------------
#' @param df_input subsetted df
#' @returns Label function to be directly fed into leaflet
getLeafletLabels <- function(df_input){
  # drop any geometry
  if(class(df_input)[1] == "sf"){
    df_input <- st_drop_geometry(df_input)
  }
  # Variable information
  p <- ncol(df_input) - 1
  var_names <- colnames(df_input)[!str_detect(colnames(df_input), "shpId")]
  
  # Initialize lists for character vectors and arguments
  input_char_vec <- list()
  stf_args <- list(df_input$shpId)  # Initialize the list with shpId
  
  # dynamically create arguments list
  for (var_name in var_names) {
    if (var_name == "combinedParamValues") {
      input_char_vec <- c(input_char_vec, "<br/>%s: %0.3f")
    } else if (is.numeric(df_input[[var_name]])) {
      input_char_vec <- c(input_char_vec, "<br/>%s: %0.3f")
    } else if (is.character(df_input[[var_name]])) {
      input_char_vec <- c(input_char_vec, "<br/>%s: %s")
    }
    stf_args <- c(stf_args, list(var_name), list(df_input[[var_name]]))
  }
  
  # construct html character vector
  input_char <- paste0("<strong>%s</strong>", paste0(input_char_vec, collapse = ""))
  
  # use do.call to create labels
  return(do.call(sprintf, c(input_char, stf_args)) %>% lapply(htmltools::HTML))
}