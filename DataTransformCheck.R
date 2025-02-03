require(moments)
DataTransformCheck <- list()

#' @name numericType
#' @description Helps to identify when a numeric variable could be a factor variable
#' Uses a cutoff of less than 15 unique values
DataTransformCheck$numericType <- function(x){
  if(!is.numeric(x)){
    return("Variable is non-numeric.")
  }
  if(length(unique(x)) < 15){
    return("Variable might be more suitably classified as a factor (categorical) variable.")
  }
}

#' @name distCheck
#' @description Helps to identify skewness, outliers
#' and suggestions 
DataTransformCheck$distCheck <- function(x){
  if(!is.numeric(x)){
    return("Variable is non-numeric.")
    break
  }
  # summaries data
  x <- x[!is.na(x)]
  z <- (x - mean(x, na.rm = T))/sd(x, na.rm = T)
  temp <- data.frame(min = min(x, na.rm = T),
                     skew = skewness(x, na.rm = T),
                     nu_outliers = sum(z > 3.5 | z < -3.5))
  
  # automatic
  temp <- temp %>% 
    mutate(is_Skew = ifelse(is.na(skew > 1.5), FALSE, skew > 1),
           are_Outliers = ifelse(is.na(nu_outliers > 0), FALSE, nu_outliers > 0),
           is_Positive = min >= 0,
           is_PositiveNonZero = min > 0)
  
  # Sqrt: Positive and high skew
  if(temp$is_Skew & temp$is_Positive){
    return(paste0("Data is positively skewed. Consider using the square root transformation."))
    break
  }
  
  # LOG: Positive (and non-zero) and high skew
  if(temp$is_Skew & temp$is_PositiveNonZero){
    return(paste0("Data is positively skewed and non-zero. Consider using the logarithmic transformation."))
    break
  }
  
  # QUANTILE: Some outliers
  if(temp$are_Outliers){
    return(paste0("Outliers detected. Consider using the quantile or percentile transformation."))
    break
  }
}

#' @name whichFactors
#' @description gives a list of the numeric variables which could be factors
DataTransformCheck$whichFactors <- function(df){
  paste(names(unlist(apply(df, 2, DataTransformCheck$numericType))), collapse = ", ")
}

#' @name getTransSug
#' @description gives a dataframe where each row gives the transformation suggestion 
#' for each numeric variable
DataTransformCheck$getTransSug <- function(df){
  temp <- unlist(apply(df, 2, DataTransformCheck$distCheck))
  data.frame(variable = names(temp),
             suggestion = unname(temp))
}



