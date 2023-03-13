optim.basket <- function(design, par, method, control, utility, constraint, ...){
  if(utility == "Rejection_Probabilities[1]"){
    get_fn
  }
}

get_fn <- function(design, utility, constraint, ...){
  details_output <- c("Rejection_Probabilities")
  utility_spec <- parse_utility(utility)
  utility_str <- utility_spec[["utility_str"]]
  utility_i <- utility_spec[["utility_i"]]
  if(utility_str %in% details_output & constraint %in% details_output){
    function(x){
      details <- get_details(design = design, epsilon = x[1], tau = x[2], ...)
    }
  }
}

parse_utility <- function(utility){
  utility_split <- strsplit(x = utility, split = "\\[")[[1]]
  if(length(utility_split) != 2){
    stop(paste0("The utility string's value, '", utility, "' must contain ",
                "exactly one opening square bracket [."))
  }
  utility_str <- utility_split[1]
  utility_i <- gsub("]", replacement = "", x = utility_split[2])
  return(list(utility_str = utility_str, utility_i = utility_i))
}

