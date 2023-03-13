#' Optimize a basket trial design
#'
#' @param design
#' @param par
#' @param method
#' @param control
#' @param utility
#' @param constraint
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
optim.basket <- function(design, par, method, control, utility, constraint, ...){
  if(!("fnscale" %in% names(control))){
    control <- c(control, fnscale = -1)
  }
  if(is.character(constraint) & is.character(utility)){
    fn <- get_fn(design, utility, constraint, ...)
    return(optim(par = c(2, 0),
          fn = fn,
          method = method,
          control = control))
  }
}

get_fn <- function(design, utility, constraint, ...){
  details_output <- c("Rejection_Probabilities")
  utility_spec <- parse_utility(utility)
  constraint_spec <- parse_constraint(constraint)
  utility_str <- utility_spec[["utility_str"]]
  utility_i <- utility_spec[["utility_i"]]
  constraint_str <- constraint_spec[["constraint_str"]]
  constraint_i <- constraint_spec[["constraint_i"]]
  constraint_val <- constraint_spec[["constraint_val"]]
  if(utility_str %in% details_output & constraint_str %in% details_output){
    function(x){
      details <- get_details(design = design, epsilon = x[1], tau = x[2], ...)
      if(details[[constraint_str]][constraint_i] >= constraint_val){
        return(-Inf)
      } else {
        return(details[[utility_str]][utility_i])
      }
    }
  }
}

parse_utility <- function(utility){
  utility_split <- strsplit(x = utility, split = "\\[")[[1]]
  if(length(utility_split) != 2){
    stop(paste0("The utility string's value, i.e. '", utility, "', must contain ",
                "exactly one opening square bracket [."))
  }
  utility_str <- utility_split[1]
  utility_i <- as.numeric(gsub("]", replacement = "", x = utility_split[2]))
  return(list(utility_str = utility_str, utility_i = utility_i))
}

parse_constraint <- function(constraint){
  constraint_split1 <- strsplit(x = constraint, split = "\\[")[[1]]
  if(length(constraint_split1) != 2){
    stop(paste0("The constraint string's value, i.e. '", constraint, "', must contain ",
                "exactly one opening square bracket [."))
  }
  constraint_str <- constraint_split1[1]
  constraint_split2 <- strsplit(x = constraint_split1[2], split = "<")[[1]]
  if(length(constraint_split2) != 2){
    stop(paste0("The constraint string's value, i.e. '", constraint, "', must contain ",
                "exactly one 'less than' operator <."))
  }
  constraint_i <- as.numeric(gsub("]", replacement = "", x = constraint_split2[1]))
  constraint_val <- as.numeric(constraint_split2[2])
  return(list(constraint_str = constraint_str, constraint_i = constraint_i,
              constraint_val = constraint_val))
}

