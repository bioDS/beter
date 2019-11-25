#' Test whether list is null or empty
#'
#' @param x a list
#' @return true/false
is.empty = function(x){
    is.null(x) || length(x) == 0
    }


#' Sort named list
#'
#' Reorder list according to its names.
#' @param x a list
#' @param decreasing logical. Should the sort be increasing or decreasing?
#' @return sorted list
sort_named_list = function(x, decreasing = FALSE){
    if(!is.list(x))
        stop("Object must be vector")
    x = x[sort(names(x), decreasing = decreasing)]
    for(i in seq_along(x)){
        if(is.list(x[[i]]))
            x[[i]] = sort_named_list(x[[i]], decreasing=decreasing)
        }
    x
    }
