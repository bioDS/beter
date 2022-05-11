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


#' Convert numeric elements in list to string
#'
#' Recursively converts numeric elements in list to string using the `format` function.
#' This is done to guarantee that scientific notation is not used.
#'
#' @param x a list
#' @param scientific whether to keep or not the scientific notation
#' @param trim suppress justifying values to right
#' @return formatted list
format_list = function(x, scientific=FALSE, trim=TRUE){
    if(length(x) == 0)
        return(x)

    numeric = sapply(x, is.numeric)
    list = sapply(x, is.list)

    x[numeric] = format(x[numeric], scientific=scientific, trim=trim)
    x[list] = lapply(x[list], format_list, scientific=scientific, trim=trim)

    x
    }
