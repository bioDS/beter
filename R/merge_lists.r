#' Merge two lists
#'
#' Additive and non-additive merging of lists. \code{add} will merge lists additively, this means
#' that named items shared in both lists are added concatenated. \code{merge} is non-additive
#' merging and named items from list in the first argument will be replaced by items from
#' the list in the second argument.
#'
#' @param list1 a list
#' @param list2 a list
#' @return merged list
merge = function(list1, list2){
    if( !( (is.list(list1) || is.null(list1)) && (is.list(list2) || is.null(list2)) ) )
        stop("Both elements must be lists or NULL")
    if(is.empty(list1) && is.empty(list2))
        return(list())
    if(is.empty(list1))
        return(list2)
    if(is.empty(list2))
        return(list1)
    rlist::list.merge(list1, list2)
    }


#' @rdname merge
add = function(list1, list2){
    if( !( (is.list(list1) || is.null(list1)) && (is.list(list2) || is.null(list2)) ) )
        stop("Both elements must be lists or NULL")
    if(is.empty(list1) && is.empty(list2))
        return(list())
    if(is.empty(list1))
        return(list2)
    if(is.empty(list2))
        return(list1)
    merge = function(x){c(list1[[x]], list2[[x]])}
    names = unique(c(names(list1), names(list2)))
    merged = lapply(names, merge)
    names(merged) = names
    merged
    }


#' Test whether list is null or empty
#'
#' @param x a list
#' @return true/false
is.empty = function(x){
    is.null(x) || length(x) == 0
    }
