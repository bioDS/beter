#' Set options for the beter package
#'
#' Set options that affect behaviour of `beter` package.
#'
#' @param ... positional arguments are ignored
#' @param scientific older version of BEAST2 do not support scientific format in the XMLs for
#'     integer or long parameters and thus by default, scientific notation (either from toml config,
#'     or when specified in R) is unrolled. By setting `scientific = TRUE`, scientific notation is
#'     preserved using native R's auto-conversion (using `as.character`).
#'
#' @export  
beter_options = function(..., scientific){
    if(length(list(...)) > 0)
        warning("Positional arguments are ignored.")

    if(!missing(scientific))
        options("beter.scientific" = scientific)

    invisible()
    }
