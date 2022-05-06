#' Write TOML config
#'
#' This function will write a list as a TOML. This is not a fully fledged TOML writer and is used
#' only for testing purposes.
#'
#' @param x a named list
#' @param file path to file where TOML config will be written
#'
#' @export
#'
#' @examples
#' config = list(
#'     "xml" = list("chunk" = "<xml>{{tag}}</xml>"),
#'     "templates" = list(),
#'     "defaults" = list("tag" = "Just another tag in the wall!")
#'      )
#' write_config(config, stdout())
write_config = function(x, file){
    lines = list2toml(x)
    writeLines(lines, file)
    }
