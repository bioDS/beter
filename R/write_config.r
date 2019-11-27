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


#' list to toml
#'
#' Convert list to a string vector that represents of TOML. Each item of the vector represents
#' a line of the TOML file.
#'
#' \code{list2toml} process structured lists recursively by first processing any non-list members
#' and then processing remaining items of the list type using another call of \code{lit2toml}.
#' In these calls, the name of the item is used as a \code{parent_name}. 
#'
#' @param x a named list
#' @param parent_name name of the parent list in recursive calls of this function. Should not be
#'     specified
#' @return a string vector representing individual lines of TOML representation of \code{x}.
#'
#' @export
#'
#' @examples
#' config = list(
#'     "xml" = list("chunk" = "<xml>{{tag}}</xml>"),
#'     "templates" = list(),
#'     "defaults" = list("tag" = "Just another tag in the wall!")
#'      )
#' list2toml(config)
list2toml = function(x, parent_name=NULL){
    # check against empty list:
    if(length(x) == 0){
        return("")
        }
    # process those that are not lists:
    items = x[sapply(x, Negate(is.list))]
    item_lines = unlist(mapply(process_item, names(items), items, USE.NAMES=FALSE))
    if(length(item_lines) > 0)
        item_lines = c(item_lines, "") 
    # process lists using a recursive call of this function:
    lists = x[sapply(x, is.list)]
    list_lines = unlist(mapply(
        process_list,
        names(lists), lists,
        MoreArgs=list(parent_name=parent_name),
        USE.NAMES=FALSE
        ))
    c(item_lines, list_lines)
    }


process_item = function(name, item){
    if(methods::is(item, "character"))
        item = paste0("\"", item, "\"")
    if(length(item) > 1)
        item = paste("[", paste0(item, collapse=", "), "]")
    paste(name, "=", item)
    }


process_list = function(name, list, parent_name=NULL){
    new_name = join(parent_name, name, sep=".")
    line = paste0("[", new_name, "]")
    lines = list2toml(list, new_name)
    c(line, lines)
    }
