#' Find {{ moustache }} tags in a file or text.
#' \code{find_tags_str} will find tags in a string. \code{find_tags_xml} and \code{find_tags_toml}
#' will find tags in a file after using an apropriate way to read it. Finally, \code{find_tags}
#' will call above mentioned functions depending on its input.
#'
#' @param template String that represents path to xml or toml file or text that contain tags
#' @return list of {{ moustache }} tags
#' @examples
#' find_tags("Foo bar {{baz}}")
find_tags = function(template){
    if(!file.exists(template))
        return(find_tags_str(template)
    if(tolower(file_ext(template)) == "toml")
        return(find_tags_toml(template)
    if(tolower(file_ext(template)) == "xml")
        return(find_tags_xml(template))
    stop("Unrecognized file extension")
    }

#' @rdname find_tags
find_tags_xml = function(template){
    template = readLines(template)
    find_tags_str(template)
    }

#' @rdname find_tags
find_tags_toml = function(template){
    template = RcppTOML::parseTOML(template)
    find_tags_str(template)
    }

#' @rdname find_tags
find_tags_str = function(template){
    tags = stringr::str_extract_all(template, "\\{{2,3}[/#]?[.\\w]*\\}{2,3}")
    tags = unlist(tags)
    tags = stringr::str_remove_all(tags, "[\\{\\}/#]")
    tags = tags[tags != "." ]
    tags = unique(tags)
    tags
    }


#' Temporarily set a path to a specific directory. Path is restored once
#' the calling function/frame ends.
#' @param dir directory
#'
#' dir = tempdir()
#' test = function(){
#'     settmpwd(dir)
#'     print(getwd())
#'     }
#' test() # should print dir
#' print(getwd()) # back to normal
settmpdir = function(dir){
    envir = parent.frame()
    envir$oldwd = getwd()
    do.call("on.exit", list(quote(setwd(oldwd))), envir=envir)
    setwd(dir)
    }


#' An utility function with an effect similar to Unix mkdir -p.
#' Recursively create directory. If directory exists, do nothing.
#'
#' @param dir
#'
#' tmpdir = tempdir()
#' mkdir(tmpdir) # dir already exists, no error or warning is reported
mkdir = function(dir){
    if(!dir.exists(dir))
        dir.create(dir, recursive=TRUE)
    }
