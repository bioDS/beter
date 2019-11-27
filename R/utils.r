#' Find {{ moustache }} tags.
#'
#' Find {{ moustache }} tags in a string or file. \code{find_tags_str} will find tags in a string.
#' \code{find_tags_xml} and \code{find_tags_toml} will find tags in a file after using
#' an apropriate way to read it. Finally, \code{find_tags} will call above mentioned functions
#' depending on its input.
#'
#' @param template String that represents path to xml, toml file or text that contain tags.
#' For the \code{find_tags} function, if the string is a path to an existing file, first read as
#' using the TOML parser or as a plain text depending if the extension of file is \code{.toml}
#' or \code{.xml}.
#' @return list of {{ moustache }} tags
#'
#' @export
#'
#' @examples
#' find_tags("Foo bar {{baz}}")
find_tags = function(template){
    if(!file.exists(template))
        return(find_tags_str(template))
    if(tolower(tools::file_ext(template)) == "toml")
        return(find_tags_toml(template))
    if(tolower(tools::file_ext(template)) == "xml")
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
    template = RcppTOML::parseTOML(template)$xml
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
#' @examples
#' dir = tempdir()
#' test = function(){
#'     beter:::settmpdir(dir)
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
#' @param dir directory
#'
#' @examples
#' tmpdir = tempdir()
#' beter:::mkdir(tmpdir) # dir already exists, no error or warning is reported
mkdir = function(dir){
    if(!dir.exists(dir))
        dir.create(dir, recursive=TRUE)
    }


#' join strings
#'
#' Similar as paste, but ignores NULL, NA and empty strings.
#'
#' @param ... one or more R objects, to be converted to character vectors.
#' @param sep a a character string to separate the terms.  Not "NA_character_".
#' @param collapse an optional character string to separate the results.  Not "NA_character_".
#' @return a character vector of the concatenated values. See \code{\link{paste}}.
#'
#' @examples
#' beter:::join("foo", NULL, NA, "", "bar") # "foo bar"
join = function(..., sep=" ", collapse=NULL){
    args = list(...)
    args = rlist::list.clean(
        args,
        fun=function(x) is.null(x) || is.na(x) || x == "",
        recursive=TRUE
        )
    do.call(paste, args=c(args, sep=sep, collapse=collapse))
    }
