#' Recursively parse TOML config
#̈́'
#' First recursively parse any TOML configs linked in the template block and then process XML
#' chunks by substituting the {{ moustache }} tags using the content of the default block and
#' any parameters from the parent TOML config.
#'
#' There are two different ways how parameters and xml chunks sharing the same name are merged.
#' Parameters will overwrite parameters with the same name in their daughter config. This means
#' that subtemplates/subconfigs can be shared by different TOML configs and be parametrized
#' according to their need. On the other hand, xml chunks with the same name are merged together
#' as a list, this makes it possible for different subtemplates to define their own priors,
#' operators or logs, which can be then put in a single point without replacing each other's
#' structures. This makes it possible to build a BEAST2 XML. 
#'
#' @param file TOML config
#' @param defaults parameters that will replace any other parameters from the default block
#' @return list of processed XML chunks from this or any daughter config
#'     and the default parameter block
#'
#' @export
parse_config = function(file, defaults=NULL){
    config = RcppTOML::parseTOML(file, escape=FALSE)
    settmpdir(dirname(file))

    defaults = merge(config$defaults, defaults)
    data = list()
    if(!is.null(config$xml))
        data = add(data, process_xml_chunks(config$xml, defaults))
    if(!is.null(config$templates))
        data = add(data, process_subtemplates(config$templates, defaults))
    return(list("data" = data, "defaults" = defaults))
    }


#' Process TOML subtemplates/subconfigs in the templates block
#'
#' @param subtemplates list of TOML subtemplates from the templates block
#' @param defaults parameters
#'
#' @keywords internal
process_subtemplates = function(subtemplates, defaults){  
    data = list()
    for(subtemplate in subtemplates){
        data = add(data, parse_config(subtemplate, defaults)$data)
        }
    data
    }


#' Process list of xml chunks
#'
#' process xml chunks, replacing the {{ moustache }} tags with the parameters
#' @param xml_chunks list of strings with {{ moustache }} tags
#' @param parameters parameters to substitute {{ moustache }} tags
#' @return list of processed xml chunks
#'
#' @keywords internal
process_xml_chunks = function(xml_chunks, parameters){
    if(!isTRUE(getOption("beter.scientific")))
        parameters = format_list(parameters)

    lapply(xml_chunks, whisker::whisker.render, data=parameters)
    }
