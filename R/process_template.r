#' Process an XML template with TOML config.
#'
#' \code{process_template} will parse TOML config with three different blocks, xml containing
#' additional xml chunks that will be processed independently, templates containing file paths
#' relative to the parsed TOML config) to additional TOML configs and defaults contain default
#' parameters for main template, xml chunks contained in the TOML config or any subtemplates linked
#' in the TOML config. The xml chunks and any parameters are then substituted to the template
#' according to {{ moustache }} tags.
#'
#' First the function will recursively parse any TOML configs linked in the template block and
#' then process XML chunks by substituting the {{ moustache }} tags using the content of the
#' default block and any parameters from the parent TOML config. After that, processed XML chunks
#' as well as any other values in default block of TOML config or from input parameters are
#' substituted into the XML template according to the present {{ moustache }} tags.
#'
#' There are two different ways how parameters and xml chunks sharing the same name are merged.
#' Parameters will overwrite parameters with the same name in their daughter config. This means
#' that subtemplates/subconfigs can be shared by different TOML configs and be parametrized
#' according to their need. On the other hand, xml chunks with the same name are merged together
#' as a list, this makes it possible for different subtemplates to define their own priors,
#' operators or logs, which can be then put in a single point without replacing each other's
#' structures. This makes it possible to build a BEAST2 XML.
#'
#' @param template an XML template with {{ moustache }} tags that will be substituted according to
#'     the input parameters or the default parameters in the TOML config.
#' @param config a TOML config file containing default values for {{ moustache }} tags, XML chunks
#'     or path to additional TOML subconfigs/subtemplates.
#' @param output processed template
#' @param parameters these will replace the parameters with the same name in the TOML config
#'     or any TOML subconfigs/subtemplates
#'
#' @export
process_template = function(template, config, output, parameters=NULL){
    config = parse_config(config, parameters)
    data = merge(config$data, merge(config$defaults, parameters))
    template = readLines(template)
    text = whisker::whisker.render(template, data)
    writeLines(text, output)
    }


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
process_xml_chunks = function(xml_chunks, parameters){
    lapply(xml_chunks, whisker::whisker.render, data=parameters)
    }
