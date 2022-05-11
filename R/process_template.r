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
#' In addition, alignment in format FASTA or NEXUS can be specified and will be inputted
#' as {{{ sequences }}} tag. If not specified, the alignmet data type will be guessed
#' and the alignment id will be constructed out of the alignment file. 
#'
#' @param template an XML template with {{ moustache }} tags that will be substituted according to
#'     the input parameters or the default parameters in the TOML config.
#' @param output processed template
#' @param config a TOML config file containing default values for {{ moustache }} tags, XML chunks
#'     or path to additional TOML subconfigs/subtemplates.
#' @param alignment sequence alignment
#' @param format of the sequence alignment
#' @param parameters these will replace the parameters with the same name in the TOML config
#'     or any TOML subconfigs/subtemplates
#'
#' @export
process_template = function(
    template, output,
    config=NULL, alignment=NULL, format=NULL, parameters=NULL
    ){
    if(!is.null(config)){
        config = parse_config(config, parameters)
        }

    data = merge(config$data, merge(config$defaults, parameters))

    if(!is.null(alignment)){
        if(is.null(data$alignment_id))
            data$alignment_id = basename_sans_ext(alignment)
        sequences = read_sequences(alignment, format)
        data$sequences = sequences2xml(sequences, data)
        }

    template = readLines(template)
    
    if(!isTRUE(getOption("beter.scientific")))
        data = format_list(data)
        
    text = whisker::whisker.render(template, data)
    writeLines(text, output)
    }
