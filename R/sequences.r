#' Convert sequences to BEAST xml data block.
#'
#' This functions converts the parsed sequences into an XML data block for BEAST.
#'
#' @param sequences parsed from file
#' @param data other variables, such as specification of dataType
#' @return BEAST xml data block
sequences2xml = function(sequences, data=list()){
    # if parameters are not set, try to guess them
    if(is.null(data$datatype)) data$datatype = guess_datatype(sequences)
    if(is.null(data$alignment_id)) data$alignment_id = "alignment"

    if(data$datatype == "standard"){
        data$standard = TRUE
        data$number_of_states = guess_number_of_states(sequences)
        }

    template_standard = paste0(
        "    <userDataType",
        " id=\"StandardDataType\"",
        " spec=\"beast.evolution.datatype.StandardData\"",
        " nrOfStates=\"{{number_of_states}}\" />\n"
        )

    template = paste0(
        "<data id=\"{{alignment_id}}\" dataType=\"{{datatype}}\">\n",
        "    {{#sequences}}\n",
        "    <sequence taxon=\"{{name}}\">{{sequence}}</sequence>\n",
        "    {{/sequences}}\n",
        "{{#standard}}",
        template_standard,
        "{{/standard}}",       
        "</data>")
    sequences_tuples = mapply(
        function(x,y){list(name=x, sequence=y)},
        names(sequences), sequences,
        SIMPLIFY=FALSE, USE.NAMES=FALSE
        )
    data$sequences = sequences_tuples

    whisker::whisker.render(template, data)
    }


#' Guess datatype from sequences
#'
#' Guess data type from sequences. Currently two data types are supported:
#' \itemize{
#'      \item nucleotide
#'      \item standard
#'  }
#'
#' @param sequences list of strings
#' @return datatype
guess_datatype = function(sequences){
    chars = lapply(utils::head(sequences), substring, 1, 5)
    chars = paste0(unlist(chars), collapse="")
    chars = unique(strsplit(chars, "")[[1]])
    chars = toupper(chars)
    if(all(chars %in% c("A", "C", "T", "G", "-", "N", "?")))
        return("nucleotide")
    if(all(chars %in% c(0:9, "-", "N", "?")))
        return("standard")
    stop("Unrecognized data type: ", paste0(chars, collapse=", "))
    }


#' Guess the number of states
#'
#' Guess the number of states in sequences
#'
#' @param sequences list of strings
#' @return number of states
guess_number_of_states = function(sequences){
    chars = lapply(sequences, function(x) unique(strsplit(x, "")[[1]]))
    chars = unique(unlist(chars))
    chars = chars[!chars %in% c("-", "N", "?")]
    length(chars)
    }
