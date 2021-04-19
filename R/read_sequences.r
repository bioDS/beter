#' Read sequences from a file
#'
#' These functions read a sequence alignment file and return a named list of sequences.
#' Currently supported formats are `fasta` and `nexus`. Interleaved formats, where sequences are
#' defined in a repeating blocks, are not supported.
#'
#' The function `[read_sequences]` tries to guess the correct format from the file extension.
#' Alternatively, the correct format can be specified. Currently supported formats are
#' \emph{fasta} and \emph{nexus}.
#'
#' Functions `[read_fasta]` and `[read_nexus]` are then used to read the sequence alignment file. 
#'
#' @param file with alignment
#' @param format requested format
#' @return named list of sequences
#'
#' @export
read_sequences = function(file, format=NULL){
    supported_formats = c("fasta", "nexus")

    if(!is.null(format)){
        if(!format %in% supported_formats) stop("Unsupported format: ", format)
        if(format == "fasta") return(read_fasta(file))
        if(format == "nexus") return(read_nexus(file))

        } else {

        ext = tolower(tools::file_ext(file))

        if(ext %in% c("fasta", "fst", "fas")) return(read_fasta(file))
        if(ext %in% c("nexus", "nex")) return(read_nexus(file))
        # else:
        stop("Unrecognized extension:", ext)
        }
    }


#' @rdname read_sequences
read_fasta = function(file){
    text = readLines(file)

    starts = grep("^>", text)
    stops = c(starts[-1]-1, length(text))

    sequences = mapply(
        function(start, stop, text){
            seq = text[(start+1):stop]
            seq = gsub("[:blank:]*", "", seq)
            paste0(seq, collapse="")
            },
        starts, stops, MoreArgs=list(text)
        )
    names(sequences) = sub("^>", "", text[starts])

    return(sequences)
    }


#' @rdname read_sequences
read_nexus = function(file){
    text = readLines(file)

    # Find start and end of the data block:
    begin_data = grep("^begin data;", text, ignore.case=TRUE)
    ends = grep("^end;", text, ignore.case=TRUE)
    end_data = ends[which.min(abs(ends - begin_data))]

    # Find the start and end of the data matrix
    begin_matrix = grep("^matrix", ignore.case=TRUE, text[begin_data:end_data]) + begin_data - 1
    end_matrix = grep("^;", text[begin_matrix:end_data])[1] + begin_matrix - 1

    header = parse_nexus_header(text[(begin_data+1):(begin_matrix-1)])
    sequences = parse_nexus_sequences(text[(begin_matrix+1):(end_matrix-1)])

    return(sequences)
    }


#' Internal helper functions for reading alignment
#'
#' These functions are used internally when alignemnt is being read and processed. They tend to
#' extract and proces sequences from particular part of text.
#'
#' @param text vector of lines with sequences in particular format
#' @param from in a fasta file, this is a vector of begining of sequences
#' @param to in a fasta file, this is a vector of ends of sequences
#' @name read_sequences_helper
NULL


#' @rdname read_sequences_helper
parse_nexus_header = function(text){
    text = sub(";$", "", text)
    text = strsplit(text, split=" ", fixed=TRUE)
    text = unlist(text)
    text = grep("=", text, value=TRUE)
    text = strsplit(text, split="=", fixed=TRUE)
    header = lapply(text, getElement, 2)
    names(header) = lapply(text, getElement, 1)
    header
    }


#' @rdname read_sequences_helper
parse_nexus_sequences = function(text){
    text = strsplit(text, split="[[:blank:]]+")
    sequences = lapply(text, getElement, 2)
    sequences = lapply(sequences, toupper)
    names(sequences) = lapply(text, getElement, 1)
    sequences
    }
