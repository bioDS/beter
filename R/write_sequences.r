#' Write sequences to a file
#'
#' @param seq a named vector of sequences
#' @param file a character string naming a file
#' @param format **optional** write sequences in this format, currently supported
#' formats are `fasta` and `nexus`
#' @param nchar **optional** a number of characters per line
#' @param datatype **optional** the type of data
#' @param ... **optional** parameters passed to the underlying function
#'
#' @export
#'
#' @examples
#' seq1 = c("A" = "AAA", "B" = "BBB", "C" = "CCC")
#' fasta = tempfile(fileext = ".fasta")
#' write_fasta(seq1, fasta)
#'
#' seq2 = read_fasta(fasta)
#' identical(seq1, seq2)
write_sequences = function(seq, file, format=NULL, ...){
    supported_formats = c("fasta", "nexus")

    if(is.null(format)){
        ext = tolower(tools::file_ext(file))        
        if(ext %in% c("fasta", "fst", "fas")) format = "fasta"
        if(ext %in% c("nexus", "nex")) format = "nexus"
        if(is.null(format)) stop("Unrecognized extension: ", ext)
        }

    if(!format %in% supported_formats) stop("Unsupported format: ", format)
    if(format == "fasta") write_fasta(seq, file, ...)
    if(format == "nexus") write_nexus(seq, file, ...)
    }


#' @rdname write_sequences
#' @export
write_fasta = function(seq, file="", nchar=80){
    if(!is.null(nchar)){
        nchar = as.numeric(nchar)
        seq = gsub(
            paste0("(.{1,", nchar, "})"),
            "\\1\n",
            seq
            )
        }
    text = paste0(">", names(seq), "\n", seq)
    # remove the last newline, write lines already adds it
    text[length(text)] = sub("\n$", "", text[length(text)])
    writeLines(text, file)
    }


#' @rdname write_sequences
#' @export
write_nexus = function(seq, file="", datatype=NULL){
    if(is.null(datatype))
        datatype = guess_datatype(seq)

    header = paste0(
        "#NEXUS\n",
        "BEGIN DATA;\n",
        "\tDimension ntax=", length(seq), " nchar=", nchar(seq[1]), ";\n",
        "\tFormat datatype=", datatype, ";\n",
        "\tMatrix\n"
        )
    footer = ";\nEND;\n"
    
    names = names(seq)
    names = format(names, width=max(nchar(names))+1)
    data = paste0(names, seq, "\n")
    
    writeLines(c(header, data, footer), file)
    }
