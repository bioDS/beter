#' Write sequences to a file
#'
#' @param seq a named vector of sequences
#' @param file a character string naming a file
#' @param format **optional** write sequences in this format, currently supported
#' formats are `fasta` and `nexus`
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
write_sequences = function(seq, file="", format=NULL, ...){
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
#' @param nchar **optional** a number of characters per line
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
#' @param datatype **optional** the type of data
#' @param missing **optional** symbol representing missing data
#' @param gap **optional** symbol representing a gap in aligned sequence
#' @export
write_nexus = function(seq, file="", datatype=NULL, missing=NULL, gap=NULL){
    if(is.null(datatype))
        datatype = guess_datatype(seq)

    format = paste0(
        "format datatype=", datatype,
        if(!is.null(missing)) paste0(" missing=", missing) else "",
        if(!is.null(gap)) paste0(" gap=", gap) else "",
        ";"
        )

    header = c(
        "#NEXUS",
        "begin data;",
        paste0("dimensions ntax=", length(seq), " nchar=", nchar(seq[1]), ";"),
        format,
        #paste0("format datatype=", datatype, missing, gap, ";\n",
        "matrix"
        )
    footer = ";\nend;"
    
    names = names(seq)
    names = format(names, width=max(nchar(names))+1)
    data = paste0(names, seq)
    
    writeLines(c(header, data, footer), file)
    }
