#' Write sequences to a file
#'
#' @param seq a named vector of sequences
#' @param file a character string naming a file
#' @param nchar **optional** a number of characters per line
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
