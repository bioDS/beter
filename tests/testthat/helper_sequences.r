#' Make a temporal copy of file with a random filename
tempcopy = function(file){
    temp = tempfile()
    file.copy(file, temp, overwrite=TRUE)
    return(temp) 
    }

#' Named vector of sequences
sequences = c(
    "seq1" = "ACTGACTG",
    "seq2" = "CTGACTGA",
    "seq3" = "TGACTGAC"
    )

#' Create random sequences
random_sequences = function(states=1:9, nseq=10, length=10){
    seqs = replicate(
        nseq,
        paste0(sample(as.character(states), size=length, replace=TRUE), collapse=""),
        simplify=FALSE
        )
    names(seqs) = paste0("seq_", seq_len(nseq))
    seqs
    }
