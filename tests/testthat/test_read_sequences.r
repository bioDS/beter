context("Read sequence alignment files")

#' Make a temporal copy of file with a random filename
tempcopy = function(file){
    temp = tempfile()
    file.copy(file, temp, overwrite=TRUE)
    return(temp) 
    }

# Named vector of sequences
sequences = c(
    "seq1" = "ACTGACTG",
    "seq2" = "CTGACTGA",
    "seq3" = "TGACTGAC"
    )

# Path to sequence alignment files
fasta_file = "test_files/test.fasta"
nexus_file = "test_files/test.nex"


test_that("Can read a sample fasta file", {
    expect_identical(read_fasta(fasta_file), sequences)
    })


test_that("Can read a sample nexus file", {
    expect_identical(read_nexus(nexus_file), sequences)
    })


test_that("Uses a correct function to read alignment when FORMAT is specified", {
    expect_identical(read_sequences(tempcopy(fasta_file), format="fasta"), sequences)
    expect_identical(read_sequences(tempcopy(nexus_file), format="nexus"), sequences)
    })
 
 
test_that("Uses a correct function to read alignment by guessing format from extension", {
    expect_identical(read_sequences(fasta_file), sequences)
    expect_identical(read_sequences(nexus_file), sequences)
    })
