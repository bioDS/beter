test_fasta = "test_files/uppercase.fasta"
test_nexus = "test_files/uppercase.nex"
primates_fasta = "test_files/primates.fasta"
primates_nexus = "test_files/primates.nex"

#' A wrapper around tools::md5sum that does not return file names
md5sum = function(files){
    sums = tools::md5sum(files)
    names(sums) = NULL
    sums
    }


test_that("Written fasta is identical to test file", {
    fasta = tempfile(fileext = ".fasta")
    on.exit(unlink(fasta), add=TRUE)
    write_fasta(sequences, fasta, nchar=4)

    expect_identical(md5sum(fasta), md5sum(test_fasta))
    })


test_that("Written nexus is identical to test file", {
    nexus = tempfile(fileext = ".nex")
    on.exit(unlink(nexus), add=TRUE)
    write_nexus(sequences, nexus, missing="?", gap="-")
    
    expect_identical(md5sum(nexus), md5sum(test_nexus))
    })


test_that("Writting and reading fasta is identical to input", {
    fasta = tempfile(fileext = ".fasta")
    on.exit(unlink(fasta), add=TRUE)
    write_fasta(sequences, fasta, nchar=4)
    seq = read_fasta(fasta)
    
    expect_identical(seq, sequences) 
    })


test_that("Writting and reading nexus is identical to input", {
    nexus = tempfile(fileext = ".nex")
    on.exit(unlink(nexus), add=TRUE)
    write_nexus(sequences, nexus, missing="?", gap="-")
    seq = read_nexus(nexus)
    
    expect_identical(seq, sequences)
    })


test_that("write_sequences selects the correct format", {
    # fasta
    fasta = tempfile(fileext = ".fasta")
    on.exit(unlink(fasta), add=TRUE)
    write_sequences(sequences, fasta, nchar=4)
    expect_identical(md5sum(fasta), md5sum(test_fasta))
    
    # nexus
    nexus = tempfile(fileext = ".nex")
    on.exit(unlink(nexus), add=TRUE)
    write_sequences(sequences, nexus, missing="?", gap="-")
    expect_identical(md5sum(nexus), md5sum(test_nexus))
    })


test_that("Throws an error if unsupported file format is provided", {
    expect_error(write_sequences(sequences, format=""), "Unsupported format: ")
    expect_error(write_sequences(sequences, format="blargh"), "Unsupported format: blargh")
    expect_error(write_sequences(sequences, "test."), "Unrecognized extension: ")
    expect_error(write_sequences(sequences, "test.blargh"), "Unrecognized extension: blargh")
    })
