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


test_that("Written file is identical to test file", {
    fasta = tempfile(fileext = ".fasta")
    on.exit(unlink(fasta), add=TRUE)
    write_fasta(sequences, fasta, nchar=4)

    expect_identical(md5sum(fasta), md5sum(test_fasta))
    })

# TODO Check if writting and reading sequences produce an identical output

# TODO Check if number of characters per line works
