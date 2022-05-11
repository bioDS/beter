test_that("write_config is identical to list2toml", {
    temp_file = tempfile()
    write_config(complex_lst, temp_file)
    obj = readLines(temp_file)
    unlink(temp_file)
    
    expected = list2toml(complex_lst)
    
    expect_identical(obj, expected)
    })
