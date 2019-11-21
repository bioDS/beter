context("Test utils.r")


test_that("Can find {{ mustache }} tags", {
    test_toml = file.path("test_files", "test.toml")
    test_xml = file.path("test_files", "test.xml")
    test_other = tempfile(fileext=".other")
    cat("I live AGAIN!", file=test_other)

    expect_equal(find_tags("foo bar {{baz}}"), "baz")
    expect_equal(find_tags(test_toml), c("baz", "foo"))
    expect_equal(find_tags(test_xml), c("foo", "bar", "baz"))
    expect_error(find_tags(test_other))
    })


test_that("settmpdir is working temporaly and not pernamently", {
    wd = getwd()
    tmpdir = tempdir()
    test_settmpdir = function(){
        settmpdir(tmpdir)
        getwd()
        }

    expect_equal(test_settmpdir(), tmpdir)
    expect_equal({test_settmpdir(); getwd()}, wd)
    })


test_that("test mkdir is silently creating path", {
    tmpdir = tempdir()
    test_dir = file.path(tmpdir, "test")
    expect_false(dir.exists(test_dir))
    expect_silent(mkdir(test_dir))
    expect_true(dir.exists(test_dir))
    expect_silent(mkdir(test_dir))
    unlink(test_dir, recursive=TRUE)
    })
