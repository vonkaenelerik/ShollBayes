test_that("shollmcmcObj output", {
    expect_s4_class(load_model("ug3_model"), "shollmcmcObj")
    expect_s4_class(load_model("mdnd_model"), "shollmcmcObj")
    expect_s4_class(load_model("crko_model"), "shollmcmcObj")
})

