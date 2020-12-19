#tests/testthat/test-uiElements.R

testthat::test_that("Menu works", {
  main_men <- main_menu()
  testthat::expect_true(!is.null(main_men))
})
