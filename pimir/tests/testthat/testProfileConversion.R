context("Age-Price/Age-Efficiency Profile Conversion")

test_that("efficiencyToPriceProfile throws an error for incorrect input",{
  expect_error(efficiencyToPriceProfile("A", 0, 0))
  expect_error(efficiencyToPriceProfile(numeric(), 0, 0))
  expect_error(efficiencyToPriceProfile(c(1,0.5, 0), -1, 0))
  expect_error(efficiencyToPriceProfile(c(1,0.5, 0), 1, -1))
  expect_error(efficiencyToPriceProfile(c(1,0.5, 0), 3, 0))
})

test_that("efficiencyToPriceProfile converts an age-efficiency profile to a
          age-price profile", {
  # following example taken from OECD Manual section 3.3 (page 33)
  ageEfficiencyProfile <- seq(1, 0, -0.125) # linear profile with max age is 8
  result <- efficiencyToPriceProfile(ageEfficiencyProfile[-1], 0.05, 0.02)
  expect_equal(result, c(0.785041, 0.594306, 0.428507, 0.288378, 0.174675,
                         0.088173, 0.029674, 0),
               tolerance=1e-6)
})

test_that("priceToEfficiencyProfile throws an error for incorrect input",{
  expect_error(priceToEfficiencyProfile("A", 0, 0))
  expect_error(priceToEfficiencyProfile(numeric(), 0, 0))
  expect_error(priceToEfficiencyProfile(c(1,0.5, 0), -1, 0))
  expect_error(priceToEfficiencyProfile(c(1,0.5, 0), 1, -1))
  expect_error(priceToEfficiencyProfile(c(1,0.5, 0), 3, 0))
})

test_that("priceToEfficiencyProfile converts an age-efficiency profile to a
          age-price profile", {
  # following example is the reverse of the above example
  priceProfile <- c(0.785041, 0.594306, 0.428507, 0.288378, 0.174675,
                    0.088173, 0.029674, 0)
  result <- priceToEfficiencyProfile(priceProfile, 0.05, 0.02)
  expect_equal(result, seq(1, 0, -0.125)[-1],
               tolerance=1e-5)
})

