context("Constraining")
suppressWarnings(library(tibble))
suppressWarnings(library(readr))
suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))

# ---------------- Read Test Data ----------------------------------------------

# Read in old, new, and target datasets
# datasets contain series for CP, CYP, and PYP for two sector/ind/asset combinations
colSpec <- cols(
  Sector = col_character(),
  Industry = col_character(),
  Asset = col_character(),
  Period = col_character(),
  CP = col_double(),
  CYP = col_double(),
  PYP = col_double())

publishedRaw <- read_csv("../../inst/data/test_constrain_old.csv", col_types = colSpec)
latestRaw <- read_csv("../../inst/data/test_constrain_new.csv", col_types = colSpec)
# Target values were produced using open period of 2015Q1
target <- read_csv("../../inst/data/test_constrain_target.csv", col_types = colSpec)

# ------------------------ Contrain --------------------------------------------

test_that("Constraining single series returns correct result.", {

  published <- tribble(
    ~Period, ~Value,
    "2012Q1",444.56270822, "2012Q2",218.71742337, "2012Q3",275.29280948,
    "2012Q4",328.72859494, "2013Q1",253.69119465, "2013Q2",126.47803976,
    "2013Q3",240.41637122, "2013Q4",254.93037638, "2014Q1",315.00125154,
    "2014Q2",221.27725185, "2014Q3",529.20455102, "2014Q4",688.48304369,
    "2015Q1",688.85932784, "2015Q2",739.61497417, "2015Q3",777.95765551,
    "2015Q4",553.18688267, "2016Q1",699.00002044, "2016Q2",387.02605657,
    "2016Q3",470.21216946, "2016Q4",444.89151095, "2017Q1",556.19218464,
    "2017Q2",564.51112263, "2017Q3",733.4809431)

  latest <- tribble(
    ~Period, ~Value,
    "2012Q1",446.76947982503, "2012Q2",220.920070471268, "2012Q3",275.519660866956,
    "2012Q4",329.533859892145, "2013Q1",253.78622847858, "2013Q2",128.643779150532,
    "2013Q3",244.225082058552, "2013Q4",257.961160403792, "2014Q1",315.600920737406,
    "2014Q2",223.850372893398, "2014Q3",532.885348292396, "2014Q4",695.188322639197,
    "2015Q1",696.006131169977, "2015Q2",744.251019693828, "2015Q3",793.271937415033,
    "2015Q4",557.730624877062, "2016Q1",704.751367710936, "2016Q2",391.495339372304,
    "2016Q3",473.636729292919, "2016Q4",451.567448741121, "2017Q1",567.069878699974,
    "2017Q2",571.690917818487, "2017Q3",737.689231427854)

  # target contains correct values with open period set to 2015Q1
  target <- tribble(
    ~Period, ~Value,
    "2012Q1",444.56270822, "2012Q2",218.71742337, "2012Q3",275.29280948,
    "2012Q4",328.72859494, "2013Q1",253.69119465, "2013Q2",126.47803976,
    "2013Q3",240.41637122, "2013Q4",254.93037638, "2014Q1",315.00125154,
    "2014Q2",221.27725185, "2014Q3",529.20455102, "2014Q4",688.48304369,
    "2015Q1",689.292964236837, "2015Q2",737.072517793326, "2015Q3",785.620615536171,
    "2015Q4",552.351162511938, "2016Q1",697.953850611698, "2016Q2",387.719261189868,
    "2016Q3",469.068375241116, "2016Q4",447.211958855026, "2017Q1",561.600336711789,
    "2017Q2",566.176804661144, "2017Q3",730.574019745695)

  result <- constrain(published, latest, "2015Q1", varName = "Value")

  expect_equal(as.data.frame(result), as.data.frame(target))

})

test_that("Constrain extends an old series using new data.", {

  # Old series finishing in 2016Q1
  published <- tribble(
    ~Period, ~Value,
    "2012Q1",444.56270822, "2012Q2",218.71742337, "2012Q3",275.29280948,
    "2012Q4",328.72859494, "2013Q1",253.69119465, "2013Q2",126.47803976,
    "2013Q3",240.41637122, "2013Q4",254.93037638, "2014Q1",315.00125154,
    "2014Q2",221.27725185, "2014Q3",529.20455102, "2014Q4",688.48304369,
    "2015Q1",688.85932784, "2015Q2",739.61497417, "2015Q3",777.95765551,
    "2015Q4",553.18688267, "2016Q1",697.953850611698 )

  # New series finishing in 2017Q3
  latest <- tribble(
    ~Period, ~Value,
    "2012Q1",446.76947982503, "2012Q2",220.920070471268, "2012Q3",275.519660866956,
    "2012Q4",329.533859892145, "2013Q1",253.78622847858, "2013Q2",128.643779150532,
    "2013Q3",244.225082058552, "2013Q4",257.961160403792, "2014Q1",315.600920737406,
    "2014Q2",223.850372893398, "2014Q3",532.885348292396, "2014Q4",695.188322639197,
    "2015Q1",696.006131169977, "2015Q2",744.251019693828, "2015Q3",793.271937415033,
    "2015Q4",557.730624877062, "2016Q1",704.751367710936, "2016Q2",391.495339372304,
    "2016Q3",473.636729292919, "2016Q4",451.567448741121, "2017Q1",567.069878699974,
    "2017Q2",571.690917818487, "2017Q3",737.689231427854)

  # target contains correct values with open period set to 2015Q1
  target <- tribble(
    ~Period, ~Value,
    "2012Q1",444.56270822, "2012Q2",218.71742337, "2012Q3",275.29280948,
    "2012Q4",328.72859494, "2013Q1",253.69119465, "2013Q2",126.47803976,
    "2013Q3",240.41637122, "2013Q4",254.93037638, "2014Q1",315.00125154,
    "2014Q2",221.27725185, "2014Q3",529.20455102, "2014Q4",688.48304369,
    "2015Q1",689.292964236837, "2015Q2",737.072517793326, "2015Q3",785.620615536171,
    "2015Q4",552.351162511938, "2016Q1",697.953850611698, "2016Q2",387.719261189868,
    "2016Q3",469.068375241116, "2016Q4",447.211958855026, "2017Q1",561.600336711789,
    "2017Q2",566.176804661144, "2017Q3",730.574019745695)

  result <- constrain(published, latest, "2015Q1", varName = "Value")

  expect_equal(as.data.frame(result), as.data.frame(target))

})

test_that("Constrain returns provided new values unmodified when pass = TRUE.", {

  latest <- tribble(
    ~Period, ~Value,
    "2012Q1",446.76947982503, "2012Q2",220.920070471268, "2012Q3",275.519660866956,
    "2012Q4",329.533859892145, "2013Q1",253.78622847858, "2013Q2",128.643779150532,
    "2013Q3",244.225082058552, "2013Q4",257.961160403792, "2014Q1",315.600920737406,
    "2014Q2",223.850372893398, "2014Q3",532.885348292396, "2014Q4",688.48304369)

  result <- constrain(new = latest, varName = "Value", pass = TRUE)
  expect_equal(result, latest)
})


test_that("Constrain throws error when called with invalid arguments", {

  published <- tribble(
    ~Period, ~Value,
    "2012Q1",444.56270822, "2012Q2",218.71742337, "2012Q3",275.29280948,
    "2012Q4",328.72859494, "2013Q1",253.69119465, "2013Q2",126.47803976,
    "2013Q3",240.41637122, "2013Q4",254.93037638, "2014Q1",315.00125154)

  latest <- tribble(
    ~Period, ~Value,
    "2012Q1",446.76947982503, "2012Q2",220.920070471268, "2012Q3",275.519660866956,
    "2012Q4",329.533859892145, "2013Q1",253.78622847858, "2013Q2",128.643779150532,
    "2013Q3",244.225082058552, "2013Q4",257.961160403792, "2014Q1",315.600920737406,
    "2014Q2",223.850372893398, "2014Q3",532.885348292396, "2014Q4",688.48304369,
    "2015Q1",689.292964236837, "2015Q2",737.072517793326)


  # Without required old dataset (when pass is FALSE)
  expect_error(constrain(new = latest, openPeriod = "2014Q1", varName = "Value"), "Must provide old dataset")
  # Without required openPeriod (when pass is FALSE)
  expect_error(constrain(old = published, new = latest, varName = "Value"), "Must provide openPeriod")

  # Without varName in provided new dataset
  expect_error(constrain(old = published, new = latest, openPeriod = "2014Q1", varName = "missingValue"), "\"missingValue\" not in new")

  # Without old dataset containing Period col
  expect_error(constrain(old = rename(published, Time = Period),
                         new = latest, openPeriod = "2014Q1", varName = "Value"),
               "old must contain \"Period\"")

  # Without new containing the openPeriod
  expect_error(constrain(old = published,
           new = filter(latest, Period != "2014Q1"),
           openPeriod = "2014Q1", varName = "Value"),
           "new must contain the openPeriod")
  # Without new containing a period before the openPeriod
  expect_error(constrain(old = published,
                         new = filter(latest, Period >= "2014Q1"),
                         openPeriod = "2014Q1", varName = "Value"),
               "new must contain at least one period before the openPeriod")

  # Without old containing the value preceeding the openPeriod (does not contain
  # 2014Q4)
  expect_error(constrain(old = filter(published, Period >= "2014Q1"),
                         new = latest,
                         openPeriod = "2014Q1", varName = "Value"),
               "old must contain at least one period before the openPeriod")


  # With new containing NA values immediately preceeding the openPeriod
  expect_error(constrain(old = published,
            new = mutate(latest, Value = if_else(Period == "2013Q4", NA_real_, Value)),
            openPeriod = "2014Q1", varName = "Value"),
            "new must not contain missing values")
  # With new containing NA values in the openPeriod
  expect_error(constrain(old = published,
                         new = mutate(latest, Value = if_else(Period == "2014Q1", NA_real_, Value)),
                         openPeriod = "2014Q1", varName = "Value"),
               "new must not contain missing values")

})


test_that("constrain must calculate correctly when old data is zero preceeding the openPeriod.", {

  # If the last value from the closed period is zero we cannot inflate from this
  # figure using growth rates, so constrain() must use a zero adjustment

  # Old series finishing in 2016Q1 with the value preceeding the open period
  # set to zero (2014Q4)
  published <- tribble(
    ~Period, ~Value,
    "2012Q1",444.56270822, "2012Q2",218.71742337, "2012Q3",275.29280948,
    "2012Q4",328.72859494, "2013Q1",253.69119465, "2013Q2",126.47803976,
    "2013Q3",240.41637122, "2013Q4",254.93037638, "2014Q1",315.00125154,
    "2014Q2",221.27725185, "2014Q3",529.20455102,
    "2014Q4",0,  # Zero
    "2015Q1",688.85932784, "2015Q2",739.61497417, "2015Q3",777.95765551,
    "2015Q4",553.18688267, "2016Q1",697.953850611698 )

  # New series
  latest <- tribble(
    ~Period, ~Value,
    "2012Q1",446.76947982503, "2012Q2",220.920070471268, "2012Q3",275.519660866956,
    "2012Q4",329.533859892145, "2013Q1",253.78622847858, "2013Q2",128.643779150532,
    "2013Q3",244.225082058552, "2013Q4",257.961160403792, "2014Q1",315.600920737406,
    "2014Q2",223.850372893398, "2014Q3",532.885348292396, "2014Q4",695.188322639197,
    "2015Q1",696.006131169977, "2015Q2",744.251019693828, "2015Q3",793.271937415033,
    "2015Q4",557.730624877062, "2016Q1",704.751367710936, "2016Q2",391.495339372304)

  # target contains correct values with open period set to 2015Q1
  # 2014Q4 should be zero. Onward values should be non-zero.
  target <- tribble( ~Period, ~Value,
    "2012Q1",444.56271, "2012Q2",218.71742, "2012Q3",275.29281, "2012Q4",328.72859,
    "2013Q1",253.69119, "2013Q2",126.47804, "2013Q3",240.41637, "2013Q4",254.93038,
    "2014Q1",315.00125, "2014Q2",221.27725, "2014Q3",529.20455, "2014Q4",0,
    "2015Q1",0.001, "2015Q2",0.00107, "2015Q3",0.00114, "2015Q4",0.0008,
    "2016Q1",0.00101, "2016Q2",0.00056)
  result <- constrain(published, latest, "2015Q1", varName = "Value", zeroAdj = 0.001)
  result <- mutate(result, Value = round(Value, 5))
  expect_equal(result, target)
})


test_that("constrain is correct when zeros are in the new data past the openPeriod.", {

  # published data with no zero values
  published <- tribble( ~Period, ~Value,
    "2014Q1",0.3, "2014Q2",0.2, "2014Q3",0.5, "2014Q4",0.6,
    "2015Q1",0.7, "2015Q2",0.75, "2015Q3",0.77, "2015Q4",0.5,
    "2016Q1",0.7, "2016Q2",0.3, "2016Q3",0.4)

  # latest data containing zero values past the openPeriod (2015Q4, 2016Q1)
  # Applying constraining naively would create divide by zero errors when growth
  # rates are calculated
  latest <- tribble( ~Period, ~Value,
    "2014Q1",0.31, "2014Q2",0.22, "2014Q3",0.53, "2014Q4",0.7,
    "2015Q1",0.7, "2015Q2",0.74, "2015Q3",0.79, "2015Q4",0,
    "2016Q1",0, "2016Q2",0.12, "2016Q3",0.48)

  # Target does not contain NAs and is able to reinflate from zero (e.g. 2016Q2)
  target <- tribble( ~Period, ~Value,
    "2014Q1",0.3, "2014Q2",0.2, "2014Q3",0.5, "2014Q4",0.6,
    "2015Q1",0.6, "2015Q2",0.634, "2015Q3",0.677, "2015Q4",0,
    "2016Q1",0, "2016Q2",0.103, "2016Q3",0.411)

  result <- constrain(published, latest, openPeriod = "2015Q1", varName = "Value")
  result <- mutate(result, Value = round(Value, 3))
  expect_equal(result, target)

})



# ------------------------ Constrain All ---------------------------------------


test_that("ConstrainAll can process multiple series and variables.", {

  published <- group_by(publishedRaw, Sector, Industry, Asset)
  published <- nest(published)

  latest <- group_by(latestRaw, Sector, Industry, Asset)
  latest <- nest(latest)

  result <- constrainAll(published, latest,
                         openPeriod = "2015Q1",
                         varNames = c("CP", "CYP", "PYP"))

  # Test the constrained values (in "constrained")
  result <- select(result, -data)
  result <- unnest(result)

  expect_equivalent(as.data.frame(result), as.data.frame(target))

})

test_that("ConstrainAll matches old and new series correctly.", {

  published <- group_by(publishedRaw, Sector, Industry, Asset)
  published <- nest(published)

  latest <- group_by(latestRaw, Sector, Industry, Asset)
  latest <- nest(latest)

  # flip the order of latest
  latest <- latest[c(2,1), ]

  result <- constrainAll(published, latest,
                         openPeriod = "2015Q1",
                         varNames = c("CP", "CYP", "PYP"))

  # Test the constrained values (in "constrained")
  result <- select(result, -data)
  result <- unnest(result)

  expect_equivalent(as.data.frame(result), as.data.frame(target))

})


test_that("ConstrainAll passes through any old series with unmatched new series.", {

  published <- group_by(publishedRaw, Sector, Industry, Asset)
  published <- nest(published)

  latest <- group_by(latestRaw, Sector, Industry, Asset)
  latest <- nest(latest)

  # Limit latest to S1 (published has S1 and S2)
  latest <- dplyr::filter(latest, Sector == "S1")

  result <- constrainAll(published, latest,
                         openPeriod = "2015Q1",
                         varNames = c("CP", "CYP", "PYP"))

  # Test the constrained values (in "constrained")
  result <- select(result, -data)
  result <- unnest(result)

  # S1 should match target
  resultS1 <- dplyr::filter(result, Sector == "S1")
  targetS1 <- dplyr::filter(target, Sector == "S1")
  expect_equivalent(as.data.frame(resultS1), as.data.frame(targetS1))

  # S2 should be unchanged from the original publishedRaw data
  resultS2 <- dplyr::filter(result, Sector == "S2")
  targetS2 <- dplyr::filter(publishedRaw, Sector == "S2")
  expect_equivalent(as.data.frame(resultS2), as.data.frame(targetS2))

})


test_that("ConstrainAll ignores any unmatched new series.", {

  published <- group_by(publishedRaw, Sector, Industry, Asset)
  published <- nest(published)

  latest <- group_by(latestRaw, Sector, Industry, Asset)
  latest <- nest(latest)

  # Limit published to S1 (latest has S1 and S2)
  published <- dplyr::filter(published, Sector == "S1")

  result <- constrainAll(published, latest,
                         openPeriod = "2015Q1",
                         varNames = c("CP", "CYP", "PYP"))

  expect_equal(nrow(published), nrow(result))

  # Test the constrained values
  result <- select(result, -data)
  result <- unnest(result)

  # S1 should match target
  targetS1 <- dplyr::filter(target, Sector == "S1")
  expect_equivalent(as.data.frame(result), as.data.frame(targetS1))

})

test_that("ConstrainAll passes through latest series when pass = TRUE.", {

  published <- group_by(publishedRaw, Sector, Industry, Asset)
  published <- nest(published)

  latest <- group_by(latestRaw, Sector, Industry, Asset)
  latest <- nest(latest)

  result <- constrainAll(published, latest,
                         openPeriod = "2015Q1",
                         varNames = c("CP", "CYP", "PYP"),
                         pass = TRUE)
  result <- unnest(select(result, -data))

  # The result should be the "latest" data passed through
  expect_equal(result, unnest(latest))

})

test_that("ConstrainAll needs minimal arguments when pass = TRUE.", {
  # Only old, new and varNames args are required when pass = TRUE

  published <- group_by(publishedRaw, Sector, Industry, Asset)
  published <- nest(published)

  latest <- group_by(latestRaw, Sector, Industry, Asset)
  latest <- nest(latest)

  result <- constrainAll(old = published,
                         new = latest,
                         varNames = c("CP", "CYP", "PYP"),
                         pass = TRUE)
  result <- unnest(select(result, -data))

  # The result should be the "latest" data passed through
  expect_equal(result, unnest(latest))

})
