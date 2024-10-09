context("Aggregation")
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))
test_that("aggregateLevels aggregates the levels of a category", {
  set.seed(4072017)
  testData <- data.frame(Category = rep(c("A", "B", "C"), each=5),
                         Values = c(rpois(5, 5), rpois(5, 5), rpois(5, 5)))

  result <- aggregateLevels(testData, "Category", "Values", c("B"="C"))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("Category", "Values"))
  expect_equal(as.character(result$Category), c("A", "B"))
  sumA <- sum(testData$Values[testData$Category=="A"])
  sumB <- sum(testData$Values[testData$Category!="A"])
  expect_equal(result$Values, c(sumA, sumB))
})

test_that("aggregateLevels aggregates the levels of a category without replacement", {
  set.seed(4072017)
  testData <- data.frame(Category = rep(c("A", "B", "C"), each=5),
                         Values = c(rpois(5, 5), rpois(5, 5), rpois(5, 5)),
                         stringsAsFactors = FALSE)

  levels <- unique(testData$Category)
  names(levels) <- levels
  result <- aggregateLevels(testData, "Category", "Values", levels)
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("Category", "Values"))
  expect_equal(as.character(result$Category), c("A", "B", "C"))
  sumA <- sum(testData$Values[testData$Category=="A"])
  sumB <- sum(testData$Values[testData$Category=="B"])
  sumC <- sum(testData$Values[testData$Category=="C"])
  expect_equal(result$Values, c(sumA, sumB, sumC))
})

test_that("aggregateLevels can return the original data after aggregation", {
  set.seed(4072017)
  testData <- data.frame(Category = rep(c("A", "B", "C"), each=5),
                         Values = c(rpois(5, 5), rpois(5, 5), rpois(5, 5)),
                         stringsAsFactors = FALSE)

  result <- aggregateLevels(testData, "Category", "Values", c("B"="C"),
                            keepLowLevel = TRUE)
  expect_equal(nrow(result), nrow(testData)+2)
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("Category", "Values", "CategoryAggregated"))
  testResult <- as.character(result$CategoryAggregated[!is.na(result$CategoryAggregated)])
  expect_equal(testResult, c("A", "B"))

  sumA <- sum(testData$Values[testData$Category=="A"], na.rm=TRUE)
  testResult <- result$Values[result$CategoryAggregated=="A" & !is.na(result$CategoryAggregated)]
  expect_equal(testResult, sumA)

  sumB <- sum(testData$Values[testData$Category!="A"], na.rm=TRUE)
  testResult <- result$Values[result$CategoryAggregated=="B" & !is.na(result$CategoryAggregated)]
  expect_equal(testResult, sumB)

  originalData <- result[!is.na(result$Category), c("Category", "Values")]
  expect_equal(testData$Values, originalData$Values)
})

test_that("aggregateHierarchy aggregates from one level to another", {
  set.seed(16072017)
  testData <- data.frame(HighLevel = rep(c("A", "B"), length.out = 15),
                         LowLevel = rep(c("C", "D", "E"), each=5),
                         Values = c(rpois(5, 5), rpois(5, 5), rpois(5, 5)))
  result <- aggregateHierarchy(testData, "HighLevel", "LowLevel", "Values")
  expect_equal(dim(result), c(2, 2))
  expect_equal(colnames(result), c("HighLevel", "Values"))
  expect_equal(result$HighLevel, factor(c("A", "B")))
  expect_equal(result$Values, c(46, 29))
})

test_that("aggregateHierarchy can return the original data after aggregation", {
  set.seed(16072017)
  testData <- data.frame(HighLevel = rep(c("A", "B"), length.out = 15),
                         LowLevel = rep(c("C", "D", "E"), each=5),
                         Values = c(rpois(5, 5), rpois(5, 5), rpois(5, 5)))
  result <- aggregateHierarchy(testData, "HighLevel", "LowLevel", "Values",
                               keepLowLevel = TRUE)
  expect_equal(dim(result), c(17, 3))
  expect_equal(colnames(result), c("HighLevel", "LowLevel", "Values"))
  expect_equal(levels(result$HighLevel), c("A", "B"))
  expect_equal(unique(result$LowLevel), c("C", "D", "E", "TOTAL"))
  expect_equal(result$Values[result$LowLevel=="TOTAL"], c(46, 29))

  original <- result[result$LowLevel!="TOTAL", ]
  original$LowLevel <- factor(original$LowLevel)
  expect_equal(testData, original)
})

test_that("aggregate functions throw errors with incorrect input", {
  set.seed(16072017)
  testData <- data.frame(HighLevel = rep(c("A", "B"), length.out = 15),
                         LowLevel = rep(c("C", "D", "E"), each=5),
                         Values = c(rpois(5, 5), rpois(5, 5), rpois(5, 5)))
  expect_error(aggregateLevels(testData, "Category",  "Values", c("D"="C")))
  expect_error(aggregateLevels(testData, "HighLevel", "WrongValues",  c("D"="C")))

  expect_error(aggregateHierarchy(testData, "Category", "LowLevel", "Values"))
  expect_error(aggregateHierarchy(testData, "HighLevel", "WrongLevel", "Values"))
  expect_error(aggregateHierarchy(testData, "HighLevel", "LowLevel", "WrongValues"))
})

test_that("aggregateWithHierarchyTable aggregates using a hierarchy table", {
  set.seed(16072017)
  testData <- data.frame(LowLevel = rep(c("C", "D", "E"), each=5),
                         Values = c(rpois(5, 5), rpois(5, 5), rpois(5, 5)))
  hierarchyTable <- data.frame(HighLevel=c("A", "B", "B"),
                               LowLevel=c("C", "D", "E"))

  result <- aggregateWithHierarchyTable(testData, "LowLevel", hierarchyTable, "HighLevel", "Values")
  target <- structure(list(LowLevel = c("C", "C", "C", "C", "C", "D", "D",
                                        "D", "D", "D", "E", "E", "E", "E", "E",
                                        "A", "B"),
                           Values = c(3L, 8L, 9L, 3L, 7L, 7L, 7L, 2L, 9L, 4L,
                                      3L, 2L, 3L, 3L, 5L, 30L, 45L)),
                      .Names = c("LowLevel", "Values"),
                      row.names = c(NA, -17L), class = "data.frame")
  expect_equal(result, target)
})

test_that("aggregateWithHierarchyTable aggregates using a hierarchy table and drops original rows", {
  set.seed(16072017)
  testData <- data.frame(LowLevel = rep(c("C", "D", "E"), each=5),
                         Values = c(rpois(5, 5), rpois(5, 5), rpois(5, 5)),
                         stringsAsFactors = FALSE)
  hierarchyTable <- data.frame(HighLevel=c("A", "B", "B"),
                               LowLevel=c("C", "D", "E"),
                               stringsAsFactors = FALSE)
  result <- aggregateWithHierarchyTable(testData, column = "LowLevel", hierarchyTable,
                                        targetLevel = "HighLevel",
                                        values =  "Values",
                                        keepOriginal = FALSE)
  target <- structure(list(LowLevel = c("A", "B"),
                           Values = c(30L, 45L)),
                      .Names = c("LowLevel", "Values"),
                      class = c("tbl_df", "tbl", "data.frame"),
                      row.names = c(NA,-2L))
  expect_equal(result, target)

  # Pre and post column names should match
  expect_equal(colnames(testData), colnames(result))

})

test_that("aggregateWithHierarchyTable aggregates using a hierarchy table - complex example", {
  # read in data
  testData <- suppressMessages(readr::read_csv("../../inst/data/test_data_aggregate.csv"))
  hierarchyTable <- suppressMessages(readr::read_csv("../../inst/data/sector_hierarchy.csv"))
  values <- c("1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004",
              "2005","2006","2007","2008","2009")

  result <- aggregateWithHierarchyTable(testData, "Sector", hierarchyTable, "level_2",
                                       values)
  expect_equal(dim(result), c(12, 18))

  hierarchyTable <- data.frame(Total=rep("TOTAL", 3), Industry=c(1, 2, 3))
  nextResult <- aggregateWithHierarchyTable(result, "Industry", hierarchyTable, "Total",
                                           values)
  expect_equal(dim(nextResult), c(16, 18))
})

test_that("aggregateWithHierarchyTable aggregates using a hierarchy table and
          keeps subtotals - complex example", {
  # read in data
  testData <- suppressMessages(readr::read_csv("../../inst/data/test_data_aggregate.csv"))
  hierarchyTable <- suppressMessages(readr::read_csv("../../inst/data/sector_hierarchy.csv"))
  values <- c("1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004",
              "2005","2006","2007","2008","2009")

  result <- aggregateWithHierarchyTable(testData, "Sector", hierarchyTable, "level_2",
                                        values, withSubTotals = TRUE)
  expect_equal(dim(result), c(27, 18))
})

test_that("aggregateWithHierarchyTable fails when there are no matching levels", {
  set.seed(16072017)
  testData <- data.frame(LowLevel = rep(c("D", "E", "F"), each=5),
                         Values = c(rpois(5, 5), rpois(5, 5), rpois(5, 5)))
  hierarchyTable <- data.frame(HighLevel=c("A", "B", "B"),
                               LowLevel=c("A", "B", "C"))

  expect_error(aggregateWithHierarchyTable(testData, "LowLevel", hierarchyTable, "HighLevel", "Dummy"))
})

test_that("aggregateWithHierarchyTable generates warning if any categories in column do not match the hierarchyTable", {
  # When keepOriginal = FALSE we do not want series to be dropped if they are not in the hierarchy table
  testData <- data.frame(LowLevel = rep(c("A", "B", "C"), each = 3),
                         Values = c(1:3, 11:13, 101:103),
                         stringsAsFactors = FALSE)
  # Define hierarchy table *without* a grouping for "C"
  hierarchyTable <- data.frame(HighLevel = c("TOP", "TOP"),
                               LowLevel = c("A", "B"),
                               stringsAsFactors = FALSE)
  expect_message(aggregateWithHierarchyTable(testData,
                                             column = "LowLevel",
                                             hierarchyTable = hierarchyTable,
                                             targetLevel = "HighLevel",
                                             values = "Values",
                                             keepOriginal = FALSE),
                 "The following categories were not found in the hierarchy table")

})

test_that("aggregateWithHierarchyTable warns if hierarchyTable has duplicates.", {
  testData <- data.frame(LowLevel = rep(c("A", "B"), each = 3),
                         Values = c(1:3, 11:13),
                         stringsAsFactors = FALSE)
  # Define hierarchy table with duplicate rows
  hierarchyTable <- data.frame(HighLevel = c("TOP", "TOP"),
                               LowLevel = c("A", "B"),
                               stringsAsFactors = FALSE)
  # Make duplicates
  hierarchyTable <- rbind(hierarchyTable, hierarchyTable, stringsAsFactors = FALSE)
  expect_warning(aggregateWithHierarchyTable(testData,
                                             column = "LowLevel",
                                             hierarchyTable = hierarchyTable,
                                             targetLevel = "HighLevel",
                                             values = "Values",
                                             keepOriginal = FALSE),
                 "hierarchyTable contains duplicate rows")

})

