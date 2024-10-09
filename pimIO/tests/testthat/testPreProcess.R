context("Preprocessing of input data")

test_that("preProcess processes the input data",{

  input <- data.frame(gfcfCP = c(225, 167, 222),
                      gfcfCVM = c(325.6, 214.7, 259.3),
                      K1CP = c(10, 20, 30), K3CP = c(10, 20, 30),
                      K4CP = c(-20, -22, -10), K5CP = c(-20, -22, -10),
                      K61CP = c(10, 20, 30), K62CP = c(10, 20, 30))
  input$PriceIndex <- input$gfcfCP / input$gfcfCVM
  result <- preProcess(input)
  # the following is obtained by executing dput(round(result,6))
  target <- structure(list(gfcfCP = c(225, 167, 222),
                           gfcfCVM = c(325.6, 214.7, 259.3),
                           K1CP = c(10, 20, 30), K3CP = c(10, 20, 30),
                           K4CP = c(-20, -22, -10), K5CP = c(-20, -22, -10),
                           K61CP = c(10, 20, 30), K62CP = c(10, 20, 30),
                           PriceIndex = c(0.691032, 0.77783, 0.856151),
                           K1CVM = c(14.471111, 25.712575, 35.040541),
                           K3CVM = c(14.471111, 25.712575, 35.040541),
                           K4CVM = c(-28.942222, -28.283832, -11.68018),
                           K5CVM = c(-28.942222, -28.283832, -11.68018),
                           K61CVM = c(14.471111, 25.712575, 35.040541),
                           K62CVM = c(14.471111, 25.712575, 35.040541),
                           gfcf_ociv = c(325.6, 260.982635, 376.101802)),
                      .Names = c("gfcfCP", "gfcfCVM", "K1CP", "K3CP", "K4CP",
                                 "K5CP", "K61CP", "K62CP",
                                 "PriceIndex", "K1CVM", "K3CVM", "K4CVM",
                                 "K5CVM", "K61CVM", "K62CVM",
                                 "gfcf_ociv"),
                      row.names = c(NA, -3L), class = "data.frame")

  expect_equal(result, target, tolerance = 1e-6)
})
