# code for making FIXTURES: objects that are used for testing 
# NOTE: this code is NOT run when testing, the objects need to be
# made and re-made during development, then saved as .rds files

mac_RTI_good <- rti(mac_height$obs, scale_rci = 1.39)
saveRDS(mac_RTI_good, file = "tests/testthat/fixtures/mac_RTI_good.rds")

mac_RTI_NA <- mac_RTI_good
mac_RTI_NA$RCI <- NA
saveRDS(mac_RTI_NA, file = "tests/testthat/fixtures/mac_RTI_NA.rds")
