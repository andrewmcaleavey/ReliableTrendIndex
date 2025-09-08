# tests/testthat/test-simFuncs.R

test_that("generate_data returns expected tibble and side-effects", {
  set.seed(123)
  n_ppl    <- 200
  sd_tot   <- 1
  meas_err <- 0.5
  out <- generate_data(
    n_sims = 1, n_ppl = n_ppl,
    sd_tot = sd_tot, m_bl_tot = 0,
    delta = -0.25, delta_sd = 0.8, meas_err = meas_err
  )
  
  # basic structure
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2 * n_ppl) # 2 rows per person
  expect_true(all(c("id","time","obs","tru","obs_diff","true_diff",
                    "ReliableChange","TrueChange","obsChange",
                    "wrongRCI","trueRCI","borderMisclassified",
                    "delta_err","obsCorrect","RCICorrect",
                    "nullTile","delta") %in% names(out)))
  
  # side effects created in parent env by <<-
  expect_true(exists("Sdiff", inherits = TRUE))
  expect_true(exists("RCI",   inherits = TRUE))
  
  # RCI ~ 1.96 * sqrt(2) * meas_err (in expectation)
  expect_equal(get("RCI", inherits = TRUE), 1.96 * sqrt(2) * meas_err, tolerance = 0.15)
  
  # empirical reliability at baseline close to theoretical rxx
  rxx_emp <- rxx_empirical(out)
  rxx_theory <- sd_tot^2 / (sd_tot^2 + meas_err^2)
  expect_equal(rxx_emp, rxx_theory, tolerance = 0.06)
})

test_that("generate_xt_data returns expected tibble", {
  set.seed(123)
  n_ppl    <- 120
  n_obs    <- 6
  out <- generate_xt_data(
    n_sims = 1, n_ppl = n_ppl, n_obs = n_obs,
    sd_tot = 1, m_bl_tot = 0, delta = -0.3, delta_sd = 0.6, meas_err = 0.4
  )
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), n_ppl * n_obs)
  expect_true(all(c("true_t0","true_slope","true_value","obs_diff","true_diff") %in% names(out)))
  # side effects visible
  expect_true(exists("Sdiff", inherits = TRUE))
  expect_true(exists("RCI",   inherits = TRUE))
})

test_that("RCIfunc implements expected closed form", {
  rxx <- 0.8
  s1  <- 2
  cut <- 1.96
  # algebra: RCI = cut * sqrt(2) * s1 * sqrt(1 - rxx)
  expect_equal(RCIfunc(rxx, s1, cut), cut * sqrt(2) * s1 * sqrt(1 - rxx))
})

test_that("tables of ReliableChange behave as expected", {
  set.seed(1)
  x <- generate_data(n_ppl = 150)
  
  # raw counts per person (divideByTwo = TRUE halves the 2-rows-per-id table)
  tab_half <- relChangeRaw_table(x, divideByTwo = TRUE)
  expect_equal(sum(tab_half), 150)
  
  tab_full <- relChangeRaw_table(x, divideByTwo = FALSE)
  expect_equal(sum(tab_full), nrow(x)) # includes both time rows
  
  # percentages sum to ~100
  pct <- relChangePct_table(x)
  expect_type(pct, "double")
  expect_equal(sum(pct), 100, tolerance = 1e-8)
  expect_true(all(names(pct) %in% c("RelDet","RelImp","NoRel")))
})

test_that("plotting helpers return ggplot objects", {
  set.seed(42)
  x <- generate_data(n_ppl = 80)
  
  p1 <- plot_raw(x)
  p2 <- hist_obs_diff_raw(x)
  p3 <- plot_rci_grps(x)
  
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
})

test_that("counting_func returns expected named list and into_output_tbl shapes it", {
  set.seed(99)
  x <- generate_data(n_ppl = 120)
  res <- counting_func(x)
  
  # expect some key names present
  expect_true(all(c(
    "RCITotalAcc","ObsTotalAcc","SensDetRCI","SensImpRCI","SpecDetRCI","SpecImpRCI",
    "PPPDetRCI","PPPImpRCI","NPPDetRCI","NPPImpRCI","TypeSRCI","TypeSObs",
    "MAEdiff","AbsErrBiasDiff","rxx","RCI","PctClassifiedRCI","delta"
  ) %in% names(res)))
  
  # proportions are within [0,1] where appropriate
  props <- c("RCITotalAcc","ObsTotalAcc","SensDetRCI","SensImpRCI","SpecDetRCI",
             "SpecImpRCI","PPPDetRCI","PPPImpRCI","NPPDetRCI","NPPImpRCI","TypeSRCI","TypeSObs",
             "PctClassifiedRCI")
  for (nm in props) {
    expect_gte(res[[nm]], 0)
    expect_lte(res[[nm]], 1)
  }
  
  # into_output_tbl produces a 1-row tibble
  row <- into_output_tbl(res)
  expect_s3_class(row, "tbl_df")
  expect_equal(nrow(row), 1)
})

test_that("compute_lm augments data and accuracy helpers are in [0,1]", {
  set.seed(123)
  x <- generate_data(n_ppl = 60)
  xl <- compute_lm(x)
  
  expect_true(all(c("lm.p.value","lm.Rel","lm.est","lmDir","lm.95.rel") %in% names(xl)))
  acc_lm  <- total_accuracy_lm(xl)
  acc_lmd <- total_accuracy_lmDir(xl)
  acc_obs <- total_accuracy_obs(xl)
  acc_rci <- total_accuracy_rci(xl)
  
  for (v in c(acc_lm, acc_lmd, acc_obs, acc_rci)) {
    expect_gte(v, 0)
    expect_lte(v, 1)
  }
})

test_that("compute_rma runs (if metafor present) and accuracy in [0,1]", {
  skip_if_not_installed("metafor")
  set.seed(321)
  x <- generate_data(n_ppl = 50, sd_tot = 1, meas_err = 0.5)
  
  # compute_rma uses SEm in the rma call; provide it in the caller env
  # (matches current implementation expectation)
  on.exit({ if (exists("SEm", envir = .GlobalEnv)) rm(SEm, envir = .GlobalEnv) }, add = TRUE)
  SEm <<- stats::sd(dplyr::filter(x, time == 0)$obs) * sqrt(1 - (1^2 / (1^2 + 0.5^2)))
  
  xr <- compute_rma(x)
  expect_true(all(c("rma.p.value","rma.Rel","rma.est","rmaDir","rma.95.rel") %in% names(xr)))
  
  acc_rma  <- total_accuracy_rma(xr)
  acc_rmad <- total_accuracy_rmaDir(xr)
  expect_gte(acc_rma,  0); expect_lte(acc_rma,  1)
  expect_gte(acc_rmad, 0); expect_lte(acc_rmad, 1)
})

test_that("plotting_func and plotting_func_delta return named list of ggplot objects", {
  # fabricate a tiny comparison.data with required columns
  comp_rxx <- tibble::tibble(
    rxx = seq(0.5, 0.9, length.out = 5),
    ObsTotalAcc = runif(5), RCITotalAcc = runif(5),
    TypeSObs = runif(5), TypeSRCI = runif(5),
    RCI = runif(5, 0.5, 2),
    PctClassifiedRCI = runif(5),
    PctTrueDet = runif(5), PctRelDet = runif(5), PctObsDet = runif(5),
    PctTrueImp = runif(5), PctRelImp = runif(5), PctObsImp = runif(5),
    SensDetObs = runif(5), SensDetRCI = runif(5),
    SensImpObs = runif(5), SensImpRCI = runif(5),
    SpecDetObs = runif(5), SpecDetRCI = runif(5),
    SpecImpObs = runif(5), SpecImpRCI = runif(5),
    PPPDetObs  = runif(5), PPPDetRCI  = runif(5),
    PPPImpObs  = runif(5), PPPImpRCI  = runif(5),
    NPPDetObs  = runif(5), NPPDetRCI  = runif(5),
    NPPImpObs  = runif(5), NPPImpRCI  = runif(5)
  )
  plots1 <- plotting_func(comp_rxx)
  expect_type(plots1, "list")
  expect_true(all(c("plotAcc","plotTypeS","plotRCI","plotClassifiedRCI",
                    "plotGrpAccDet","plotGrpAccImp","plotSensDet","plotSensImp",
                    "plotSpecDet","plotSpecImp","plotPPPDet","plotPPPImp",
                    "plotNPPDet","plotNPPImp") %in% names(plots1)))
  lapply(plots1, function(p) expect_s3_class(p, "ggplot"))
  
  comp_delta <- comp_rxx |>
    dplyr::mutate(delta = seq(-0.6, 0.6, length.out = 5)) |>
    dplyr::select(-rxx) |>
    dplyr::relocate(delta)
  
  plots2 <- plotting_func_delta(comp_delta)
  expect_type(plots2, "list")
  expect_true(all(names(plots1) %in% names(plots2)))
  lapply(plots2, function(p) expect_s3_class(p, "ggplot"))
})
