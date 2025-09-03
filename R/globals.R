utils::globalVariables(c(
  "Metric", "NPPDetObs", "NPPDetRCI", "NPPImpObs", "NPPImpRCI", "ObsTotalAcc", "PPPDetObs", 
  "PPPDetRCI", "PPPImpObs", "PPPImpRCI", "PctClassifiedRCI", "PctObsDet", "PctObsImp", 
  "PctRelDet", "PctRelImp", "PctTrueDet", "PctTrueImp", "Proportion", "RCI", "RCICorrect", 
  "RCITotalAcc", "ReliableChange", "SEm", "Sdiff", "SensDetObs", "SensDetRCI", "SensImpObs", 
  "SensImpRCI", "SpecDetObs", "SpecDetRCI", "SpecImpObs", "SpecImpRCI", "TrueChange", 
  "TypeSObs", "TypeSRCI", "delta", "delta_err", "fit", "id", "idx", "n_correct", "numcorrect", "obs", 
  "obsChange", "obsCorrect", "obs_diff", "rnorm", "rxx", "scale_rci_calc", "score", "time", "tru", 
  "true_diff", "true_slope", "true_t0", "true_value", "y", "y_hat", "yhat", "ymax", "ymin", 
  "Sdiff", "RCI"
))

# note: this is just here to avoid R CMD CHECK Notes. 
# I know that this is not good programming. 
# All of this exists to support old vignettes, which probably should be
# deleted/end-of-lifed somehow. It's not important that they remain, 
# only that the Note is satisfied
