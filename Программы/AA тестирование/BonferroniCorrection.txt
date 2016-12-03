BonferroniCorrection <- function(pVal, alpha) {
        FP <- length(which(pVal <= alpha / length(pVal)))
        FPR <- FP / length(pVal)
}
