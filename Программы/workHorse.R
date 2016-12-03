workHorse <- function(K = k, N = n, ...) {
        # Время работы функии
        tic()
        tests <- list()
        modelFlow <- usersFlow(K, N, pA = pksi, pB = peta)
        # modelFlow <- usersFlow()
        
        for (i in 1 : K) {
                tests[i] <- list(chiTest(ksi = unlist(modelFlow$flow[[1]]), 
                                         eta = unlist(modelFlow$flow[[i]])))
                # setTxtProgressBar(pb, i)
        }
        
        pVal <- lapply(tests, function(x) {unlist(x[1])})
        falsePositive <- length(which(unlist(rbind(pVal)) < alpha))
        fPRate <- falsePositive / K
        
        plot(sort(unlist(rbind(pVal))), type = 'p', ylim = c(0,1))
        abline(h = alpha, col = 'red')
        abline(h = alpha/n, col = 'red')
        toc()
        
        fPRateBon <- BonferroniCorrection(pVal, alpha)
        
        # write.table(unlist(pVal), file = paste("pVal_", k, ".txt"), 
        #             quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        return(list("pVal" = pVal, "FPR" = fPRate, "FPRB" = fPRateBon))
}

# ls(environment(workHorse))
# get("tests", environment(workHorse))
