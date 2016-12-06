correction <- function(Pval = pVal, Alpha = alpha) {
        
        # FP <- length(which(Pval < Alpha / length(Pval)))
        # FP <- length(which(Pval <= Alpha / length(Pval)))
        # FP <- length(p.adjust(Pval, method = "bonferroni") < Alpha)
        # FPR <- FP / length(Pval)
        
        ## or all of them at once (dropping the "fdr" alias):
        p.adjust.M <- p.adjust.methods[p.adjust.methods != "fdr"]
        p.adj    <- sapply(p.adjust.M, function(meth) p.adjust(Pval, meth))
        
        # stopifnot(identical(p.adj[,"none"], p), p.adj <= p.adj.60)


        FP <- sapply(split(p.adj, p.adjust.M), function(x) length(x < alpha))
        p.adj <- rbind(p.adj, FP)
        # tapply(p.adj, p.adjust.M, function(x) length(x < alpha))
        
        
        return(p.adj)
        # noquote(apply(p.adj, 2, format.pval, digits = 3))
        
}



