workHorse <- function(K = k, N = round(n), Alpha = alpha, ...) {
        
        # Время работы функии
        tic()
        
        modelFlow <- usersFlow(K, N, pA = pksi)
        
        ## Сравниваем все альтернативные страницы с контрольной
        # Получаем значение p-value и верхнюю границу доверительного 
        # интервала для каждой пары альтернативной и контрольной страниц
        tests <- list()
        pb <- txtProgressBar(min = 1, max = N, style = 3)
        for (i in 2 : K) {
                tests[i-1] <- list(chiTest(A = unlist(modelFlow$flow[[1]])[1:N], 
                                           B = unlist(modelFlow$flow[[i]])[1:N]))
                
                setTxtProgressBar(pb, i)
        }
        close(pb)
        
        ## Сравниваем контрольную страницу с альтернативными
        # Массив массивов
        testsMultiple <- list()
        for (i in 1 : K) {
                testsMultiple[i] <- as.list(testsMultiple[i])
                testsMultiple[[i]] <- as.list(testsMultiple[[i]])
        }
        
        M <- 1
        for (i in 1 : K) {
                for (j in M : K) {
                        testsMultiple[[i]][j] <- list(
                                chiTest(A = unlist(modelFlow$flow[[i]])[1:N],
                                        B = unlist(modelFlow$flow[[j]])[1:N]))
                        
                }
                M <- M + 1
        }
        
        pValMat <- matrix(0, K, K)
        for (i in 1 : K) {
                for (j in 1 : K) {
        pValMat[j, i] <- ifelse(is.null((testsMultiple[[i]][[j]]$pVal)) || i==j,
                                          0, testsMultiple[[i]][[j]]$pVal)
                }
        }
        
        pValMultiple <- lapply(testsMultiple, function(x) {unlist(x[1])})
        
        close(pb)
        toc()
        
        p.adj.Matrix <- correction(array(pValMat[pValMat != 0]), Alpha)
        # write.table(p.adj.Matrix, file = paste("pVal_", K, ".txt"), 
        #             quote = FALSE, row.names = FALSE, sep = ';')

        return(list("pValMultiple" = pValMat, "testsMultiple" = testsMultiple,
                    "p.adj.Matrix" = p.adj.Matrix)) 
}

# ls(environment(workHorse))
# get("n", environment(workHorse))
