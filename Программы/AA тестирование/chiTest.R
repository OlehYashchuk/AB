########## Критерий Хи2 для пропорций
chiTest <- function(A = ksi, B = eta) {
        # р-pool
        pPool <- (sum(A) + sum(B)) / (length(A) + length(B))
        # Числитель статистики
        chiSNumerator <- sum(A) / length(A) - sum(B) / length(B)
        # Знаменатель статистики
        chiSDenominator <- sqrt(pPool * (1-pPool) * 
                                        (1/length(A)+1/length(B))+0.1^100)
        # Статистика критерия
        se <- chiSNumerator / chiSDenominator
        # p-value для статистики Хи2 при двусторонней альтернативе
        pVal <- 2 * min(pnorm(se, lower.tail = TRUE), 
                        pnorm(se, lower.tail = FALSE))
        # верхняя граница доверительного интервала(при верной нулевой гипотезе)
        upperConf <- quant * chiSDenominator
        
        return(list("pVal" = pVal, "upperConf" = upperConf))
}
