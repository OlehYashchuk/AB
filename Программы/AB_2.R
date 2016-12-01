# Множественная проверка гипотез
# Критерий Хи2

rm(list = ls())

options("max.print" = 50)
getOption

library(pwr)

########## Параметры моделирования
# уровень значимости
alpha <- 0.05
beta <- 0.2
# квантиль N(0,1) для уровня значимости alpha
quant <- qnorm(1 - alpha / 2)
# вероятности клика по кнопке
pksi <- 0.1 # pcont
peta <- 0.1 # pexp
# необходимый объём выборки в каждой группе
pwr <- power.prop.test(p1 = pksi, p2 = ifelse(peta-pksi!=0, peta, pksi+0.02), 
                       sig.level = alpha,
                       power = 1 - beta,
                       alternative = c("two.sided"),
                       strict = FALSE)
n <- pwr$n

########## Моделирование потока пользователей
usersFlow <- function(N = n, pA = pksi, pB = peta) {
        ksi <- 0
        eta <- 0
        ksiIndex <- 0
        etaIndex <- 0
        # Моделируем посетителей до достижения необходимого колличества "n"
        # в каждой группе
        while (min(length(ksi), length(eta)) < N) {
                # Разделение потока на А и В
                # Если whos = 1, пользователь попадает в группу ksi
                # Если whos = 2, пользователь попадает в группу eta
                whos <- sample(c(1, 2), 1, replace = T, prob = c(0.5, 0.5))
                if (whos == 1) {
                        ksiIndex <- ksiIndex + 1    
                        ksi[ksiIndex] <- sample(c(0, 1), 1, replace = T, 
                                                prob = c(1 - pA, pA))
                } else if (whos == 2) {
                        etaIndex <- etaIndex + 1
                        eta[etaIndex] <- sample(c(0, 1), 1, replace = T, 
                                                prob = c(1 - pB, pB))
                }
        }
        return(list("ksi" = ksi, "eta" = eta))
}
b <- usersFlow()
lapply(b, length)

# attach(b)
# detach(b)
########## Критерий Хи2 для пропорций
chiTest <- function(ksi, eta) {
        # р-pool
        pPool <- (sum(ksi) + sum(eta)) / (length(ksi) + length(eta))
        # Числитель статистики
        chiSNumerator <- sum(ksi) / length(ksi) - sum(eta) / length(eta)
        # Знаменатель статистики
        chiSDenominator <- sqrt(pPool * (1-pPool) * 
                                        (1/length(ksi)+1/length(eta))+0.1^100)
        # Статистика критерия
        se <- chiSNumerator / chiSDenominator
        # p-value для статистики Хи2 при двусторонней альтернативе
        pVal <- 2 * min(pnorm(se, lower.tail = TRUE), 
                        pnorm(se, lower.tail = FALSE))
        # верхняя граница доверительного интервала(при верной нулевой гипотезе)
        upperConf <- quant * chiSDenominator
        
        return(list("pVal" = pVal, "upperConf" = upperConf))
}
install.packages("tictoc")
require(tictoc)

tic()
k <- 500
pValList <- list()
pb <- txtProgressBar(min = 1, max = k, style = 3)
for (i in 1 : k) {
        b <- usersFlow()
        # a <- chiTest(b$ksi, b$eta)$pVal
        pValList[i] <- chiTest(b$ksi, b$eta)$pVal
        setTxtProgressBar(pb, i)
}
close(pb)
toc()

pValList <- do.call(rbind, lapply(pValList, data.frame,stringsAsFactors=FALSE))
summary(pValList)
plot(sort(pValList$X..i..), type = 'l')
abline(h = alpha)
length(which(pValList$X..i.. <= alpha)) / k
