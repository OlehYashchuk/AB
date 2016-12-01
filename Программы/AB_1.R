# Сравнение двух потоков А и В
# Критерий Хи2

rm(list = ls())

options("max.print" = 50)
getOption

# Константы
# кол-во испытаний (точек на графике)
n <- 5000
# уровень значимости
alpha <- 0.05
quant <- qnorm(1 - alpha / 2)
# вероятности клика по кнопке
pcont <- pksi <- 0.1
pexp <- peta <- 0.1
# резервирование выборок
ksi <- eta <- 0

# Гипотеза проверяется для выборок объёма "begin" и больше
begin <- 100

# Вектора числителя и знаменателя статистики Хи квадрат
chiSNumerator <- 0
chiSDenominator <- 0

# Распределитель потока
whos <- 0
# Текущая позиция веток потока
ksiIndex <- 0
etaIndex <- 0
# Текущее i выполнения теста (для удобства)
testIndicator <- 0
# Индекс записи значий статистики в массив (для удобства)
counterNumeratorDenominator <- 0

se <- 0
z <- 0
pVal <- 0
upperConf <- 0

# Основной цикл
for (i in 1 : (2 * n)) {
        # Разделения потока на А и В
        # Если whos = 1, i-e значение добавляется в выбоку ksi
        # Если whos = 2, i-e значение добавляется в выбоку eta
        whos <- sample(c(1, 2), 1, replace = T, prob = c(0.5, 0.5))
        
        if (whos == 1) {
                ksiIndex <- ksiIndex + 1    
                ksi[ksiIndex] <- sample(c(0, 1), 1, replace = T, 
                                        prob = c(1 - pksi, pksi))
        } else if (whos == 2) {
                etaIndex <- etaIndex + 1
                eta[etaIndex] <- sample(c(0, 1), 1, replace = T, 
                                        prob = c(1 - peta, peta))
        }
        
        testIndicator <- testIndicator + 1
        
        # Статистика Хи2 считается на каждом втором шаге
        if ((min(ksiIndex, etaIndex) >= begin) &
            (((testIndicator / 2) - round(testIndicator / 2)) == 0)) {
                
                ppoolemp <- (sum(ksi[1 : ksiIndex]) + sum(eta[1 : etaIndex])) /
                        (length(ksi[1 : ksiIndex]) + length(eta[1 : etaIndex]))
                
                # Индекс
                counterNumeratorDenominator <- counterNumeratorDenominator + 1
                
                # Числитель статистики
                chiSNumerator[counterNumeratorDenominator] <- 
                        sum(ksi[1 : ksiIndex]) / length(ksi[1 : ksiIndex]) - 
                        sum(eta[1 : etaIndex]) / length(eta[1 : etaIndex])
                
                # Знаменатель статистики
                chiSDenominator[counterNumeratorDenominator] <- 
                        sqrt(ppoolemp * (1 - ppoolemp) * 
                                (1 / length(ksi[1 : ksiIndex]) + 
                                 1 / length(eta[1 : etaIndex])) + 
                                0.000000000001^2)
                se[counterNumeratorDenominator] <- chiSNumerator[counterNumeratorDenominator] /
                        chiSDenominator[counterNumeratorDenominator]
                # z[counterNumeratorDenominator] <- quant * 
                        # se[counterNumeratorDenominator]
                pVal[counterNumeratorDenominator] <- 2*pnorm(se[counterNumeratorDenominator], 
                                                           lower.tail = TRUE)
                upperConf[counterNumeratorDenominator] <- quant * 
                        chiSDenominator[counterNumeratorDenominator]
        }
}

# plot(sort(pVal), type = 'l')
# abline(h = alpha)
# 
# # Количество отклонений гипотезы H0 при pval < alpha
# length(which(pVal <= alpha))
# # Количество отклонений гипотезы H0 при Xi2 > Z(1-alpha/2)
# length(which(abs(se) >= quant))
# # Количество отклонений гипотезы H0 при выходе за доверительный интервал
# length(which(chiSNumerator >= confLevels$upper))
# length(which(chiSNumerator <= confLevels$lower))

# Доверительный интервал (-z*SE; z*SE) 
upperConf2 <- qnorm(1 - alpha / 2) * chiSDenominator
sum(upperConf == upperConf2)
length(upperConf2)

lowerConf <- - upperConf

# Доверительный интервал для значения dEmp (Udacity)
# (на графике не строится (можно построить - две строки в конце программы))
upperConfdEmp <- chiSNumerator + qnorm(1 - alpha / 2) * chiSDenominator
lowerConfdEmp <- chiSNumerator - qnorm(1 - alpha / 2) * chiSDenominator

# Запись в список (для удобства)
confLevels <- list(upperConf, lowerConf, upperConfdEmp, lowerConfdEmp)
names(confLevels)[[1]] <- 'upper'
names(confLevels)[[2]] <- 'lower'
names(confLevels)[[3]] <- 'upperdEmp'
names(confLevels)[[4]] <- 'lowerdEmp'

# Частота ошибки I рода
# typeIErrorRate <- cumsum((abs(chiSNumerator) > confLevels$upper) * TRUE) /
#         cumsum(rep(1, length(chiSNumerator)))

# Параметры графиков
lwd.plot <- 1
start <- 1
finish <- max(length(ksi), length(eta))
limX <- finish
limY <- max(confLevels$upper, na.rm = T)


# График поведения (pksiEmp - petaEmp)
plot(chiSNumerator[start:limX], t='l', col='Blue', lwd = lwd.plot,
     lab = c(20, 20, 5),
     las = 0,
     xlim = c(0,limX), ylim = c(- limY, limY), 
     xlab = 'Visitors per group', 
     ylab = 'Conversion Rate Difference',
     main = 'Confidence Interval for Conversion Rate Difference'
)

# Дов интервал (-qvantile*SE; qvantile*SE)
lines(confLevels$upper, col = 'Red', lwd = 1)
lines(confLevels$lower, col = 'Red', lwd = 1)

# Оси Ох, Оу
abline(h = 0)
abline(v = 0)

library(pwr)
pwr <- power.prop.test(p1 = pksi, p2 = ifelse(peta-pksi!=0, pet, pksi+0.02), 
                       sig.level = 0.05,
                       power = 0.8,
                       alternative = c("two.sided"),
                       strict = FALSE)

# Необходимое кол-во посетителей n (в обеих групах)
abline(v = pwr$n, col = "red", lty = 5)
text((pwr$n+650), -0.06, paste("n = ", round(pwr$n))) 

# pwr.2p.test(h = , n = , sig.level =, power = )
# pwr.2p.test(h = 0.02, n = 3533, sig.level = alpha)
# 
# pwr.p.test(h = , n = , sig.level = power = )
# pwr.p.test(h = 0.02, sig.level = alpha, power = 1 - 0.2)
# 
# pwr.chisq.test(w =, N = , df = , sig.level =, power = )
# pwr.chisq.test(w = 0.02, N = 3533, df = 3531, sig.level = alpha)
# 
# pwr.t2n.test(n1 = , n2= , d = , sig.level =, power = )
# pwr.t.test(3533, d = 0.02, sig.level = alpha)
# 
# power.prop.test(p1=8/200, p2=60/2000, power=0.8, sig.level=0.05)
# power.prop.test(p1=pksi, p2=pksi+0.02, power=0.8, sig.level=0.05)
# 
# ?power.t.test
# power.t.test(delta=0.02, sd=0.3, power=0.8, alternative = "two.sided")
# power.t.test(delta=0.02, sd=0.3, power=0.8, alternative = "two.sided", 
#              type = "two.sample")


