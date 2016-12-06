# Множественная проверка гипотез при А/А тестирование

rm(list = ls())

getwd()
setwd("d:/D/AB/Программы/AA тестирование")
# setwd("d:/D/Coursera/AB-testing/AB/Программы")

source("usersFlow.R")
source("chiTest.R")
source("workHorse.R")
source("correction.R")
source("draw.R")

# options("max.print" = 50)
# getOption

library(pwr)
library(tictoc)
library(ggplot2)

########## Параметры моделирования
# количество разбиений потока пользователей
k <- 5
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

a <- list()
# Запуск всей программы
a <- workHorse(k, n, pksi, peta, alpha)
# sapply(a, length)
# Частота ошибки первого рода без поправки на множественность
# a$FPR
# Частота ошибки первого рода с поправкой Бонферрони
# a$FPRB
# График p-values
# draw(a)

a$p.adj.Matrix
