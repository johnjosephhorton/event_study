library(ggplot2)
library(dplyr)
library(magrittr)
library(lfe)

SimulateStaticPanel <- function(T, N, sigma, beta){
    index <- 1:N
    time <- 1:T
    unit.effects <- rnorm(N)
    time.effects <- rnorm(T)
    unit.time.trends <- rnorm(N)
    epsilon <- rnorm(N*T, 0, sigma)
    df <- expand.grid(index = index, time = time) %>%
        group_by(index) %>% 
        mutate(treatment.date = sample(1:T,1)) %>%
        mutate(treated = time > treatment.date) %>%
        mutate(t = as.numeric(time))
    df$y <- with(df, beta * treated + unit.effects[index] + time.effects[time] + unit.time.trends[index] * time + epsilon) 
    df
}


EstimateBeta <- function(df){ 
   m <- felm(y ~ treated | index + factor(index):t + time | 0 | index, data = df)
   coef(m)["treatedTRUE"]
}

betas <- replicate(100, EstimateBeta(SimulateStaticPanel(20, 100, 0.1, 1))) %>% as.numeric


df <- SimulateStaticPanel(20, 100, 0.1, 1)
EstimateBeta(df)


g <- ggplot(data = df, aes(x = time, y = y, group = index, colour = factor(treated))) + geom_line() +
    facet_wrap(~treatment.date, ncol = 5)


SimulateStaticPanelTrend <- function(T, N, sigma, beta, gamma){
    index <- 1:N
    time <- 1:T
    unit.effects <- rnorm(N)
    time.effects <- rnorm(T)
    unit.time.trends <- rnorm(N)
    epsilon <- rnorm(N*T, 0, sigma)
    df <- expand.grid(index = index, time = time) %>%
        group_by(index) %>% 
        mutate(treatment.date = sample(1:T,1)) %>%
        mutate(treated = time > treatment.date) %>%
        mutate(relative.time = ifelse(time > treatment.date, time - treatment.date, 0), 
               t = as.numeric(time)
               )
    df$y <- with(df, gamma * relative.time + beta * treated + unit.effects[index] + time.effects[time] + unit.time.trends[index] * time + epsilon) 
    df
}


df <- SimulateStaticPanelTrend(20, 100, 0.1, 1, 0.1)

m <- felm(y ~ treated + relative.time | index + factor(index):t + time | 0 | index, data = df)
summary(m)
