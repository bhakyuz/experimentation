library(ggplot2)
library(dplyr)
library(tidyr)

n <- 10000
# approximates right skewed dist
x <- rbeta(n, 9, 1)

df <- data.frame(x = x*10000)

ggplot(df, aes(x = x)) + 
  geom_density()

df <- df %>% mutate(
  # Lambda minus 1 lm1
  lambda_minus_3 = 1 / x^3 ,
  lambda_minus_2 = 1 / x^2 ,
  lambda_minus_1 = 1 / x ,
  lambda_0 = log(x),
  lambda_plus_1 = x,
  lambda_plus_2 = x^2,
  lambda_plus_3 = x^3
)

df2 <- gather(df, key = "trasformation", value = "value", 
              # lambda_minus_3:lambda_plus_3
              x:lambda_plus_3
              ) %>%
  mutate(
    trasformation = factor(trasformation, levels = unique(trasformation))
  )

ggplot(df2, aes(x = value)) + 
  geom_density() +
  facet_wrap(~trasformation, scales = "free")

ggplot(df, aes(x = x)) + 
  geom_density()

ggplot(df) + 
  geom_density(aes(x = lm3))

ggplot(df) + 
  geom_density(aes(x = lm2))

ggplot(df) + 
  geom_density(aes(x = lm1))

ggplot(df) + 
  geom_density(aes(x = lp3))
