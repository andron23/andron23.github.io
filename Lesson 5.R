library(readr)

data <- read_csv(file = 'Lesson 5 dataset.csv')

data <- 
  data %>% 
  unite(racket, shuttlecock, col = "racket_shuttlecock")

library(ggplot2)

# Как изменялись продажи

gg1 <- 
  data %>% 
  mutate(month = month(date)) %>% 
  group_by(month, racket_shuttlecock) %>%
  summarise(sold = n()) %>% 
  ggplot() +
  geom_line(aes(y = sold, x = month, col = racket_shuttlecock)) +
#  facet_grid(~racket_shuttlecock) + 
  scale_x_continuous(breaks = 1:12)

library(patchwork)

# Как менялась цена

gg2 <- 
  data %>% 
  ggplot() +
  geom_line(aes(x = date, y = price, col = racket_shuttlecock))

gg1/gg2

#Проверяем корреляцию между изменением цены и продажами наборов численно

data %>% 
  mutate(month = month(date)) %>% 
  group_by(month, racket_shuttlecock) %>%
  mutate(sold = n()) %>% 
  group_by(racket_shuttlecock) %>% 
  summarise(correlation = cor(x = price, y = sold))

#Проверяем корреляцию между изменением цены и продажами наборов на графике

data %>% 
  mutate(month = month(date)) %>% 
  group_by(month, racket_shuttlecock) %>%
  mutate(sold = n()) %>% 
  ggplot() + 
  geom_point(aes(x = price, y = sold, col = racket_shuttlecock))

data %>% 
  mutate(month = month(date)) %>% 
  group_by(month, racket_shuttlecock) %>%
  mutate(sold = n()) %>% 
  ggplot() + 
  geom_point(aes(x = price, y = sold, col = racket_shuttlecock)) + 
  geom_smooth(aes(x = price, y = sold, col = racket_shuttlecock), method = lm, formula = y ~ x) + 
  facet_grid(~racket_shuttlecock)




data1 <- 
  data %>% 
  mutate(month = month(date)) %>% 
  group_by(month, racket_shuttlecock) %>%
  mutate(sold = n())

fit <- lm(data = data1[-2] %>% filter(racket_shuttlecock == "1_2"), sold ~ price)

summary(fit)