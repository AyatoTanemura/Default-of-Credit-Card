# Defult of Credit Cart----

# Library----
library(s20x)
library(tidyverse)
library(here)

# Dataload ----
# df <- read.csv("Data/UCI_Credit_Card.csv", head = T)

df <- read_csv("Data/UCI_Credit_Card.csv")

df.edit <- read_csv("Data/UCI_Credit_Card.csv") %>% 
  mutate(SEX = factor(SEX),
         EDUCATION = factor(EDUCATION),
         MARRIAGE = factor(MARRIAGE),
        SEX = case_when(
                          SEX == 1 ~ 'male',
                          TRUE ~ 'female'),
        EDUCATION = case_when(
                              EDUCATION ==  1 ~ 'graduate school',
                              EDUCATION ==  2 ~ 'university',
                              EDUCATION ==  3 ~'high school',
                              EDUCATION ==  4 ~ 'other',
                              EDUCATION ==  5 ~'unknown',
                              TRUE ~ 'unknown'
                              ),
        MARRIAGE = case_when(
                              MARRIAGE == 1 ~ 'married',
                              MARRIAGE == 2 ~ 'single',
                              TRUE ~ 'others',
                              ),
        default.payment.next.month = case_when(
                                                default.payment.next.month == 1 ~ 'default',
                                                default.payment.next.month == 0 ~ 'no default',
                                                TRUE ~ 'NA'
                                                  ),
        AGEGROUP = case_when(
                              AGE <= 10 ~ '~10',
                              AGE > 10 & AGE <= 20 ~ '11~20',
                              AGE > 20 & AGE <= 30 ~ '21~30',
                              AGE > 30 & AGE <= 40 ~ '31~40',
                              AGE > 40 & AGE <= 50 ~ '41~50',
                              AGE > 50 & AGE <= 60 ~ '51~60',
                              AGE > 60 & AGE <= 70 ~ '61~70',
                              AGE > 70 & AGE <= 80 ~ '71~80',
                              TRUE ~ '81~',
                              ),
        AGEGROUP = factor(AGEGROUP)
        )


names(df.edit)

# Data Transform using base R
df$SEX <- factor(df$SEX)
str(df)

# Class----
class(df)
class(read.csv("Data/UCI_Credit_Card.csv", header = T))

# SUmmary----
summary(df)
dim(df)
nrow(df)
ncol(df)

sum(complete.cases(df)) #check rows with missing value

# Histogram----
hist(df$AGE)

plot(LIMIT_BAL~AGE, data = df.edit)

ggplot(data = df.edit) + geom_point(aes(x = AGE, y = LIMIT_BAL))

ggplot(data = df.edit) + 
  geom_point(aes(x = AGE, y = LIMIT_BAL, col = SEX)) +
               facet_wrap(~SEX)

ggplot(data = df.edit) + geom_hex(aes(x = AGE, y = LIMIT_BAL))


count(df.edit, LIMIT_BAL, AGE, SEX, MARRIAGE) %>% 
  ggplot(.)+ 
  geom_point(aes(x = AGE, y = LIMIT_BAL, size = n, col = SEX)) +
  facet_wrap(MARRIAGE~SEX)

names(df)
count(df.edit, LIMIT_BAL, AGE, SEX, MARRIAGE, default.payment.next.month) %>% 
  ggplot(.) +
  geom_point(aes(x = AGE, y = LIMIT_BAL, size = n, col = factor(default.payment.next.month))) +
  facet_wrap(MARRIAGE~SEX)
## Practice----
# geom_density2d

count(df.edit, LIMIT_BAL, AGE, SEX, MARRIAGE, default.payment.next.month) %>% 
  ggplot(.) +
  geom_density2d(aes(x = AGE, y = LIMIT_BAL)) +
  facet_wrap(MARRIAGE~SEX)

count(df.edit, LIMIT_BAL, AGE, SEX, MARRIAGE, default.payment.next.month) %>% 
  ggplot(.) +
  geom_density2d(aes(x = AGE, y = LIMIT_BAL)) +
  facet_wrap(~factor(default.payment.next.month))

# multiGG

names(df.edit)
count(df.edit, LIMIT_BAL, AGE, SEX, MARRIAGE, default.payment.next.month) %>% 
  ggplot(.) +
  geom_point(aes(x = AGE, y = LIMIT_BAL, size = n, col = factor(default.payment.next.month))) +
  geom_smooth(method = lm, aes(x = AGE, y = LIMIT_BAL, col = factor(default.payment.next.month))) +
  facet_wrap(SEX~factor(default.payment.next.month))

# Bar Chart

count(df.edit, LIMIT_BAL, AGE, SEX, MARRIAGE, default.payment.next.month) %>% 
  ggplot(.) +
  geom_bar(aes(x = LIMIT_BAL, col = SEX)) +
  facet_wrap(factor(default.payment.next.month)~SEX)


# Linear Model----
# reference p165 Stats20x

## Plot---- 

ggplot(data = df) + geom_point(aes(x = AGE, y = LIMIT_BAL))

count(df.edit, LIMIT_BAL, AGE, SEX, MARRIAGE, default.payment.next.month) %>% 
  ggplot(.) +
  geom_point(aes(x = AGE, y = LIMIT_BAL)) +
  facet_wrap(~ default.payment.next.month)

# Box plot
count(df.edit, LIMIT_BAL, AGE, SEX, MARRIAGE, default.payment.next.month, AGEGROUP) %>% 
  ggplot(.) +
  geom_boxplot(aes(x = AGEGROUP, y = LIMIT_BAL))  +
  facet_wrap(~ default.payment.next.month)

count(df.edit, LIMIT_BAL, AGE, SEX, MARRIAGE, default.payment.next.month, AGEGROUP) %>% 
  ggplot(.) +
  geom_boxplot(aes(x = default.payment.next.month, y = AGE)) 

# Start modeling with this

count(df.edit, LIMIT_BAL, AGE, SEX, MARRIAGE, default.payment.next.month, AGEGROUP) %>% 
  ggplot(.) +
  geom_boxplot(aes(x = default.payment.next.month, y = LIMIT_BAL)) 

## Assumption Check----

default.fit <- lm(LIMIT_BAL ~ default.payment.next.month, data = df.edit)

### EOV assumption----

plot(default.fit, which = 1)

### normality assumption----

normcheck(default.fit)

### Unduly influential point----

cooks20x(default.fit)

### ANOVA----

anova(default.fit)

### One way analysis----

summary1way(default.fit, draw.plot = FALSE)

### summary----

summary(default.fit)






























