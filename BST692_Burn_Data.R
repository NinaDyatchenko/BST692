
############## Descriptive stats on all variables in Burn1000
## ggplots or simple plots

# Downloading data and packages
library(aplore3)
library(ggplot2)
data(burn1000)

## Exploring data burn1000:
str(burn1000)  # structure of parameters in the data
summary(burn1000)  # info on all parameters
summary(burn1000$id) # there are 1000 participants

## Age:
summary(burn1000$age)  # min, Q1, Median, Mean, Q3, Max.

Agevar <- ggplot(burn1000, aes(x=age)) + 
  geom_histogram(color="black", fill="white") +
  geom_vline(aes(xintercept = median(age)), 
             color = 'blue', 
             linetype = "dashed") +
  theme_minimal() +
  geom_text(aes(x = median(age) + 2, 
                label = "Median age", 
                y = 90), 
            color = "blue", 
            angle = 90) +
  scale_y_continuous(breaks = seq(0, 105, 5)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "Histogram of Age", x = "Age", y = "Count")
Agevar

## Gender:
summary(burn1000$gender) # 295 Females, 705 Males

GendVar <- ggplot(data = burn1000, aes(x = gender, fill = gender)) + 
  geom_bar() +

  scale_y_continuous(breaks = seq(from = 0, to = 750, by = 100)) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = 1) +
  labs(title = "Gender", x = "", y = "Count")
GendVar

## Race:
summary(burn1000$race) # 411 Non-white, 589 White

RaceVar <- ggplot(data = burn1000, aes(x = race)) + 
  geom_bar(fill = c("lightgray", "darkgray"), width = 0.6) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(from = 0, to = 750, by = 100)) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = 2) +
  labs(title = "Race", x = " ", y = "Count")
RaceVar

## Gender by Race
ggplot(data = burn1000, aes(x = race, fill = gender)) +
  geom_bar(stat = 'count', position = position_dodge()) +
  labs(title = "Race by Gender", x = " ", y = "Count")


## Total burn surface area (0 - 100%):
summary(burn1000$tbsa)  # Min, Q1, Median 6%, Mean 13.54%, Q3, Max. 98%

ggplot(burn1000, aes(tbsa)) +
  geom_histogram() +
  #scale_fill_distiller(palette = "Reds") +
  stat_bin(geom = "text", aes(label = ..count..), vjust = -0.4) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  labs(title = "Total burn surface area", 
       x = "Total burn surface area (%)", 
       y = "Count")


## Burn involved inhalation injury (1: No, 2: Yes)
summary(burn1000$inh_inj)  # 878 Inhaled, 122 Didn't inhale

InhInjVar <- ggplot(data = burn1000, aes(x = inh_inj)) + 
  geom_bar(fill = c("lightgray", "darkgray"), width = 0.6) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(from = 0, to = 900, by = 100)) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = 2) +
  labs(title = "Burn involved inhalation injury", 
       x = "Burn involved inhalation injury", 
       y = "Count")
InhInjVar


## Flame involved in burn injury (1: No, 2: Yes)
summary(burn1000$flame)  # 878 Inhaled, 122 Didn't inhale

FlameVar <- ggplot(data = burn1000, aes(x = flame)) + 
  geom_bar(fill = c("lightgray", "darkgray"), width = 0.6) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(from = 0, to = 900, by = 100)) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = 2) +
  labs(title = "Flame involved in burn injury", 
       x = "Flame involved in burn injury", 
       y = "Count")
FlameVar

## Inhalation by Flame exposure
ggplot(data = burn1000, aes(x = inh_inj, fill = flame)) +
  geom_bar(stat = 'count', 
           position = position_dodge(width = 1)) +
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(1), vjust = -0.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 500, by = 100)) +
  labs(title = "Inhalation by Flame exposure", x = "Inhalation", y = "Count")

  

## Death
summary(burn1000$death)  # 850 Alive, 150 Dead

DeathVar <- ggplot(data = burn1000, aes(x = death)) + 
  geom_bar(fill = c("lightgray", "red"), width = 0.6) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(from = 0, to = 900, by = 100)) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = 2) +
  labs(title = "Life status", 
       x = "Status", 
       y = "Count")
DeathVar

## Life status vs. Flame
ggplot(data = burn1000, aes(x = death, fill = flame)) +
  geom_bar(stat = 'count', 
           position = position_dodge(width = 1)) +
  scale_fill_manual(name = "flame", values = c("gray", "red")) +
  geom_text(stat = 'count', aes(label = ..count..), 
            position = position_dodge(1), vjust = -0.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 500, by = 100)) +
  labs(title = "Life status by Flame exposure", x = "Status", y = "Count")


ggplot(aes(y = age, x = race, fill = gender), data = burn1000) + geom_boxplot()

boxplot(tbsa~facility,
        data=burn1000,
        main="Different boxplots for each month",
        xlab="Month Number",
        ylab="Degree Fahrenheit",
        col="orange",
        border="brown"
)


