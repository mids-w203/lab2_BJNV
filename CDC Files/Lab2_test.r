library(tidyverse)
library(haven)
brfss <- read_xpt("/Users/nanaedusah/Documents/GitHub/Statistics/Lab2/CDC Files/LLCP2023.XPT ")
view(head(brfss,100))
df <- brfss %>%
  select(MENTHLTH, INCOME3, LADULT1,EXERANY2, MARITAL, EDUCA,VETERAN3,EMPLOY1) %>%
  filter(MENTHLTH <= 30 | MENTHLTH == 88, INCOME3>=1 & INCOME3 <= 12, 
         LADULT1!= 1) %>%
  mutate(
    MENTHLTH = ifelse(MENTHLTH == 88, 0, MENTHLTH), #if number of mental healthdays is none (88), chnage value to 0
    INCOME3_label = case_when(
      INCOME3 == 1 ~ "$10,000",
      INCOME3 == 2 ~ "$12,500",
      INCOME3 == 3 ~ "$17,500",
      INCOME3 == 4 ~ "$22,500",
      INCOME3 == 5 ~ "$30,000",
      INCOME3 == 6 ~ "$40,000",
      INCOME3 == 7 ~ "$62,500",
      INCOME3 == 8 ~ "$87,500",
      INCOME3 == 9 ~ "$125,000",
      INCOME3 == 10 ~ "$175,000",
      INCOME3 == 11 ~ "$200,000",
      INCOME3 == 77 ~ "N/A",
      INCOME3 == 99 ~ "NA",
      is.na(INCOME3) ~ "N/A",
      TRUE ~ as.character(INCOME3)
    )
  ) %>%
view(head(df,100))
summary(df)
ggplot(df, aes(x = INCOME3_label, y = MENTHLTH)) +
  geom_point(alpha =0.2)+
  labs(title = "Mental Health Days by Income Level",
       x = "Income Level",
       y = "Mental Health Days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
modle1 <- lm(MENTHLTH ~ INCOME3_label, data = df)
summary(modle1)
#data.frame(
  #column = names(brfss),
  #label = sapply(brfss, function(x) attr(x, "label")),
  #stringsAsFactors = FALSE
# Bar chart: Income Level vs. Number of Mental Health Days Reported
library(ggplot2)
ggplot(df, aes(x = INCOME3_label, fill = as.factor(MENTHLTH))) +
  geom_bar(position = "dodge") +
  labs(title = "Income Level vs. Number of Mental Health Days Reported",
       x = "Income Level",
       y = "Count",
       fill = "Mental Health Days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Bar chart: Income Level vs. Average Number of Mental Health Days
library(ggplot2)
df_avg <- df %>%
  group_by(INCOME3_label) %>%
  summarise(avg_menthlth = mean(MENTHLTH, na.rm = TRUE))
ggplot(df_avg, aes(x = INCOME3_label, y = avg_menthlth)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Number of Mental Health Days by Income Level",
       x = "Income Level",
       y = "Average Mental Health Days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
