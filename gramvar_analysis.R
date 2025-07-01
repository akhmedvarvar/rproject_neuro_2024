# загружаем библиотеки
library(nortest)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(modelr)
library(lmtest)
library(lme4)
library(lmerTest)
library(sjPlot)
library(tidyr) 

# загружаем данные
setwd('C:/Users/kozlo/Downloads')
gramvar_data <- read.csv("gramvar_data.csv")
stimuli_data <- read.csv("stimuli.csv")

# ПРЕДОБРАБОТКА
# Преобразование stimuli_data в длинный формат
stimuli_data <- stimuli_data %>%
  pivot_longer(cols = -id_sentence, 
               names_to = "SentenceID", 
               values_to = "Value") %>%
  pivot_wider(names_from = id_sentence, values_from = Value)

# Преобразование gramvar_data в длинный формат
gramvar_data <- gramvar_data %>%
  pivot_longer(cols = starts_with("winter_") | starts_with("summer_"), 
               names_to = "SentenceID", 
               values_to = "Грамматичность")

# Объединение данных
merged_data <- gramvar_data %>%
  left_join(stimuli_data, by = "SentenceID")

# Фильтрация данных: удаляем строки, где участник не оценивал предложение
merged_data <- merged_data %>%
  filter((Условие == "Лето" & grepl("^summer_", SentenceID)) |
           (Условие == "Зима" & grepl("^winter_", SentenceID)))

# Переименуем столбцы, чтобы R не ругался на пробел
colnames(merged_data)[colnames(merged_data) == "Длина составляющей"] <- "Длина_составляющей"
colnames(merged_data)[colnames(merged_data) == "Вынос ремы"] <- "Вынос_ремы"

# Преобразование столбцов в факторы
merged_data$Грамматичность <- as.factor(merged_data$Грамматичность)
merged_data$Регистр <- as.factor(merged_data$Регистр)
merged_data$Условие <- as.factor(merged_data$Условие)
# Преобразуем столбцы в числовой формат
merged_data$Длина_составляющей <- as.numeric(merged_data$Длина_составляющей)
merged_data$Расстояние <- as.numeric(merged_data$Расстояние)
merged_data$Вынос_ремы <- as.numeric(merged_data$Вынос_ремы)
merged_data$Пауза <- as.numeric(merged_data$Пауза)
write.csv(merged_data, "merged_data.csv", row.names = FALSE)

############# АНАЛИЗ ##############
merged_data <- read.csv("merged_data.csv")
# 1. Описательная статистика
summary(merged_data)
str(merged_data)

# Распределение оценок грамматичности
library(ggplot2)

ggplot(merged_data, aes(x = Грамматичность)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Распределение оценок грамматичности", x = "Грамматичность", y = "Кол-во наблюдений") +
  theme_minimal()

# Средние оценки грамматичности по условиям
library(dplyr)

merged_data %>%
  group_by(Условие) %>%
  summarize(
    MeanGrammaticality = mean(as.numeric(as.character(Грамматичность)), na.rm = TRUE),
    SDGrammaticality = sd(as.numeric(as.character(Грамматичность)), na.rm = TRUE)
  )

############# Модели #############

# Порядковая смешанная модель
# Порядковая тк грамматичность -- порядковая категориальная переменная 
# (оценка от 1 до 5).
# В случайные эффекты записываем ID участника,
# так мы учитываем межличностную вариацию.

library(ordinal)
merged_data$Грамматичность <- as.factor(merged_data$Грамматичность)

ordinal_mixed_model <- clmm(Грамматичность ~ Длина_составляющей + Расстояние + Вынос_ремы + Пауза + Регистр + (1 | ID_participant), 
                            data = merged_data)

summary(ordinal_mixed_model)

# Полная модель
model_full <- clmm(Грамматичность ~ Длина_составляющей + Расстояние + Вынос_ремы + Пауза + Регистр + (1 | ID_participant) + (1 | SentenceID), 
                   data = merged_data)

summary(model_full)

# Функция для построения моделей с исключением переменных
stepwise_model <- step(model_full, direction = "both", trace = 1)

# Результат — оптимальная модель с наименьшим AIC
summary(stepwise_model)

tab_model(ordinal_mixed_model,
          dv.labels = c("Результаты смешанной порядковой модели"),
          string.p = "p-value")


# Линейная модель для средних значений 
library(dplyr)

# Вычисляем среднюю оценку для каждого предложения
avg_scores <- merged_data %>%
  group_by(SentenceID) %>%
  summarize(
    MeanGrammaticality = mean(as.numeric(as.character(Грамматичность)), na.rm = TRUE)
  )

# Объединяем с таблицей характеристик предложений
analysis_data <- avg_scores %>%
  left_join(stimuli_data, by = c("SentenceID" = "SentenceID"))

colnames(analysis_data)[colnames(analysis_data) == "Длина составляющей"] <- "Длина_составляющей"
colnames(analysis_data)[colnames(analysis_data) == "Вынос ремы"] <- "Вынос_ремы"

# Преобразование столбцов в факторы
analysis_data$Регистр <- as.factor(analysis_data$Регистр)
# Преобразуем столбцы в числовой формат
analysis_data$Длина_составляющей <- as.numeric(analysis_data$Длина_составляющей)
analysis_data$Расстояние <- as.numeric(analysis_data$Расстояние)
analysis_data$Вынос_ремы <- as.numeric(analysis_data$Вынос_ремы)
analysis_data$Пауза <- as.numeric(analysis_data$Пауза)
write.csv(analysis_data, "analysis_data.csv", row.names = FALSE)

# Линейная регрессия
linear_model <- lm(MeanGrammaticality ~ Длина_составляющей + Расстояние + Вынос_ремы + Пауза + Регистр, data = analysis_data)

# Результаты модели
summary(linear_model)

tab_model(linear_model,
          dv.labels = c("Результаты линейной модели"),
          string.p = "p-value")

ggplot(analysis_data, aes(x = Пауза, y = MeanGrammaticality)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Влияние паузы на среднюю оценку грамматичности предложения",
       x = "Пауза", y = "Средняя оценка грамматичности") +
  theme_minimal()


