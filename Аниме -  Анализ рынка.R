library(ggplot2)
library(dplyr)
library(tidyr)
library(moments)
library(factoextra)
library(Hmisc)      # Для rcorr, который дает p-value для корреляций
library(corr)      # Для удобного отображения корреляций
library(ggcorrplot)

# Работа с реальными данными
df<-read.csv(file.choose(), dec=",")
df<-na.omit(df)

# Проверка текстовых столбцов на пустые строки или NA
is_na_or_empty <- function(row) {
  text_columns <- sapply(row, is.character)
  any(is.na(row[text_columns]) | row[text_columns] == "")
}

# Создаем таблицу без пустых строк и NA
df <- df[!apply(df, 1, is_na_or_empty), ]
#перобразование данных
df$score <- gsub(",", ".", df$score)
df$score <- as.numeric(df$score)

#Преобразование столбца isreal в числовой формат
df$isreal <- ifelse(df$isreal == "True", 1, 
                    ifelse(df$isreal == "False", 0, NA))

#Удаление ненужных столбцов
df <- df[, !(names(df) %in% c("Erotica", "Hentai"))]

# Функция для преобразования значений в целочисленный формат
convert_to_int <- function(value) {
  # Проверяем наличие знака '/'
  if (grepl("/", value)) {
    # Если есть знак '/', проверяем наличие знака '?'
    if (grepl("\\?", value)) {
      # Если есть знак '?', берем часть до знака '/'
      part_before_slash <- sub("(.*?) /.*", "\\1", value)
      int_value <- suppressWarnings(as.integer(trimws(part_before_slash)))
    } else {
      # Если знака '?' нет, берем часть после знака '/'
      part_after_slash <- sub(".*?/(.*)", "\\1", value)
      int_value <- suppressWarnings(as.integer(trimws(part_after_slash)))
    }
  } else {
    # Если нет знака '/', просто пробуем преобразовать в целое число
    int_value <- suppressWarnings(as.integer(trimws(value)))
  }
  
  return(int_value)
}


# Применяем функцию ко всему столбцу
df$epis <- sapply(df$epis, convert_to_int)

# Подготовка данных
df$reit <- gsub("PG-13", 3, df$reit)
df$reit <- gsub("PG", 2, df$reit)
df$reit <- gsub("G", 1, df$reit)
df$reit <- gsub("R-17", 4, df$reit)
df$reit <- gsub("R\\+", 5, df$reit)  # Используем двойной слэш для экранирования знака '+'
df$reit <- gsub("Rx", 6, df$reit)

# Преобразуем в целые числа
df$reit <- as.integer(df$reit)

#выбросы
# 1. Здравый смысл
# Проверка на выбросы
outliers_sense_rating <- df$raiting[df$raiting < 0]
outliers_sense_data <- df$data[df$data < 0]
outliers_sense_epis <- df$epis[df$epis < 0]
outliers_sense_isreal <- df$isreal[!(df$isreal %in% c(0, 1))]
outliers_sense_genres <- lapply(df[, c("Avant.garde", "Gourmet", "Drama", "Comedy", 
                                       "Slice.of.Life", "Adventure", "Romance", 
                                       "Supernatural", "Sports", "Mystery", 
                                       "Thriller", "Horror", "Science.Fiction", 
                                       "Fantasy", "Action", "Ecchi", "Shounen", 
                                       "Shoujo", "Seinen", "Josei", "Kids")], 
                                function(x) x[!(x %in% c(0, 1))])
outliers_sense_reit <- df$reit[df$reit < 1 | df$reit > 6]
outliers_sense_leng <- df$leng[df$leng < 0]

# Вывод результатов
cat("Выбросы для 'rating':", outliers_sense_rating, "\n")
cat("Выбросы для 'data':", outliers_sense_data, "\n")
cat("Выбросы для 'epis':", outliers_sense_epis, "\n")
cat("Выбросы для 'isreal':", outliers_sense_isreal, "\n")
cat("Выбросы для жанров:\n")
for (genre in names(outliers_sense_genres)) {
  if (length(outliers_sense_genres[[genre]]) > 0) {
    cat(paste("  ", genre, ":", outliers_sense_genres[[genre]], "\n"))
  }
}
cat("Выбросы для 'reit':", outliers_sense_reit, "\n")
cat("Выбросы для 'leng':", outliers_sense_leng, "\n")

# 2. Анализ гистограмм
par(mfrow = c(1, 2))  # Устанавливаем 2x2 графика
hist(df$data, main = "Гистограмма рейтинга", xlab = "Году выпуска", breaks = 20, col = "blue")
hist(df$score, main = "Гистограмма оценки", xlab = "Оценка", breaks = 20, col = "green")
hist(df$epis, main = "Гистограмма эпизодов", xlab = "Эпизоды", breaks = 20, col = "red")
hist(df$leng, main = "Гистограмма длины", xlab = "Длина", breaks = 20, col = "purple")

# 3. Анализ ящичковых диаграмм
par(mfrow = c(1, 2))  # Устанавливаем 2x2 графика
boxplot(df$data, main = "Ящичковая диаграмма рейтинга", ylab = "Году выпуска", col = "lightblue")
boxplot(df$score, main = "Ящичковая диаграмма оценки", ylab = "Оценка", col = "lightgreen")
boxplot(df$epis, main = "Ящичковая диаграмма эпизодов", ylab = "Эпизоды", col = "lightcoral")
boxplot(df$leng, main = "Ящичковая диаграмма длины", ylab = "Длина", col = "lightyellow")


# Функция для удавления выбросов
remove_outliers <- function(data, column) {
  # Проверяем, существует ли указанный столбец в датафрейме
  if (!column %in% names(data)) {
    stop("Указанный столбец не существует в датафрейме.")
  }
  
  # Выявляем выбросы
  outliers <- boxplot(data[[column]], plot = FALSE)$out
  
  # Удаляем выбросы
  data_no_outliers <- data[!data[[column]] %in% outliers, ]
  
  return(data_no_outliers)
}

# Удаляем выбросы для указанных переменных
df_no_outliers <- df  # Создаем копию исходного датафрейма

# Удаляем выбросы по каждой переменной
df_no_outliers <- remove_outliers(df_no_outliers, "data")
df_no_outliers <- remove_outliers(df_no_outliers, "score")
df_no_outliers <- remove_outliers(df_no_outliers, "epis")
df_no_outliers <- remove_outliers(df_no_outliers, "leng")

# Проверяем результат
cat("Количество наблюдений после удаления выбросов:", nrow(df_no_outliers), "\n")

# Визуализация гистограмм после удаления выбросов
par(mfrow = c(1, 2))  # Устанавливаем 1x2 графика
hist(df_no_outliers$data, main = "Гистограмма рейтинга (без выбросов)", xlab = "Году выпуска", breaks = 20, col = "blue")
hist(df_no_outliers$score, main = "Гистограмма оценки (без выбросов)", xlab = "Оценка", breaks = 20, col = "green")
hist(df_no_outliers$epis, main = "Гистограмма эпизодов (без выбросов)", xlab = "Эпизоды", breaks = 20, col = "red")
hist(df_no_outliers$leng, main = "Гистограмма длины (без выбросов)", xlab = "Длина", breaks = 20, col = "purple")
par(mfrow = c(1, 1))

df<-df_no_outliers

#вывод метрики для количественных данных
# Определяем функцию для расчета описательных статистик
calculate_descriptive_stats <- function(data, column) {
  # Проверяем, существует ли указанный столбец в датафрейме
  if (!column %in% names(data)) {
    stop("Указанный столбец не существует в датафрейме.")
  }
  
  # Извлекаем данные из указанного столбца
  values <- data[[column]]
  
  # Рассчитываем описательные статистики
  mean_value <- mean(values, na.rm = TRUE)
  variance_value <- var(values, na.rm = TRUE)
  sd_value <- sd(values, na.rm = TRUE)
  median_value <- median(values, na.rm = TRUE)
  
  # Рассчитываем моду
  mode_value <- as.numeric(names(sort(table(values), decreasing = TRUE)[1]))
  
  # Рассчитываем квартильные значения
  q1_value <- quantile(values, 0.25, na.rm = TRUE)
  q3_value <- quantile(values, 0.75, na.rm = TRUE)
  
  # Рассчитываем коэффициент вариации
  cv_value <- (sd_value / mean_value) * 100
  
  # Рассчитываем асимметрию
  asim_value <- sum((values - mean_value)^3, na.rm = TRUE) / 
    (length(values) * sd_value^3)
  
  # Рассчитываем эксцесс
  exc_value <- sum((values - mean_value)^4, na.rm = TRUE) / 
    (length(values) * sd_value^4) - 3
  
  # Создаем список с результатами
  stats <- list(
    Среднее = mean_value,
    Дисперсия = variance_value,
    Стандартное_отклонение = sd_value,
    Медиана = median_value,
    Мода = mode_value,
    Q1 = q1_value,
    Q3 = q3_value,
    Коэффициент_вариации = cv_value,
    Асимметрия = asim_value,
    Эксцесс = exc_value
  )
  
  # Выводим название фактора
  cat("Статистики для:", column, "\n")
  
  # Форматируем вывод
  for (stat_name in names(stats)) {
    cat(stat_name, ":", stats[[stat_name]], "\n")
  }
}

calculate_descriptive_stats(df, "data")
calculate_descriptive_stats(df, "epis")
calculate_descriptive_stats(df, "score")
calculate_descriptive_stats(df, "leng")


# Определяем функцию для расчета описательных статистик для качественных данных
calculate_descriptive_stats_categorical <- function(data, columns) {
  # Проверяем, существуют ли указанные столбцы в датафрейме
  for (column in columns) {
    if (!column %in% names(data)) {
      stop(paste("Указанный столбец", column, "не существует в датафрейме."))
    }
  }
  
  # Создаем список для хранения результатов
  all_stats <- list()
  
  # Проходим по каждому столбцу и рассчитываем статистики
  for (column in columns) {
    # Извлекаем данные из указанного столбца
    values <- data[[column]]
    
    # Рассчитываем частоты
    freq_table <- table(values)
    
    # Рассчитываем пропорции
    prop_table <- prop.table(freq_table) * 100
    
    # Рассчитываем моду
    mode_value <- names(freq_table[which.max(freq_table)])
    
    # Сохраняем результаты в список
    all_stats[[column]] <- list(
      Частоты = freq_table,
      Пропорции = prop_table,
      Мода = mode_value
    )
  }
  
  # Выводим результаты
  for (column in columns) {
    cat("Статистики для:", column, "\n")
    for (stat_name in names(all_stats[[column]])) {
      cat(stat_name, ":\n")
      print(all_stats[[column]][[stat_name]])
      cat("\n")
    }
    cat("====================================\n")  # Разделитель между жанрами
  }
}

# Вызовы функции для качественных данных
# Указываем все нужные столбцы в одном векторе
columns_to_analyze <- c("isreal", "reit", "Avant.garde", "Gourmet", "Drama", 
                        "Comedy", "Slice.of.Life", "Adventure", "Romance", 
                        "Supernatural", "Sports", "Mystery", "Thriller", 
                        "Horror", "Science.Fiction", "Fantasy", "Action", 
                        "Ecchi", "Shounen", "Shoujo", "Seinen", "Josei", 
                        "Kids")

# Вызываем функцию для всех указанных столбцов
calculate_descriptive_stats_categorical(df, columns_to_analyze)

# Преобразуем данные в длинный формат
genre_data <- df %>%
  select(Avant.garde, Gourmet, Drama, Comedy, Slice.of.Life, 
         Adventure, Romance, Supernatural, Sports, Mystery, 
         Thriller, Horror, Science.Fiction, Fantasy, Action, 
         Ecchi, Shounen, Shoujo, Seinen, Josei, Kids) %>%
  pivot_longer(cols = everything(), names_to = "Genre", values_to = "Value") %>%
  filter(Value == 1) %>%
  group_by(Genre) %>%
  summarise(Count = n()) %>%
  ungroup()

# Общее количество аниме
total_count <- nrow(df)

# Добавляем столбец с долей
genre_data <- genre_data %>%
  mutate(Share = Count / total_count * 100)  # Доля в процентах

# Создаем отдельные пироги для каждого жанра
genre_data_long <- genre_data %>%
  mutate(Other = 100 - Share) %>%
  select(Genre, Share, Other) %>%
  pivot_longer(cols = c(Share, Other), names_to = "Type", values_to = "Value") %>%
  mutate(Type = ifelse(Type == "Share", Genre, "Другие"))

# Устанавливаем яркие цвета для жанров и прозрачный для "Другие"
colors <- c(
  "Avant.garde" = "red",
  "Gourmet" = "orange",
  "Drama" = "yellow",
  "Comedy" = "green",
  "Slice.of.Life" = "blue",
  "Adventure" = "purple",
  "Romance" = "pink",
  "Supernatural" = "cyan",
  "Sports" = "magenta",
  "Mystery" = "brown",
  "Thriller" = "lightblue",
  "Horror" = "darkred",
  "Science.Fiction" = "darkgreen",
  "Fantasy" = "lightgreen",
  "Action" = "lightcoral",
  "Ecchi" = "lightpink",
  "Shounen" = "lightyellow",
  "Shoujo" = "lightgray",
  "Seinen" = "lightblue",
  "Josei" = "lightcyan",
  "Kids" = "lightgoldenrod",
  "Другие" = "transparent"  # Прозрачный цвет для "Другие"
)

# Создаем круговую диаграмму для каждого жанра
ggplot(genre_data_long, aes(x = "", y = Value, fill = Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Доля жанров аниме от общего количества") +
  theme_void() +
  theme(legend.position = "none") +  # Убираем легенду
  geom_text(aes(label = ifelse(Type != "Другие", paste0(round(Value, 1), "%"), "")), 
            position = position_stack(vjust = 0.5)) +  # Добавляем проценты только для жанров
  facet_wrap(~ Genre, nrow = 4) +  # Размещаем диаграммы в 4 ряда
  scale_fill_manual(values = colors)  # Устанавливаем цвета

# Столбчатая диаграмма для количества аниме по жанрам
# Считаем количество аниме по каждому жанру
genre_counts <- colSums(df[, c("Avant.garde", "Gourmet", "Drama", "Comedy", "Slice.of.Life", 
                               "Adventure", "Romance", "Supernatural", "Sports", 
                               "Mystery", "Thriller", "Horror", "Science.Fiction", 
                               "Fantasy", "Action", "Ecchi", "Shounen", "Shoujo", 
                               "Seinen", "Josei", "Kids")])

# Преобразуем в датафрейм
genre_counts_df <- data.frame(Genre = names(genre_counts), Count = genre_counts)

# Строим столбчатую диаграмму
ggplot(genre_counts_df, aes(x = Genre, y = Count)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Количество аниме по жанрам",
       x = "Жанр", y = "Количество аниме") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Графики для аудитории
# Подсчитываем количество аниме для каждого значения reit
reit_count <- table(df$reit)

# Преобразуем в датафрейм
reit_data <- as.data.frame(reit_count)
colnames(reit_data) <- c("reit", "Count")

# Добавляем столбец с процентами
reit_data <- reit_data %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Строим круговую диаграмму
ggplot(reit_data, aes(x = "", y = Percentage, fill = as.factor(reit))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Процентное распределение значений reit (1-6)",
       fill = "reit") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +  # Добавляем проценты на пирог
  theme_void() +
  theme(legend.title = element_blank())

# Строим столбчатую диаграмму
ggplot(reit_data, aes(x = as.factor(reit), y = Count, fill = as.factor(reit))) +
  geom_bar(stat = "identity") +
  labs(title = "Количество аниме по значениям reit (1-6)",
       x = "reit",
       y = "Количество аниме",
       fill = "reit") +
  theme_minimal() +
  geom_text(aes(label = Count), vjust = -0.5)  # Добавляем количество над столбцами

# Подсчет частот для переменной isreal
isreal_count <- table(df$isreal)

# Преобразуем в датафрейм
isreal_data <- as.data.frame(isreal_count)
colnames(isreal_data) <- c("isreal", "Count")

# Добавляем столбец с процентами
isreal_data <- isreal_data %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Круговая диаграмма (пирог)
ggplot(isreal_data, aes(x = "", y = Percentage, fill = as.factor(isreal))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Распределение переменной isreal",
       fill = "isreal") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +  # Добавляем проценты на пирог
  theme_void()

# Ленточная диаграмма (барплот)
ggplot(isreal_data, aes(x = as.factor(isreal), y = Count, fill = as.factor(isreal))) +
  geom_bar(stat = "identity") +
  labs(title = "Количество по переменной isreal",
       x = "isreal",
       y = "Количество",
       fill = "isreal") +
  theme_minimal() +
  geom_text(aes(label = Count), vjust = -0.5)  # Добавляем количество над столбцами

# Функция для построения графика плотности распределения
plot_density <- function(data, variable) {
  ggplot(data, aes_string(x = variable)) +
    # Гистограмма
    geom_histogram(aes(y = ..density..), 
                   bins = 30, 
                   fill = "blue", 
                   color = "black", 
                   alpha = 0.7) +
    
    # Эмпирическая плотность распределения
    geom_density(color = "red", size = 1) +
    
    # Плотность нормального распределения
    stat_function(
      fun = dnorm, 
      args = list(
        mean = mean(data[[variable]], na.rm = TRUE), 
        sd = sd(data[[variable]], na.rm = TRUE)
      ), 
      color = "purple", 
      size = 1
    ) +
    
    # Настройка осей и меток
    labs(
      title = paste("Распределение", variable), 
      subtitle = "Сравнение с нормальным распределением",
      x = variable, 
      y = "Плотность вероятности") +
    
    # Стиль темы
    theme_minimal() +
    
    # Легенда
    annotate("text", x = Inf, y = Inf, 
             label = "Синий - Гистограмма\nКрасный - Эмпирическая плотность\nФиолетовый - Нормальное распределение", 
             hjust = 1, 
             vjust = 1, 
             size = 3)
}

# Построение графиков плотности для переменных score, data и epis
print(plot_density(df, "score"))
print(plot_density(df, "data"))
print(plot_density(df, "epis"))
print(plot_density(df, "leng"))

# Функция для построения Q-Q графика
plot_qq <- function(data, variable) {
  ggplot(data, aes_string(sample = variable)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = paste("Q-Q график для", variable),
         x = "Теоретические квантиль", y = "Квантиль данных") +
    theme_minimal()
}

# Построение Q-Q графиков для переменных score, data и epis
print(plot_qq(df, "score"))
print(plot_qq(df, "data"))
print(plot_qq(df, "epis"))
print(plot_qq(df, "leng"))

#Класстеризация
# Выбор столбцов по интересам
d <- df %>% select(Avant.garde:Kids, reit, epis, score, leng)

# График колена для выбора количества кластеров
fviz_nbclust(d, kmeans, method = "wss") +
  labs(subtitle = "Метод локтя")

k <- 6  # Количество кластеров
cl <- kmeans(d, centers = k)
d$kmeans_cluster <- cl$cluster

# Визуализация кластеров
fviz_cluster(cl, data = d, ellipse.type = 'convex')

# Метод Варда
Mdist2 <- dist(d)
Hc_w <- hclust(Mdist2, method = "ward.D2")
plot(Hc_w, cex = 0.6, main = "Иерархическая кластеризация (Метод Варда)")
rect.hclust(Hc_w, k = k, border = "red")  # Рисуем границы кластеров
groups9 <- cutree(Hc_w, k = k)

# Добавляем информацию о кластерах в данные
d$hclust_cluster <- groups9

# Проверка кластеров
table(d$hclust_cluster)

# Вычисление средних значений для каждого кластера
res <- aggregate(. ~ hclust_cluster, data = d, FUN = mean)
View(res)

# Вычисление средних значений для возрастных рекомендаций в каждом кластере
reit_means <- d %>%
  group_by(hclust_cluster) %>%
  summarise(mean_reit = mean(reit, na.rm = TRUE))

# Вывод результатов
print(reit_means)

# Проверка нормальности для переменных epis, score, leng в каждом кластере
normality_results <- d %>%
  group_by(hclust_cluster) %>%
  summarise(
    Epis_Shapiro_p = shapiro.test(epis)$p.value,
    Score_Shapiro_p = shapiro.test(score)$p.value,
    Leng_Shapiro_p = shapiro.test(leng)$p.value
  )

# Проверяем гипотезу
normality_results <- normality_results %>%
  mutate(
    Epis_Normal = Epis_Shapiro_p > 0.05,
    Score_Normal = Score_Shapiro_p > 0.05,
    Leng_Normal = Leng_Shapiro_p > 0.05
  )

print(normality_results)


# Сохранение данных из кластеров в новой переменной, удаляя качественные переменные
# Выбираем только количественные переменные
quantitative_columns <- c("epis", "leng")  # Укажите количественные переменные
clustered_data <- d %>% select(hclust_cluster, all_of(quantitative_columns))









# Функция для вычисления и визуализации коэффициентов Спирмена
analyze_spearman_correlation <- function(data) {
  # Вычисляем коэффициенты Спирмена и p-value
  cor_test_results <- rcorr(as.matrix(data), type = "spearman")
  cor_matrix <- cor_test_results$r
  p_matrix <- cor_test_results$P
  
  # Визуализация тепловой карты
  heatmap_plot <- ggcorrplot(
    cor_matrix, 
    hc.order = TRUE, 
    type = "lower", 
    lab = TRUE,
    title = "Тепловая карта коэффициентов корреляции Спирмена",
    show.legend = TRUE
  )
  
  return(list(cor_matrix = cor_matrix, p_matrix = p_matrix, heatmap_plot = heatmap_plot))
}

# Функция для визуализации значимости коэффициентов Спирмена
visualize_spearman_significance <- function(p_matrix) {
  significance_matrix <- ifelse(p_matrix < 0.05, "red", "green")
  
  # Преобразуем матрицу значимости в формат для ggplot
  significance_df <- as.data.frame(as.table(significance_matrix))
  colnames(significance_df) <- c("Var1", "Var2", "Significance")
  
  # Визуализация матрицы значимости
  significance_plot <- ggplot(significance_df, aes(Var1, Var2, fill = Significance)) +
    geom_tile() +
    scale_fill_manual(values = c("red" = "red", "green" = "green")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(angle = 0),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    coord_fixed()  # Установка фиксированного соотношения для ячеек
  
  return(significance_plot)
}

library(ppcor)

# Функция для вычисления и визуализации частных коэффициентов корреляции
analyze_partial_correlation <- function(data) {
  # Удаляем ненужные столбцы (если есть)
  data <- data %>% select(-hclust_cluster)  # Удалите, если у вас есть другие ненужные столбцы
  
  # Вычисляем частные коэффициенты корреляции
  pcor_results <- pcor(data)
  cor_matrix <- pcor_results$estimate
  p_matrix <- pcor_results$p.value
  
  # Визуализация тепловой карты
  heatmap_plot <- ggcorrplot(
    cor_matrix, 
    hc.order = TRUE, 
    type = "lower", 
    lab = TRUE,
    title = "Тепловая карта частных коэффициентов корреляции",
    show.legend = TRUE
  )
  
  return(list(cor_matrix = cor_matrix, p_matrix = p_matrix, heatmap_plot = heatmap_plot))
}

# Функция для визуализации значимости частных коэффициентов корреляции
visualize_partial_significance <- function(p_matrix) {
  significance_matrix <- ifelse(p_matrix < 0.05, "red", "green")
  
  # Преобразуем матрицу значимости в формат для ggplot
  significance_df <- as.data.frame(as.table(significance_matrix))
  colnames(significance_df) <- c("Var1", "Var2", "Significance")
  
  # Визуализация матрицы значимости
  significance_plot <- ggplot(significance_df, aes(Var1, Var2, fill = Significance)) +
    geom_tile() +
    scale_fill_manual(values = c("red" = "red", "green" = "green")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(angle = 0),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    coord_fixed()  # Установка фиксированного соотношения для ячеек
  
  return(significance_plot)
}

# Расчет корреляционной матрицы и значимость корреляций на 5% уровне
# Функция для расчета корреляции и значимости в каждом кластере
analyze_correlation <- function(cluster_data) {
  # Расчет корреляционной матрицы с использованием корреляции Спирмена
  cor_test_results <- rcorr(as.matrix(cluster_data), type = "spearman")
  cor_matrix <- cor_test_results$r      # Матрица коэффициентов корреляции
  p_matrix <- cor_test_results$P        # Матрица p-value
  
  # Проверка значимости
  significance_matrix <- ifelse(p_matrix < 0.05, "Значимо (<0.05)", "Незначимо (>=0.05)")
  
  # Визуализация тепловой карты
  heatmap_plot <- ggcorrplot(
    cor_matrix, 
    hc.order = TRUE, 
    type = "lower", 
    lab = TRUE,
    title = "Тепловая карта коэффициентов корреляции (Спирмен)",
    show.legend = TRUE
  )
  
  return(list(cor_matrix = cor_matrix, p_matrix = p_matrix, significance_matrix = significance_matrix, heatmap_plot = heatmap_plot))
}

# Функция для анализа корреляции и визуализации значимости
analyze_correlation_significance <- function(cluster_data) {
  # Расчет корреляционной матрицы с использованием корреляции Спирмена
  cor_test_results <- rcorr(as.matrix(cluster_data), type = "spearman")
  cor_matrix <- cor_test_results$r  # Матрица коэффициентов корреляции
  p_matrix <- cor_test_results$P    # Матрица p-value
  
  # Создаем матрицу значимости на основе p-value
  significance_matrix <- ifelse(p_matrix < 0.05, "red", "green")
  
  # Преобразуем матрицу значимости в формат для ggplot
  significance_df <- as.data.frame(as.table(significance_matrix))
  colnames(significance_df) <- c("Var1", "Var2", "Significance")
  
  # Визуализация матрицы значимости
  significance_plot <- ggplot(significance_df, aes(Var1, Var2, fill = Significance)) +
    geom_tile() +
    scale_fill_manual(values = c("red" = "red", "green" = "green")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(angle = 0),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    coord_fixed()  # Установка фиксированного соотношения для ячеек
  
  return(list(cor_matrix = cor_matrix, p_matrix = p_matrix, significance_matrix = significance_matrix, significance_plot = significance_plot))
}


# Вывод результатов для каждого кластера
# Кластер 1
cluster_data_1 <- d %>% filter(hclust_cluster == 1) %>% select(-hclust_cluster, -kmeans_cluster)
results_cluster_1 <- analyze_correlation(cluster_data_1)

cat("Кластер 1:\n")
cat("Матрица коэффициентов корреляции:\n")
print(results_cluster_1$cor_matrix)

cat("\nМатрица значимости:\n")
print(results_cluster_1$significance_matrix)

cat("\nТепловая карта:\n")
print(results_cluster_1$heatmap_plot)

results_cluster_1 <- analyze_correlation_significance(cluster_data_1)
cat("\nВизуализация матрицы значимости:\n")
print(results_cluster_1$significance_plot)


# Кластер 2
cluster_data_2 <- d %>% filter(hclust_cluster == 2) %>% select(-hclust_cluster, -kmeans_cluster)
results_cluster_2 <- analyze_correlation(cluster_data_2)

cat("\n\nКластер 2:\n")
cat("Матрица коэффициентов корреляции:\n")
print(results_cluster_2$cor_matrix)

cat("\nМатрица значимости:\n")
print(results_cluster_2$significance_matrix)

cat("\nТепловая карта:\n")
print(results_cluster_2$heatmap_plot)

results_cluster_2 <- analyze_correlation_significance(cluster_data_2)
cat("\nВизуализация матрицы значимости:\n")
print(results_cluster_2$significance_plot)


# Кластер 3
cluster_data_3 <- d %>% filter(hclust_cluster == 3) %>% select(-hclust_cluster, -kmeans_cluster)
results_cluster_3 <- analyze_correlation(cluster_data_3)

cat("\n\nКластер 3:\n")
cat("Матрица коэффициентов корреляции:\n")
print(results_cluster_3$cor_matrix)

cat("\nМатрица значимости:\n")
print(results_cluster_3$significance_matrix)

cat("\nТепловая карта:\n")
print(results_cluster_3$heatmap_plot)

results_cluster_3 <- analyze_correlation_significance(cluster_data_3)
cat("\nВизуализация матрицы значимости:\n")
print(results_cluster_3$significance_plot)


# Кластер 4
cluster_data_4 <- d %>% filter(hclust_cluster == 4) %>% select(-hclust_cluster, -kmeans_cluster)
results_cluster_4 <- analyze_correlation(cluster_data_4)

cat("\n\nКластер 4:\n")
cat("Матрица коэффициентов корреляции:\n")
print(results_cluster_4$cor_matrix)

cat("\nМатрица значимости:\n")
print(results_cluster_4$significance_matrix)

cat("\nТепловая карта:\n")
print(results_cluster_4$heatmap_plot)

results_cluster_4 <- analyze_correlation_significance(cluster_data_4)
cat("\nВизуализация матрицы значимости:\n")
print(results_cluster_4$significance_plot)


# Кластер 5
cluster_data_5 <- d %>% filter(hclust_cluster == 5) %>% select(-hclust_cluster, -kmeans_cluster)
results_cluster_5 <- analyze_correlation(cluster_data_5)

cat("\n\nКластер 5:\n")
cat("Матрица коэффициентов корреляции:\n")
print(results_cluster_5$cor_matrix)

cat("\nМатрица значимости:\n")
print(results_cluster_5$significance_matrix)

cat("\nТепловая карта:\n")
print(results_cluster_5$heatmap_plot)

results_cluster_5 <- analyze_correlation_significance(cluster_data_5)
cat("\nВизуализация матрицы значимости:\n")
print(results_cluster_5$significance_plot)


# Кластер 6
cluster_data_6 <- d %>% filter(hclust_cluster == 6) %>% select(-hclust_cluster, -kmeans_cluster)
results_cluster_6 <- analyze_correlation(cluster_data_6)

cat("\n\nКластер 6:\n")
cat("Матрица коэффициентов корреляции:\n")
print(results_cluster_6$cor_matrix)

cat("\nМатрица значимости:\n")
print(results_cluster_6$significance_matrix)

cat("\nТепловая карта:\n")
print(results_cluster_6$heatmap_plot)

results_cluster_6 <- analyze_correlation_significance(cluster_data_6)
cat("\nВизуализация матрицы значимости:\n")
print(results_cluster_6$significance_plot)


#Выбор количественных переменных для анализа
quantitative_columns <- c("epis", "leng")  # Укажите независимые переменные
model_data <- clustered_data %>% select(score, all_of(quantitative_columns))

# Вычисление корреляций
cor_matrix <- cor(model_data, use = "complete.obs")

# Извлечение корреляций с переменной score
cor_with_score <- cor_matrix["score", -1]  # Убираем score из выборки

# Вычисление множественного коэффициента корреляции
multiple_r_squared <- sum(cor_with_score^2)

# Параметры для теста Фишера
k <- length(cor_with_score)  # Количество независимых переменных
n <- nrow(model_data)         # Общее количество наблюдений

# Вычисление F-статистики
F_statistic <- (multiple_r_squared / k) / ((1 - multiple_r_squared) / (n - k - 1))

# Вычисление p-значения
p_value <- pf(F_statistic, df1 = k, df2 = n - k - 1, lower.tail = FALSE)

# Вывод результатов
cat("Коэффициенты корреляции с переменной 'score':\n")
print(cor_with_score)

cat("\nМножественный коэффициент корреляции (R^2):", multiple_r_squared, "\n")
cat("F-статистика:", F_statistic, "\n")
cat("p-значение:", p_value, "\n")

# Проверка значимости
if (p_value < 0.05) {
  cat("Множественный коэффициент корреляции статистически значим (p < 0.05).\n")
} else {
  cat("Множественный коэффициент корреляции не статистически значим (p >= 0.05).\n")
}

# Выбор количественных переменных для модели
quantitative_columns <- c("epis", "leng")  # Укажите независимые переменные
model_data <- clustered_data %>% select(score, all_of(quantitative_columns))

# Построение модели линейной регрессии
model <- lm(score ~ ., data = model_data)

# Вывод результатов модели
summary(model)

# Получение множественного коэффициента корреляции
multiple_r_squared <- summary(model)$r.squared
cat("Множественный коэффициент корреляции (R^2):", multiple_r_squared, "\n")

# Проверка значимости модели
anova_results <- anova(model)
print(anova_results)

#проверка 2 гипотезы о том что большая часть рынка аниме с оценкой выше среднего принадлежить струдии MAPPA
# 1. Вычисление средней оценки
mean_score <- mean(df$score, na.rm = TRUE)

# 2. Фильтрация популярных аниме (с оценкой выше средней)
popular_anime <- df %>% filter(score > mean_score)

# 3. Подсчет общего количества популярных аниме для каждой студии
studio_counts <- popular_anime %>%
  group_by(studio) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))  # Сортируем по убыванию

# 4. Вычисление среднего количества популярных аниме на студию
mean_popular_count <- mean(studio_counts$Count)

# 5. Фильтрация студий, у которых количество популярных аниме меньше среднего
filtered_studios <- studio_counts %>%
  filter(Count >= mean_popular_count)

# 6. Подсчет общего количества популярных аниме
total_popular_anime_count <- nrow(popular_anime)

# 7. Вычисление доли популярных аниме для оставшихся студий
filtered_studios <- filtered_studios %>%
  mutate(Percentage = (Count / total_popular_anime_count) * 100)

# 8. Вывод результатов
print(filtered_studios)

# 9. Визуализация долей популярных аниме для оставшихся студий
library(ggplot2)

ggplot(filtered_studios, aes(x = reorder(studio, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Доля популярных аниме по студиям", x = "Студия", y = "Доля (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Проверка 3 гипотезы о линейных зависимостях
# Корреляционный анализ
correlation_test <- cor.test(df$data, df$score, use = "complete.obs")

# Вывод результатов
cat("Корреляция между годом производства и рейтингом:", correlation_test$estimate, "\n")
cat("p-значение:", correlation_test$p.value, "\n")

# ANOVA для проверки зависимости между рейтингом и студией
anova_studio_score <- aov(score ~ studio, data = df)

# Вывод результатов
anova_summary <- summary(anova_studio_score)
print(anova_summary)

# Извлечение p-значения
p_value_studio <- anova_summary[[1]][["Pr(>F)"]][1]
cat("p-значение для зависимости между рейтингом и студией:", p_value_studio, "\n")

# Корреляционный анализ между возрастным ограничением и рейтингом
correlation_age_score <- cor.test(df$reit, df$score, use = "complete.obs")

# Вывод результатов
cat("Корреляция между возрастным ограничением и рейтингом:", correlation_age_score$estimate, "\n")
cat("p-значение:", correlation_age_score$p.value, "\n")

