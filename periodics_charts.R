library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(stringi)
library(tidyr)
library(ggplot2)

main <- read.csv("periodics/main.csv")
cities <- read.csv("periodics/cities.csv")
years <- read.csv("periodics/years.csv")
topics <- read.csv("periodics/topics.csv")

# ______________________________________________________________________
# ВОПРОС 12
# Сколько в среднем живут периодические издания? 
# Увеличивается или уменьшается продолжительность их жизни со временем?
# ______________________________________________________________________

# заменяем 9999 на 1900 для того, чтобы считать возраст на 1900
years <- years %>% 
  mutate(end_year = ifelse(end_year == 9999,
                           1900,
                           end_year))
# убираем дочерние издания (сомнительное решение, но пока других не вижу)
# также убрал end_year > 1900, похоже на ошибки
years <- years %>% 
  filter(!is.na(lisovsky_num)) %>% 
  filter(end_year < 1901)

titles <- main %>% 
  select(lisovsky_num, new_first_title)
# считаем длины промежутков

start_years <- main %>% 
  select(lisovsky_num, year)

years <- years %>% 
  mutate(duration = end_year - start + 1)

years_acc <- years %>% 
  group_by(lisovsky_num) %>% 
  summarize(total_duration = sum(duration)) %>% 
  arrange(-total_duration) %>% 
  left_join(titles) %>% 
  left_join(start_years)

longevity_17 <- years_acc %>% 
  filter(year < 1800) 
# %>% 
#   filter(total_duration < 100)

longevity_1800_1885 <- years_acc %>% 
  filter(year > 1799) %>% 
  filter(year < 1886)

summary(longevity_1800_1885$total_duration)


# Средняя продолжительность жизни издания 18-го века
summary(longevity_17$total_duration)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   1.000   6.368   3.000 173.000 




# рассмотрим отдельно долгожителей и рядовые издания
old <- years_acc %>% 
  filter(total_duration >= 50)

young <- years_acc %>% 
  filter(total_duration < 50)

summary(longevity_1800_1885['total_duration'])
# Min.   : 1.00  
# 1st Qu.: 2.00  
# Median : 4.00  
# Mean   :12.06  
# 3rd Qu.:17.00  
# Max.   :92.00  


ggplot(longevity_1800_1885, aes(x=total_duration)) +
  geom_histogram(binwidth=1, color="darkblue", fill="lightblue") +
  theme_minimal()+
  labs(title="Продолжительность жизни периодических изданий",
       subtitle = "(для изданий с началом выхода в 1800–1885 годах)",
       x="Число лет", y = "Число изданий") +
  scale_x_continuous(breaks=seq(0,100,2)) +
  stat_bin(binwidth=1, geom="text", aes(label=..count..), 
           vjust=-1.5, size = 3, color='darkgray') 





# Продолжительность жизни по годам выхода
start_years <- years %>% 
  select(lisovsky_num, start)

# # средняя longevity по годам
# 
# 
# longevity_by_start_year <- longevity_1800_1885 %>% 
#   group_by(year) %>% 
#   summarize(mean(total_duration)) %>% 
#   left_join(z)




longevity_by_start_year <- years_acc %>% 
  left_join(start_years) %>% 
  group_by(start) %>% 
  summarize(mean(total_duration)) %>% 
  arrange(start) 

colnames(longevity_by_start_year) <- c('start', 'mean_dur')

z <- longevity_1800_1885 %>%
  group_by(year) %>%
  count(year)

colnames(z) <- c('start', 'n')


# отфильтруем 1800-1885
longevity_by_start_year <-  longevity_by_start_year %>% 
  filter(start>1799) %>% 
  filter(start<1886) %>% 
  left_join(z)

ggplot(longevity_by_start_year, aes(x=start, y=mean_dur)) +
  geom_col(color="darkblue", fill="lightblue") +
  scale_x_continuous(breaks=seq(1800,1885,5)) +
  scale_y_continuous(breaks=seq(0,65,5)) +
  stat_smooth(aes(fill='black',x = start, y = n), method = "lm",
              formula = y ~ poly(x, 21), se = FALSE, size=0.5, colour='red', 
              show.legend = TRUE) +
  scale_fill_manual(name='',values=c("black", "blue"), 
                    labels=c('Число изданий по годам')) +
  theme_minimal() +
  labs(title="Продолжительность жизни в зависимости от года начала издания",
       subtitle = "(издания 1800–1885 годов)",
       x="Год начала издания", y = "Продолжительность жизни")

# ______________________________________________________________________
# ВОПРОС 13
# Сколько изданий могло выходить одновременно? 
# ______________________________________________________________________

amount_by_years <- tibble(year = 1703:1900) %>% 
  mutate(current_mags = 0)


for (row in 1:(nrow(parents))) {

  start_year <- years$start[row] - 1702
  end_year <- years$end_year[row] - 1702

  for (i in start_year : end_year) {
    amount_by_years$current_mags[i] <- amount_by_years$current_mags[i] + 1
  } 
}

am_by_years_before_1800 <- amount_by_years %>% 
  filter(year < 1800)

am_by_years_after_1800 <- amount_by_years %>% 
  filter(year > 1799)


ggplot(am_by_years_before_1800, aes(x=year, y=current_mags)) +
  geom_col(color="darkblue", fill="lightblue") +
  theme_minimal() +
  labs(title="Число текущих изданий по годам",
       subtitle='(1703—1799)',
       x="Год", y = "Число текущих изданий") +
  scale_x_continuous(breaks=seq(1703,1800,5)) +
  scale_y_continuous(breaks=seq(0,20,1))




ggplot(am_by_years_after_1800, aes(x=year, y=current_mags)) +
  geom_col(color="darkblue", fill="lightblue") +
  theme_minimal() +
  labs(title="Число текущих изданий по годам",
       subtitle='(1800—1900)',
       x="Год", y = "Число текущих изданий") +
  scale_x_continuous(breaks=seq(1800,1900,5)) +
  scale_y_continuous(breaks=seq(0,1000,50))






new_mags_by_years <- start_years %>% 
  group_by(start) %>% 
  count()




genres <- topics %>% 
  group_by(subtopic) %>% 
  count() %>% 
  arrange(-n)

write.csv(genres, "genres.csv")

new_genres <- read.csv("genres.csv") %>% 
  select(subtopic, X.1)

topics_widen <- topics %>% 
  left_join(new_genres)

colnames(topics_widen) <- c('lisovsky_num',
                            'level',
                            'topic',
                            'subtopic',
                            'topics_generalized')




new_by_topic <- start_years %>% 
  filter(start > 1799)

topics_to_join <- topics_widen %>% 
  select(lisovsky_num, topic, topics_generalized) %>% 
  distinct(lisovsky_num, .keep_all=T)



start_years <- years %>% 
  select(lisovsky_num, start)

start_years <- start_years %>% 
  left_join(topics_to_join) %>% 
  filter(start > 1799) %>% 
  na.omit()

z <- start_years %>% 
  mutate(n=1) %>% 
  count(topic, sort=T)

ggplot(start_years, aes(x = start, fill=topic)) + 
  geom_histogram(binwidth = 1, color="darkblue") +
  theme_minimal() +
  labs(title="Направление новых изданий",
       x="Год", y = "Число новых изданий") +
  scale_x_continuous(breaks=seq(1800,1900,5)) +
  scale_y_continuous(breaks=seq(0,200,5)) +
  scale_fill_discrete(name = "Направление изданий", 
                      labels=c('Официальные', 
                               'Политические и литературные',
                               'Гуманитарные',
                               'Естественно-научные',
                               'Научно-практические'))

top_directions_by_years <- start_years %>%
  filter(start > 1849) %>% 
  group_by(start, topic) %>% 
  count() %>% 
  arrange(start, -n)  %>% 
  mutate(decade = as.numeric(start) - as.numeric(start) %% 10)

top_directions_by_decades <- top_directions_by_years %>%
  group_by(topic, decade) %>% 
  summarize(sum(n)) %>% 
  filter(decade<1900)

colnames(top_directions_by_decades) <- c('topic', 'decade', 'n')  


ggplot(top_directions_by_decades, aes(x=decade, y=n)) + 
  geom_col(aes(decade, n, 
               fill=topic), position=position_dodge(width=10)) + 
  # geom_text(aes(label=n), 
            # position=position_dodge(width=10), vjust=0) +
  scale_x_continuous(breaks=seq(1850,1900,10)) +
  scale_y_continuous(breaks=seq(0,300,20)) +
  labs(title="Направления новых изданий по десятилетиям",
       x="Десятилетие", y = "Число новых изданий соответствующего направления") +
  scale_fill_discrete(name = "Направление изданий", 
                      labels=c('Официальные', 
                               'Политические и литературные',
                               'Гуманитарные',
                               'Естественно-научные',
                               'Научно-практические')) 






ggplot(start_years %>% filter(start > 1799), aes(x = start)) + 
  geom_histogram(binwidth = 1, color="darkblue", fill="lightblue") +
  theme_minimal() +
  labs(title="Число новых изданий по годам",
       x="Год", y = "Число новых изданий") +
  scale_x_continuous(breaks=seq(1800,1900,5)) +
  scale_y_continuous(breaks=seq(0,200,5)) +
  stat_bin(binwidth=1, geom="text", aes(label=..count..), 
           vjust=-1.5, size = 2, color='black') 





start_years <- years %>% 
  select(lisovsky_num, start)

levels <- topics %>% 
  select(lisovsky_num, level)

levels_to_join <- levels %>% 
  distinct(lisovsky_num, .keep_all=T)

start_years <- start_years %>% 
  left_join(levels_to_join) %>% 
  filter(start > 1799) %>% 
  na.omit()


levels_by_years <- start_years %>%
  group_by(start, level) %>% 
  count() %>% 
  arrange(start, -n)  %>% 
  mutate(decade = as.numeric(start) - as.numeric(start) %% 10)




ggplot(start_years, aes(x = start, fill=level)) + 
  geom_histogram(binwidth = 1, color="darkblue") +
  theme_minimal() +
  labs(title="Провинциальные и столичные издания",
       x="Год", y = "Число новых изданий") +
  scale_x_continuous(breaks=seq(1800,1900,5)) +
  scale_y_continuous(breaks=seq(0,200,5)) +
  scale_fill_discrete(name = "Уровень изданий", 
                      labels=c('Москва', 
                               'Санкт-Петербург',
                               'Провинциальные')) 
