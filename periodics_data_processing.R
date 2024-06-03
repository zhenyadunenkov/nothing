library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(stringi)
library(tidyr)

names <- read.csv("lis_names - lis_names.csv")
topics <- read.csv("lis_topics - lis_topics.csv")
main <- read.csv("lisovsky (апдейтед от 19.03.2024) - lisovsky.csv")

test1 <- topics$name[30]
test2 <- topics$name[52]


# extract_years <- function(str) {
  
  pattern <- "\\d\\d\\d\\d—\\d\\d\\d\\d"

  res <- str_extract_all(str, pattern = pattern)
  
  return(res)
}

extract_years <- function(str) {
  res <- str_replace_all(str, pattern = " \\(\\*\\) — ", replacement = "—")
  res <- str_replace_all(res, pattern = "[^\\d— ]", replacement = "")
  res <- str_replace_all(res, pattern = " +", replacement = " ")
  res <- str_replace_all(res, pattern = "^ +", replacement = "")
  res <- str_replace_all(res, pattern = " — ", replacement = " ")
  res <- str_replace_all(res, pattern = " +$", replacement = "")
  
  return(res)
}

topics <- topics %>% 
  mutate(name = str_replace_all(name, " — прод.", "—9999")) %>% 
  mutate(years = map(name, extract_years))


probs <- topics %>% 
  select(name, id, years) %>% 
  filter(nchar(years) == 4 | nchar(years) == 9 & str_sub(years, 5, 5) == "—")

colnames(probs) <- c("name", "lisovsky_num", "years") 

probs <- mutate(probs, lisovsky_num = as.integer(lisovsky_num)) %>% 
  na.omit()

get_end_year <- function(year) {
  if(is.null(year)) {
    return("")
  }
  if(nchar(year) == 4) {
    return(year)
  }
  if(nchar(year) == 9) {
    return(str_sub(year, 6, 9))
  }
}

so_far <- left_join(probs, main, by="lisovsky_num") %>% 
  select(id, year, title, lisovsky_num, years) %>% 
  mutate(end_year = map_chr(years, get_end_year)) %>% 
  select(id, lisovsky_num, title, year, end_year)

nines <- so_far %>% 
  filter(end_year == 9999)

minus <- so_far %>% 
  filter(nchar(end_year) != 0)

unsorted <- topics %>% 
  select(name, id, years) %>% 
  mutate(id = as.integer(id))

colnames(unsorted) <- c("name", 'lisovsky_num', 'years')


difficulties <- anti_join(unsorted, so_far, by='lisovsky_num')


difficulties <- difficulties %>% 
  mutate(name = as.character(name)) %>% 
  mutate(lisovsky_num = as.integer(lisovsky_num)) %>% 
  mutate(years = as.character(years)) %>% 
  distinct(lisovsky_num, years, .keep_all = T)
  
add_info <- main %>% 
  select(id, lisovsky_num, title, clean_data) %>% 
  na.omit()



difficulties <- left_join(difficulties, add_info, by='lisovsky_num', copy=F)

difficulties <- difficulties %>% 
  mutate(name = as.character(name)) %>% 
  mutate(lisovsky_num = as.character(lisovsky_num)) %>% 
  mutate(years = as.character(years)) %>% 
  mutate(clean_data = as.character(clean_data))

write.csv(so_far, "so_far.csv")
write.csv(difficulties, "difficulties.csv")



cities <- topics %>% 
  select(name, id, city)

ready_cities <- c("Санкт-Петербург", 
                  "Москва", 
                  "Асхабадъ",
                  "Варшава",
                  "Вильно",
                  "Казань",
                  "Калуга",
                  "Кіевъ",
                  "Новочеркасскъ",
                  "Одесса",
                  "Орелъ",
                  "Оренбургъ",
                  "Островъ",
                  "Рига",
                  "Ташкентъ",
                  "Тифлисъ",
                  "Тобольскъ",
                  "Харьковъ",
                  "Ярославль",
                  "Ѳеодосія",
                  "Архангельскъ",
                  "Астрахань",
                  "Баку",
                  "Батумъ",
                  "Благовѣщенскъ",
                  "Брянскъ",
                  "Владивостокъ",
                  "Владикавказъ",
                  "Владикавказъ",
                  "Воронежъ",
                  "Вятка",
                  "Гапсаль",
                  "Гельсингфорсъ",
                  "Двинскъ",
                  "Дерптъ",
                  "Екатеринбургъ",
                  "Екатеринодаръ",
                  "Екатеринославъ",
                  "Елисаветградъ",
                  "Житоміръ",
                  "Иркутскъ",
                  "Каменецъ-Подольскъ",
                  "Кишиневъ",
                  "Кострома",
                  "Красноярскъ",
                  "Кронштадтъ",
                  "Курскъ",
                  "Кяхта",
                  "Либава",
                  "Лодзь",
                  "Минскъ",
                  "Митава",
                  "Нарва",
                  "Николаевскъ",
                  "Николаевъ",
                  "Новгородъ",
                  "Новороссійскъ",
                  "Омскъ",
                  "Псковъ",
                  "Ревель",
                  "Ростовъ на Дону",
                  "Рыбинскъ",
                  "Самара",
                  "Саратовъ",
                  "Севастополь",
                  "Симбирскъ",
                  "Симферополь",
                  "Смоленскъ",
                  "Ставрополь-Кавказскій",
                  "Таганрогъ",
                  "Тверь",
                  "Томскъ",
                  "Тюмень",
                  "Уральскъ",
                  "Ханькоу",
                  "Херсонъ",
                  "Царицынъ",
                  "Черниговъ",
                  "Чита",
                  "Ялта",
                  "Оргѣевъ",
                  "Пинскъ",
                  "Владиміръ",
                  "Бѣлостокъ",
                  "Витебскъ",
                  "Вологда",
                  "Вѣрный",
                  "Гродно",
                  "Калишъ",
                  "Карсъ",
                  "Керчь",
                  "Ковно",
                  "Кременецъ",
                  "Кутаисъ",
                  "Кѣльцы",
                  "Липецкъ",
                  "Ломжа",
                  "Люблинъ",
                  "Могилевъ",
                  "Оренбургь",
                  "Пенза",
                  "Пермь",
                  "Петрозаводскъ",
                  "Петроковъ",
                  "Плоцкъ",
                  "Подольскъ",
                  "Полтава",
                  "Пятигорскъ",
                  "Радомъ",
                  "Рязань",
                  "Семипалатинскъ",
                  "Ставрополь",
                  "Сувалки",
                  "Сѣдлецъ",
                  "Тамбовъ",
                  "Тула",
                  "Уфа",
                  "Хабаровскъ",
                  "Якутскъ",
                  "Дорогобужъ",
                  "Суджъ",
                  "Баускъ",
                  "Вейсенштейнъ",
                  "Вольмаръ",
                  "Елабуга",
                  "Ирбитъ",
                  "Козловъ",
                  "Маріуполь",
                  "Мстиславль",
                  "Никольскъ-Уссурійскій",
                  "Перновъ",
                  "Петровскъ",
                  "Ржевъ",
                  "Самаркандъ",
                  "Сарапуль",
                  "Старая Русса",
                  "Сызрань",
                  "Царицинъ",
                  "Юрьевъ",
                  "Вольскъ",
                  "Почаевская Лавра",
                  "Барнаулъ",
                  "Винница",
                  "Курганъ",
                  "Новый Маргеланъ",
                  "Славянскъ",
                  "Темниковъ",
                  "Ораніенбаумъ")

difficult_cities <- cities %>% 
  filter(!(city %in% ready_cities))

simple_cities <- cities %>% 
  filter(city %in% ready_cities)

cities_to_process <- unique(difficult_cities['city']) %>% 
  mutate(correct = "")


# тут произошла внешняя доработка диффикултситиесов

difficult_cities <- read.csv("difficult_cities.csv")

difficult_cities <- difficult_cities %>% 
  select(name, id, city, start, end)

simple_cities <- simple_cities %>% 
  mutate(start = '') %>% 
  mutate(end='')

cities <- rbind(difficult_cities, simple_cities) %>% 
  select(id, city, start, end)

colnames(cities) <- c('lisovsky_num', 'city', 'year', 'end_year')


write.csv(cities, "cities.csv")





# ФИНАЛЬНАЯ ЧАСТЬ РАЗБОРКИ С ГОРОДАМИ

cities <- read.csv("periodics/cities.csv")

cities_old_names <- cities['city'] %>% 
  distinct() %>% 
  arrange()

write.csv(cities_names, "cities_old_names.csv")
cities_new_names <- read.csv("cities_names.csv") %>% 
  select(-X)


cities_names <- cities_new_names %>% 
  mutate(old_names = cities_old_names[['city']])

colnames(cities_names) <- c('new_name', 'old_name')

colnames(cities) <- c('lisovsky_num', 'old_name', 'year', 'end_year')

# теперь есть таблица связей с городами, где есть новые и старые названия
cities <- cities %>% left_join(cities_names)

# заполним NA в таблице городов

# Подгружаю то, что сделали ребята, некоторый автофилл за Ваней 
years <- read.csv("periodics/years.csv") %>% 
  mutate(id = ifelse(id == '',
                     NA,
                     id))

years <- years %>% tidyr::fill(id)


years_aux <- years %>% 
  select(lisovsky_num, year, end_year) %>% 
  na.omit() %>% 
  mutate(lisovsky_num = as.character(lisovsky_num))

colnames(years_aux) <- c('lisovsky_num', 'start', 'finish')

cities <- cities %>% 
  left_join(years_aux)

# собственно, заполняем

cities <- cities %>% 
  mutate(year = ifelse(is.na(year),
                       start,
                       year)) %>% 
  mutate(end_year = ifelse(is.na(end_year),
                           finish,
                           end_year))

cities_fin <- cities %>% 
  select(lisovsky_num, new_name, old_name, year, end_year) %>% 
  distinct()

write.csv(cities_fin, "cities.csv")

# Доразберёмся с годами

whole <- main %>% 
  select(id, lisovsky_num, year)

colnames(years) <- c('id', 'title', 'lisovsky_num', 'start', 'end_year')

years <- years %>% 
  mutate(id = as.integer(id))


whole <- whole %>% 
  left_join(years) %>% 
  select(-title)

whole <- whole %>% 
  na.omit()

write.csv(whole, "whole.csv")



problems <- whole %>% 
  filter(whole['year'] != whole['start'] + 1)

to_join <- main %>% 
  select(id, title, lisovsky_num, clean_data)

problems <- problems %>% 
  filter(problems['year'] != problems['start']) 


problem_years <- whole %>% 
  filter(is.na(start)) %>% 
  left_join(to_join)
  
write.csv(problem_years, "missing_years.csv")


# Доделываю годы после того, как ребята вычитали

missing_years <- read.csv("periodics/missing_years.csv") %>% 
  mutate(id = ifelse(id == '',
                     NA,
                     id))

missing_years <- missing_years %>% tidyr::fill(id)
  

missing_years <- missing_years %>% 
  select(id, title, lisovsky_num, start, end_year)

years_final <- rbind(years, missing_years)
write.csv(years_final, "years.csv")


# финальное сведение

main <- read.csv("periodics/main.csv") 

write.csv(main, "main.csv")



# темы

topics <- read.csv("periodics/topics.csv") 

topic_tiers <- topics %>% 
  select(topic, subtopic) %>% 
  unique()

  
unique(topics$topic)
unique(topics$subtopic)
