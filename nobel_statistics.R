# Нам понадобятся следующие библиотеки:
library(tidyr)
library(dplyr)
library(purrr)
library(jsonlite)
library(ggplot2)
library(lubridate)
library(gridExtra)


# Скачиваем и распаковываем данные
url <- "https://github.com/zhenyadunenkov/nothing/raw/main/laureates.json"
# оригинальный файл можно получить через API сайта нобелевского комитета
# по следующему адресу: 
# https://masterdataapi.nobelprize.org/2.0/laureates?offset=0&limit=10000
destfile <- paste(getwd(),"/laureates.json", sep="")
download.file(url, destfile)

# Распаковываем JSON
laureates <- fromJSON("laureates.json")$laureates %>%
  unnest(cols=c(nobelPrizes), names_repair="minimal") %>% 
  select(!starts_with("links"))
affiliations <- select(laureates, c(id, awardYear, affiliations)) %>% 
  unnest(cols=c(affiliations), names_repair="minimal") %>% 
  jsonlite::flatten()
laureates <- laureates %>% 
  jsonlite::flatten()
laureates <- laureates %>% left_join(affiliations)

# Забираем интересующие нас данные
laureates <- laureates %>% 
  select(gender, awardYear, dateAwarded, knownName.en, birth.date, 
         birth.place.city.en, birth.place.country.en, 
         birth.place.cityNow.en, birth.place.countryNow.en, 
         birth.place.continent.en, category.en, motivation.en, name.en, 
         nameNow.en, city.en, country.en, cityNow.en, countryNow.en, 
         death.date, givenName.en, familyName.en) %>%
  # Переводим даты в удобный формат, дополняем недостающие позиции
  # стандартными значениями
  mutate(dateAwarded = ifelse(is.na(dateAwarded), 
                              paste(awardYear, "-11-12", sep=""),
                              dateAwarded)) %>% 
  mutate(dateAwarded = ymd(dateAwarded)) %>%
  mutate(birth.date = ymd(birth.date)) %>% 
  mutate(death.date = ymd(death.date)) %>% 
  # Добавляем столбцы с данными, основанными на исходных данных
  mutate(lifetime = interval(birth.date, death.date) / years(1)) %>% 
  mutate(age_of_award = floor(interval(birth.date, dateAwarded) / years(1))) %>% 
  mutate(remainder = interval(dateAwarded, death.date) / years(1)) %>% 
  mutate(month_of_birth = month(birth.date, label = TRUE, abbr = FALSE)) %>% 
  mutate(current_age = ifelse(is.na(death.date),
                              interval(birth.date, ymd("2023-01-01")) / years(1),
                              NA)) %>% 
  unique()


# "ПОЛЬЗА НОБЕЛЕВСКОЙ ПРЕМИИ ДЛЯ ЗДОРОВЬЯ"

# Считаем среднюю продолжительность жизни
longevity <- select(laureates, lifetime, knownName.en) %>% 
  na.omit() %>% 
  unique() # просим уважаемых неоднократных лауреатов не влиять статистику

  summary(longevity)
# Min.   : 39.22        
# 1st Qu.: 74.47     
# Median : 82.44    
# Mean   : 80.82                     
# 3rd Qu.: 88.59                     
# Max.   :103.69  

# Строим график 
ggplot(longevity, aes(x = lifetime)) +
  geom_density() +
  scale_x_continuous("Продолжительность жизни") +
  scale_y_continuous(element_blank())


# Считаем среднюю продолжительность жизни по областям
select(laureates, lifetime, category.en) %>% 
  na.omit() %>% 
  unique() %>% 
  group_by(category.en) %>% 
  summarize(mean=mean(lifetime)) %>% 
  arrange(desc(mean))
# 1 Economic Sciences       86.2
# 2 Physiology or Medicine  81.7
# 3 Physics                 81.4
# 4 Chemistry               79.6
# 5 Literature              79.5
# 6 Peace                   78.3

# Список почивших лауреатов, проживших более 100 лет
select(laureates, c(knownName.en, lifetime)) %>% 
  filter(lifetime > 99.9)
# 1    Edmond H. Fischer 101.3918
# 2   John B. Goodenough 100.9178
# 3 Rita Levi-Montalcini 103.6904
# 4      Ronald H. Coase 102.6767

# Список ныне живых лауреатов, старше 100 лет
select(laureates, c(knownName.en, current_age)) %>% 
  filter(current_age > 99.9)
# 1 Chen Ning Yang    100.2767

# Считаем среднюю продолжительность жизни по пятилетиям
avg_lifetime_by_years <- select(laureates, c(awardYear, knownName.en, lifetime)) %>%
  filter(awardYear < 1972) %>%
  na.omit() %>% 
  unique() %>% 
  group_by(awardYear) %>% 
  summarize(mean=mean(lifetime)) %>% 
  group_by(grp = rep(row_number(), length.out = n(), each = 5)) %>% 
  summarize(avg_lifetime=mean(mean)) %>% 
  mutate(period=c("1901-05", "1906-10","1911-15","1917-21","1922-26",
                  "1927-31","1932-36","1937-44","1945-49","1950-54",
                  "1955-59","1960-64","1965-69", "1970-71"))

# Строим график по средней продолжительности жизни
ggplot(data=avg_lifetime_by_years, aes(period, avg_lifetime)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("Ср. продолжительность жизни") +
  xlab(element_blank())

# Средний возраст лауреатов и график
award_age <- select(laureates, c(knownName.en, age_of_award))

ggplot(award_age, aes(x = age_of_award)) +
  geom_density() +
  scale_x_continuous("Возраст лауреата на момент награждения") +
  scale_y_continuous(element_blank())


# Средний возраст лауреатов по годам премии
award_age_by_years <- select(laureates, c(awardYear, age_of_award)) %>%
  na.omit() %>% 
  group_by(awardYear) %>% 
  summarize(mean = mean(age_of_award))

# Строим график по среднему возрасту награждения
ggplot(data=award_age_by_years, aes(awardYear, mean)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("Ср. возраст на момент награждения") +
  xlab(element_blank()) +
  scale_x_discrete(breaks = seq(1900, 2020, by = 5))

# Средний возраст награждения
award_age <- select(laureates, c(knownName.en, awardYear, age_of_award)) %>% 
  na.omit()
summary(award_age$age_of_award)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 17.00   51.00   60.00   60.18   69.00   97.00 

# Средний возраст награждения по областям
award_age <- select(laureates, c(knownName.en, category.en, awardYear, age_of_award)) %>% 
  na.omit() %>% 
  group_by(category.en) %>% 
  summarize(mean=mean(age_of_award)) %>% 
  arrange(desc(mean))
award_age
# 1 Economic Sciences       66.4
# 2 Literature              64.8
# 3 Peace                   61.1
# 4 Chemistry               58.8
# 5 Physiology or Medicine  58.7
# 6 Physics                 58.0

# Нобелевские лауреаты по месяцам рождения и по областям
monthes_categories <- laureates %>%
  select(knownName.en, month_of_birth, category.en) %>% 
  unique() %>% 
  group_by(month_of_birth, category.en) %>% 
  count() %>% 
  na.omit() %>% 
  mutate(days_num = ifelse(month_of_birth %in% c("January", "March", "May", "July", "August", "October", "December"),
                           31,
                           ifelse(month_of_birth %in% c("April", "June", "September", "November"),
                                  30,
                                  28.25))) %>% 
  mutate(month_coef = round(n / days_num, 3))

# График
ggplot(monthes_categories, aes(y=month_coef, x=month_of_birth, fill=category.en)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  scale_fill_discrete(name = "Категории")


# "НЕМНОЖКО GENDER STUDIES"
# Соотношение мужчин и женщин среди лауреатов
laureates %>% select(knownName.en, gender) %>% 
  na.omit() %>% 
  # unique() %>%
  group_by(gender) %>% 
  count()
# 1 female    68
# 2 male     980

female_aw <- laureates %>% select(knownName.en, gender, awardYear) %>% 
  filter(gender=="female") %>%
  group_by(gender, awardYear) %>% 
  count()

female_awards <- tibble(awardYear = 1911:2023) %>% 
  mutate(awardYear = as.character(awardYear)) %>% 
  left_join(female_aw) %>% 
  select(c(awardYear, n)) %>% 
  group_by(grp = rep(row_number(), length.out = n(), each = 10)) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  summarize(sum_awards = sum(n)) %>% 
  mutate(period=c("1911-20", "1921-30","1931-40","1941-50","1951-60",
                  "1961-70","1971-80","1981-90","1991-00","2001-10",
                  "2011-20","2021-23"))

# График награждений женщин по десятилетиям
ggplot(female_awards, aes(period, sum_awards)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("Число награждений") +
  xlab(element_blank())

# Распредление женщин-лауреатов по областям 
fem_awards <- laureates %>% select(knownName.en, gender, category.en) %>% 
  filter(gender=="female") %>% 
  group_by(category.en) %>% 
  count() %>% 
  arrange(desc(n))

fem_awards
# 1 Peace                     19
# 2 Literature                17
# 3 Physiology or Medicine    14
# 4 Chemistry                  9
# 5 Physics                    5
# 6 Economic Sciences          4


# "УНИВЕРСИТЕТЫ"

# Распределение лауреатов по странам университетов
top_countries <- select(laureates, c(awardYear, category.en, countryNow.en, knownName.en)) %>%
  unique() %>% 
  group_by(countryNow.en) %>% 
  count() %>% 
  na.omit() %>% 
  arrange(desc(n))

# график 
ggplot(top_countries, aes(reorder(countryNow.en, n), n, countryNow.en)) +
  geom_col() +
  coord_flip() +
  ylab("Число лауреатов") +
  xlab(element_blank()) +
  geom_text(aes(label = n), size = 2.5, nudge_y = 10, color="black")

# Распределение стран университетов по областям
countries_by_fields <- select(laureates, c(awardYear, category.en, countryNow.en, knownName.en)) %>%
  unique() %>% 
  group_by(countryNow.en, category.en) %>% 
  count() %>% 
  na.omit() %>% 
  arrange(desc(n)) %>% 
  filter(countryNow.en %in% c("USA", "United Kingdom", "Germany", "France",
                              "Switzerland", "Japan", "Sweden", "Russia", 
                              "Denmark", "the Netherlands"))
# график
ggplot(countries_by_fields, aes(reorder(countryNow.en, n), n, countryNow.en, fill=category.en)) +
  geom_col(position="dodge") +
  ylab("Число лауреатов") +
  xlab(element_blank()) +
  scale_fill_discrete(name = "Категории")

# Распределение лауреатов по университетам
top_uni <- select(laureates, c(name.en, countryNow.en, knownName.en)) %>%
  na.omit() %>%
  group_by(name.en, countryNow.en) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter(n>5)

# график
ggplot(top_uni, aes(reorder(name.en, n), n, name.en, fill=countryNow.en)) +
  geom_col() +
  coord_flip() +
  ylab("Число лауреатов") +
  xlab(element_blank()) +
  scale_fill_discrete(name = "Страны") +
  geom_text(aes(label = n), size = 3, nudge_y = -2, color="white")


# Распределение лауреатов по странам рождения
birth_countries <- select(laureates, c(knownName.en, 
                                       awardYear, 
                                       birth.place.country.en,
                                       birth.place.countryNow.en,
                                       category.en)) %>% 
  na.omit() %>% 
  unique() %>% 
  # Восстанавливаем историческую справедливость? 
  mutate(birth.place.country.en = ifelse(birth.place.country.en == "Russian Empire" |
                                           birth.place.country.en == "USSR",
                                         "Russia",
                                         birth.place.country.en)) %>% 
  mutate(birth.place.country.en = ifelse(birth.place.country.en == "Prussia" |
                                           birth.place.country.en == "West Germany",
                                         "Germany",
                                         birth.place.country.en)) %>% 
  # Неоднозначное присвоение всех Австро-Венгерских лауреатов Австрии
  mutate(birth.place.country.en = ifelse(birth.place.country.en == "Austria-Hungary",
                                         "Austria",
                                         birth.place.country.en)) %>% 
  # Возвращаем Северную Ирландию в родную гавань
  mutate(birth.place.country.en = ifelse(birth.place.country.en == "Northern Ireland",
                                         "United Kingdom",
                                         birth.place.country.en)) %>% 
  
  group_by(birth.place.country.en) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter(n > 2)

colnames(top_countries) = c("birth.place.country.en", "n_old")
combined <- left_join(birth_countries, top_countries)

# график
ggplot(combined, aes(reorder(birth.place.country.en, n), n, birth.place.country.en)) +
  geom_col() +
  geom_col(aes(birth.place.country.en, n_old), color="pink", alpha=0) +
  coord_flip() +
  ylab("Число лауреатов") +
  xlab(element_blank()) +
  geom_text(aes(label = n), size = 2.5, nudge_y = 10, color="black")

# Лауреаты по городам рождения
birth_cities <- select(laureates, c(knownName.en, 
                                       awardYear, 
                                       birth.place.cityNow.en,
                                        birth.place.country.en,
                                       category.en)) %>% 
  na.omit() %>% 
  mutate(birth.place.country.en = ifelse(birth.place.country.en == "Russian Empire" |
                                           birth.place.country.en == "USSR",
                                         "Russia",
                                         birth.place.country.en)) %>% 
  mutate(birth.place.country.en = ifelse(birth.place.country.en == "Prussia" |
                                           birth.place.country.en == "West Germany",
                                         "Germany",
                                         birth.place.country.en)) %>% 
  mutate(birth.place.country.en = ifelse(birth.place.country.en == "Austria-Hungary",
                                         "Austria",
                                         birth.place.country.en)) %>% 
  mutate(birth.place.country.en = ifelse(birth.place.country.en == "Northern Ireland",
                                         "United Kingdom",
                                         birth.place.country.en)) %>%
  # Возвращаем Бруклин в родную гавань...
  mutate(birth.place.cityNow.en = ifelse(birth.place.cityNow.en == "Brooklyn, NY",
                                         "New York, NY",
                                         birth.place.cityNow.en)) %>% 
  group_by(birth.place.cityNow.en, birth.place.country.en) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter(n>4)

# график
ggplot(birth_cities, aes(reorder(birth.place.cityNow.en, n), n, birth.place.cityNow.en,
                         fill=birth.place.country.en)) +
  geom_col() +
  coord_flip() +
  ylab("Число лауреатов") +
  xlab(element_blank()) +
  geom_text(aes(label = n), size = 3, nudge_y = 2, color="black") +
  scale_fill_discrete(name = "Страны")
  
# Соотносим лауреатов с населением городов
cities <- c("New York, NY","Paris","London","Vienna","Chicago, IL","Berlin",
            "Boston, MA","Stockholm","Washington, D.C.","Budapest","Moscow",
            "Munich","Hamburg","Frankfurt-on-the-Main","Milwaukee, WI",
            "Prague","St. Petersburg")

stats <-  c(8335897, # Нью-Йорк
            2102650,# Париж
            8799800,# Лондон
            2002821,# Вена
            2665039,# Чикаго
            3870000,# Берлин
            675647,# Бостон
            984748,# Стокгольм
            689545,# Вашингтон
            1774000,# Будапешт
            12635000,# Москва
            1512491,# Мюнхен
            1945532, # Гамбург
            773068,# Франкфурт
            577222,# Милуоки
            1357326,# Прага
            5377503) # Петербург

# ИСТОЧНИКИ:
# New-York #https://www.nyc.gov/assets/planning/download/pdf/planning-level/nyc-population/population-estimates/population-trends-2022.pdf?r=a
# Paris https://www.insee.fr/fr/statistiques/1893198
# London https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationandhouseholdestimatesenglandandwales/census2021
# Vienna https://www.wien.gv.at/english/administration/statistics/
# Chicago https://www.census.gov/quickfacts/fact/table/chicagocityillinois,US/PST045222
# Berlin https://www.berlin.de/en/news/8425887-5559700-statistics-berlins-population-continues-.en.html
# Boston, MA https://www.census.gov/quickfacts/fact/table/bostoncitymassachusetts/PST045222
# Stockholm https://www.citypopulation.de/en/sweden/cities/mun/
# Washington, D.C. https://www.census.gov/quickfacts/fact/table/washingtoncitydistrictofcolumbia/PST045222
# Budapest https://budapest.hu/sites/english/Lapok/General-informations-about-Budapest.aspx
# Moscow https://77.rosstat.gov.ru/folder/70759/document/189531
# Munich https://www.statistikdaten.bayern.de/genesis/online?operation=result&code=12411-003r&leerzeilen=false&language=de
# Hamburg https://fragdenstaat.de/anfrage/bevolkerung-in-hamburg-am-31-12-2022-nach-stadtteilen/
# Frankfurt-on-the-Main https://statistik.hessen.de/sites/statistik.hessen.de/files/2023-06/ai2_j2022_bevoelkerung_tabelle_2.xlsx
# Milwaukee, WI https://www.census.gov/quickfacts/fact/table/milwaukeecitywisconsin/POP010220
# Prague https://www.czso.cz/csu/czso/population-of-municipalities-1-january-2023
# St. Petersburg https://78.rosstat.gov.ru/storage/mediabank/%D0%A7%D0%B8%D1%81%D0%BB.%D0%A1%D0%9F%D0%B1%20%D0%BD%D0%B0%2001.01.2022%20.pdf

st <- as_tibble(list(a= cities, b = stats))
colnames(st) = c("birth.place.cityNow.en", "population")

birth_cities <- left_join(birth_cities, st) %>% 
  mutate(nobel_coef = round((n/(population/1000000)), 1)) %>% 
  arrange(desc(n))

# график
ggplot(birth_cities, aes(reorder(birth.place.cityNow.en, nobel_coef), nobel_coef, birth.place.cityNow.en,
                         fill=birth.place.country.en)) +
  geom_col() +
  coord_flip() +
  ylab("Лауреатов на каждый миллион жителей города") +
  xlab(element_blank()) +
  geom_text(aes(label = nobel_coef), size = 3, nudge_y = 1, color="black") +
  scale_fill_discrete(name = "Страны")



# "ГДЕ РОДИЛСЯ, ТАМ И ПРИГОДИЛСЯ?"

# Смотрим на миграцию
migration <- laureates %>% 
  select(knownName.en, birth.place.countryNow.en, birth.place.country.en, countryNow.en) %>% 
  na.omit() %>% 
  mutate(birth.place.countryNow.en = ifelse(birth.place.country.en == "Russian Empire",
                                           "Russia",
                                           birth.place.countryNow.en)) %>% 
  mutate(birth.place.countryNow.en = ifelse(birth.place.country.en == "USSR",
                                            "Russia",
                                            birth.place.countryNow.en)) %>% 
  filter(birth.place.countryNow.en != countryNow.en) %>% 
  select(knownName.en, birth.place.countryNow.en, countryNow.en)

colnames(migration) = c("name", "from", "to")

top_from <- migration %>% 
  group_by(from) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter(n>2)

top_to <- migration %>% 
  group_by(to) %>% 
  count() %>% 
  arrange(desc(n)) 

top_from_to <- migration %>% 
  group_by(from, to) %>% 
  count() %>% 
  arrange(desc(n))

# графики
g1 <- ggplot(top_from, aes(reorder(from, n), n, from)) +
  geom_col() +
  coord_flip() +
  ylab("Утечка мозгов") +
  xlab(element_blank()) +
  geom_text(aes(label = n), size = 3, nudge_y = 1, color="black")

g2 <- ggplot(top_to, aes(reorder(to, n), n, to)) +
  geom_col() +
  coord_flip() +
  ylab("Приток мозгов") +
  xlab(element_blank()) +
  geom_text(aes(label = n), size = 3, nudge_y = 3, color="black")

grid.arrange(g1, g2, ncol=2)
