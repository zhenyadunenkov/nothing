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
         death.date, givenName.en, familyName.en, orgName.en) %>%
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

annotations_longevity <- data.frame(
  x = c(round(min(longevity$lifetime), 2), 
        round(mean(longevity$lifetime), 2), 
        round(max(longevity$lifetime), 2)),
  y = c(5, 35, 5),
  label = c("Мин.", "Средн.", "Макс.")
) 


# Строим график 
ggplot(longevity, aes(x=lifetime)) +
  geom_histogram(color = "#000000", fill = "#0099F8", binwidth=1) +
  geom_text(data = annotations_longevity, aes(x = x, y = y, label = paste(label, x)), 
            size = 3.5) +
  labs(
    x = "Продолжительность жизни",
    y = "Число лауреатов"
  ) +
  scale_x_continuous(breaks = seq(0, 105, 5)) +
  scale_y_continuous(breaks = seq(0, 35, 5)) +
  theme_test()



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
  filter(awardYear < 1971) %>%
  na.omit() %>% 
  unique() %>% 
  group_by(awardYear) %>% 
  summarize(mean=mean(lifetime)) %>% 
  group_by(grp = floor((as.numeric(awardYear)-1)/5)-190) %>% 
  summarize(avg_lifetime=mean(mean)) %>% 
  mutate(period=c("1901-05", "1906-10","1911-15","1916-20","1921-25",
                  "1926-30","1931-35", "1936-40","1941-45","1946-50",
                  "1951-55","1956-60","1961-65","1966-70")) %>%
  mutate(avg_lifetime=round(avg_lifetime, 1)) %>% 
  # добавляем примерный медианный возраст США, чтобы
  # посмотреть, есть ли корелляция между изменением 
  # среднего возраста лауреатов и средним медианным возрастом
  mutate(US_avg_lifetime = c(48.7, # 1905
                             50.0, # 1910
                             54.5, # 1915
                             54.1, # 1920
                             59.0, # 1925
                             59.7, # 1930
                             61.7, # 1935
                             62.9, # 1940
                             65.9, # 1945
                             68.2, # 1950
                             69.6, # 1955
                             69.7, # 1960
                             70.2, # 1965
                             70.9)) # 1970
# Источник данных по США: https://fraser.stlouisfed.org/title/historical-statistics-united-states-237/volume-1-5809
# Схема B107--115 



# Строим график по средней продолжительности жизни
ggplot(data=avg_lifetime_by_years, aes(period, avg_lifetime, group=1)) +
  geom_point(color = "#000000", fill = "#0099F8") +
  ylab("Ср. продолжительность жизни") +
  xlab(element_blank()) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  geom_smooth(aes(x=period, y=US_avg_lifetime, color="Ср. пр. жизни в США"), method="lm") +
  geom_smooth(aes(x=period, y=avg_lifetime, color="Ср. пр. жизни лауреатов"), method="lm") +
  theme_test() +
  theme(legend.position = 'right',
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text(aes(, , 
                label = avg_lifetime), color="black",
            hjust = 1.3, size=3) +
  labs(colour="Линии")


# Средний возраст лауреатов и график
award_age <- select(laureates, c(knownName.en, age_of_award, category.en)) %>% 
  na.omit() 


ggplot(award_age, aes(x=age_of_award, fill=category.en)) +
  geom_histogram(color = "#000000", binwidth=3) + 
  scale_x_continuous(breaks = seq(0, 105, 5)) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  theme_test() +
  labs(
    x = "Возраст награждения",
    y = "Число лауреатов") +
  scale_fill_discrete(name = "Категории (ср. возраст)", limits=c("Economic Sciences", 
                                                   "Literature", 
                                                   "Peace", 
                                                   "Chemistry", 
                                                   "Physiology or Medicine", 
                                                   "Physics"),
                      labels = c("Экономика (66.4)", 
                                 "Литература (64.8)", 
                                 "Премия мира (61.1)",
                                 "Химия (58.8)",
                                 "Медицина (58.7)",
                                 "Физика (58.0)"))

# Смотрим на каждую область отдельно
literature <- select(laureates, c(knownName.en, age_of_award, category.en)) %>%
  na.omit() %>%
  filter(category.en=="Literature")

economics <- select(laureates, c(knownName.en, age_of_award, category.en)) %>%
  na.omit() %>%
  filter(category.en=="Economic Sciences")

physics <- select(laureates, c(knownName.en, age_of_award, category.en)) %>% 
  na.omit() %>% 
  filter(category.en=="Physics")

chemistry <- select(laureates, c(knownName.en, age_of_award, category.en)) %>% 
  na.omit() %>% 
  filter(category.en=="Chemistry")

medicine <- select(laureates, c(knownName.en, age_of_award, category.en)) %>% 
  na.omit() %>% 
  filter(category.en=="Physiology or Medicine")

peace <- select(laureates, c(knownName.en, age_of_award, category.en)) %>% 
  na.omit() %>% 
  filter(category.en=="Peace")

lit <- ggplot(literature, aes(x=age_of_award)) +
  geom_density() +
  theme_test() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(
    x = "Литература",
    y = "") +
  coord_cartesian(xlim=c(30,100))
  
    
eco <- ggplot(economics, aes(x=age_of_award)) +
  geom_density() +
  theme_test() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(
    x = "Экономика",
    y = "")  +
  coord_cartesian(xlim=c(30,100))
phys <- ggplot(physics, aes(x=age_of_award)) +
  geom_density() +
  theme_test() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(
    x = "Физика",
    y = "")  +
  coord_cartesian(xlim=c(30,100))
chem <- ggplot(chemistry, aes(x=age_of_award)) +
  geom_density() +
  theme_test() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(
    x = "Химия",
    y = "")  +
  coord_cartesian(xlim=c(30,100))
med <- ggplot(medicine, aes(x=age_of_award)) +
  geom_density() +
  theme_test() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(
    x = "Медицина",
    y = "")  +
  coord_cartesian(xlim=c(30,100))
pea <- ggplot(peace, aes(x=age_of_award)) +
  geom_density() +
  theme_test() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(
    x = "Премия мира",
    y = "")  +
  coord_cartesian(xlim=c(30,100))

grid.arrange(med, eco, phys, chem, pea, lit)



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



# "НЕМНОГО GENDER STUDIES"
# Соотношение мужчин и женщин среди лауреатов
laureates %>% select(knownName.en, gender) %>% 
  na.omit() %>% 
  unique() %>%
  group_by(gender) %>% 
  count()
# 1 female    64
# 2 male     901

female_aw <- laureates %>% select(knownName.en, gender, awardYear, category.en) %>%
  na.omit() %>% 
  filter(gender=="female") %>%
  unique() %>% 
  mutate(awardYear = as.numeric(awardYear))

ggplot(female_aw, aes(x=awardYear, fill=category.en)) +
  geom_histogram(color = "#000000", center=1905, binwidth=10) + 
  scale_x_continuous(breaks = seq(1900, 2026, 10)) + 
  scale_y_continuous(breaks = seq(0, 20, 1)) +
  theme_test() +
  labs(
    x = "Возраст награждения",
    y = "Число лауреатов") +
  scale_fill_discrete(name = "Категории (число лауреатов)", limits=c("Peace", 
                                                                 "Literature", 
                                                                 "Physiology or Medicine", 
                                                                 "Chemistry", 
                                                                 "Physics", 
                                                                 "Economic Sciences"),
                      labels = c("Премия мира (19)", 
                                 "Литература (17)", 
                                 "Медицина (14)",
                                 "Химия (9)",
                                 "Физика (5)",
                                 "Экономика (4)")) +
theme(axis.text.x = element_text(angle = 90))
  
# "УНИВЕРСИТЕТЫ"

# Распределение лауреатов по странам университетов
top_countries <- select(laureates, c(awardYear, category.en, countryNow.en, knownName.en)) %>%
  unique() %>% 
  na.omit() %>% 
  group_by(countryNow.en, category.en) %>% 
  count(countryNow.en, category.en) 

top_countries_total <- select(laureates, c(awardYear, category.en, countryNow.en, knownName.en)) %>%
  unique() %>% 
  na.omit() %>% 
  group_by(countryNow.en) %>% 
  count(countryNow.en)
colnames(top_countries_total)[2] = "n_total"

top_countries <- left_join(top_countries, top_countries_total, copy = FALSE)



ggplot(top_countries, aes(reorder(countryNow.en, n_total), n, countryNow.en, fill=category.en)) +
  geom_col() +
  coord_flip() +
  scale_fill_discrete(name = "Категории", limits=c("Physiology or Medicine",
                                                   "Physics", 
                                                   "Peace",
                                                   "Economic Sciences",
                                                   "Chemistry", 
                                                   "Literature"),
                      labels = c("Медицина", 
                                 "Физика", 
                                 "Премия мира",
                                 "Экономика",
                                 "Химия",
                                 "Литература")) +
  geom_text(aes(y = 1.1, , group = countryNow.en, 
                label = n_total), color="gray",
            hjust = 1.3, size=3) +
  theme_test() +
  labs(
    x = "",
    y = "Число лауреатов")
  

# Распределение лауреатов по университетам
top_uni <- select(laureates, c(name.en, countryNow.en, knownName.en, cityNow.en)) %>%
  na.omit() %>%
  group_by(name.en, cityNow.en, countryNow.en) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter(n>5)

# график
ggplot(top_uni, aes(reorder(name.en, n), n, name.en, fill=countryNow.en)) +
  geom_col() +
  coord_flip() +
  ylab("Число лауреатов") +
  xlab(element_blank()) +
  scale_fill_discrete(name = "Страны",
                      labels = c("Франция","Германия","Россия",
                                 "Великобритания", "США")) +
  geom_text(aes(label = n), size = 3, nudge_y = -3, color="white") +
  
    theme_test()


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


top_countries <- select(laureates, c(awardYear, countryNow.en, knownName.en)) %>%
  unique() %>% 
  na.omit() %>% 
  group_by(countryNow.en) %>% 
  count(countryNow.en) 

colnames(top_countries) = c("birth.place.country.en", "n_old")
combined <- left_join(birth_countries, top_countries)

# график
ggplot(combined, aes(reorder(birth.place.country.en, n), n, birth.place.country.en)) +
  geom_col(fill = "#0099F8") +
  geom_col(aes(birth.place.country.en, n_old), color="pink", alpha=0) +
  coord_flip() +
  ylab("Число лауреатов") +
  xlab(element_blank()) +
  geom_text(aes(label = n), size = 2.5, nudge_y = 10, color="black") +
  theme_test()  

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
  scale_fill_discrete(name = "Страны",
                      labels = c("Австрия","Франция","Германия","Венгрия",
                                 "Россия", "Швеция","Великобритания", "США")) +
  geom_text(aes(label = n), size = 3, nudge_y = -3, color="white") +
  
  theme_test()


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
  ylab("Лауреатов на млн жителей") +
  xlab(element_blank()) +
  scale_fill_discrete(name = "Страны",
                      labels = c("Австрия","Франция","Германия","Венгрия",
                                 "Россия", "Швеция","Великобритания", "США")) +
  geom_text(aes(label = nobel_coef), size = 3, nudge_y = 1, color="black") +
  theme_test()

    ylab("Лауреатов на каждый миллион жителей города") +
  xlab(element_blank()) +
  geom_text(aes(label = nobel_coef), size = 3, nudge_y = 1, color="black") +
  scale_fill_discrete(name = "Страны")



# "ГДЕ РОДИЛСЯ, ТАМ И ПРИГОДИЛСЯ?"

# Смотрим на миграцию
migration <- laureates %>% 
  select(knownName.en, birth.place.countryNow.en, 
         birth.place.country.en, 
         countryNow.en, 
         category.en,
         awardYear) %>% 
  na.omit() %>% 
  mutate(birth.place.countryNow.en = ifelse(birth.place.country.en == "Russian Empire",
                                            "Russia",
                                            birth.place.countryNow.en)) %>% 
  mutate(birth.place.countryNow.en = ifelse(birth.place.country.en == "USSR",
                                            "Russia",
                                            birth.place.countryNow.en)) %>% 
  filter(birth.place.countryNow.en != countryNow.en) %>% 
  select(knownName.en, birth.place.countryNow.en, countryNow.en, category.en, awardYear) %>% 
  filter(knownName.en != "Marie Curie") %>% 
  filter(knownName.en != "Friz Lipman") %>%
  filter(knownName.en != "Otto Wallach") %>%
  filter(knownName.en != "Ragnar Granit") %>%
  filter(knownName.en != "Artturi Virtanen") %>%
  filter(knownName.en != "Wilhelm Wien") %>% 
  filter(knownName.en != "Ilya Prigogine") %>% 
  add_row(knownName.en="Ilya Prigogine", 
          birth.place.countryNow.en="Russia", 
          countryNow.en="USA", 
          category.en="Chemistry", 
          awardYear="1977") %>% 
  unique()


colnames(migration) = c("name", "from", "to", "category", "year")

russians <- migration %>% 
  filter(from=="Russia")

ggplot(russians, aes(x=to, fill=category)) +
  geom_histogram(stat="count") +
  coord_flip() +
  xlab(element_blank()) +
  ylab(element_blank()) +
  scale_fill_discrete(name="Категории", labels=c("Химия",
                                                 "Экономика",
                                                 "Физика",
                                                 "Медицина")) +
  scale_y_continuous(breaks = seq(0, 20, 1)) +
  theme_test()

ggplot(female_aw, aes(x=awardYear, fill=category.en)) +
  geom_histogram(color = "#000000", center=1905, binwidth=10) + 
  scale_x_continuous(breaks = seq(1900, 2026, 10)) + 
  scale_y_continuous(breaks = seq(0, 20, 1)) +
  theme_test() +
  





  

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
  geom_col(color = "#000000", fill = "#0099F8") +
  coord_flip() +
  ylab("Утечка мозгов") +
  xlab(element_blank()) +
  geom_text(aes(label = n), size = 3, nudge_y = 1, color="black") +
  theme_test()

g2 <- ggplot(top_to, aes(reorder(to, n), n, to)) +
  geom_col(color = "#000000", fill = "#0099F8") +
  coord_flip() +
  ylab("Приток мозгов") +
  xlab(element_blank()) +
  geom_text(aes(label = n), size = 3, nudge_y = 3, color="black")+
  theme_test()

grid.arrange(g1, g2, ncol=2)



