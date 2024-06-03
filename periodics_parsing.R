library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tidyverse)
library(tidyr)

# -------------------------------------------------
# БЭКЛОГ
# v проверить, что в основной всё есть (все проблемы с номерными решены, все безномерные точно есть)
# v проверить границы годов (мог налажать немного)
# - почистить таблицу с рубриками
# - присоединить рубрики и персоналии к основной таблице (через лисовски айди)
# - составить задачи для вычитки основной таблицы
# - надо как-то (видимо, вручную) проверить, что в тиббле все
#   записи из Лисовского, которые без номера
# - что делать с записями на несколько изданий
# - что делать с записями на несколько городов, на несколько наименований
  
# - понять, что делать с записями без лисовски_айди (присваивать бис от основного?)
#   наверное, делать ссылку на айди основного
# -------------------------------------------------

# ЧТО БЫ ХОТЕЛОСЬ ЕЩЁ
# - добавить год прекращения выхода издания (+ сложные кейсы, типа нескольких
#   возобновлений, а также собрать по тем, которые были "прод." на момент выхода
#   указателя, когда они всё-таки закончили выходить)

# -------------------------------------------------
# ПРОБЛЕМСЫ, КОТОРЫЕ НАДО РУКАМИ ЧИСТИТЬ:
# 624 Труды Высоч. утвержд. Вольнаго Общ. Любителей Россійск. Словесности
# 1033 Юридические записки

# -------------------------------------------------


# ИТАК, МЫ ПОНЯЛИ, ЧТО ССЫЛКИ ДОЛЖНЫ БЫТЬ ВОТ ТАКОГО ВИДА:

# -------------------------------------------------
# ЧТО МЫ ВЫТАСКИВАЕМ ИЗ ЭТОЙ СТРАНИЦЫ
# (пишу функцию для обработки)


process_page <- function(page, page_id) {

# название на новом русском
name_modern <- html_nodes(page, xpath="//meta[@name='title']") %>% 
  html_attr(name="content")

# весь текстовый блок -- узлом, чтобы потом можно было доковыряться и вообще проверить
block <- html_element(page, ".text")

# block_list <- paste(capture.output(block, file=NULL), collapse="")
block_list <- as.character(block)

# название на новом русском из другого места для проверки
headers <- list(html_elements(block, "h4")) # набор хедеров, на некоторых страницах их несколько, надо думать, как обрабатывать


# содержание всех txt элементов
txt_elements <- list(html_elements(block, ".txt"))
txt0_elements <- list(html_elements(block, ".txt0"))
txt1_elements <- list(html_elements(block, ".txt1"))
 
txt2_elements <- html_elements(block, ".txt2") # такого элемента не встречал, если будет, то ворнинг нужен
if (length(txt2_elements) > 0) {
  print("ПОПАЛСЯ txt2_element в")
  print(name_new)
  print(link)
  print("")
}

txt3_elements <- html_elements(block, ".txt3") # такого элемента не встречал, если будет, то ворнинг нужен
if (length(txt3_elements) > 0) {
  print("ПОПАЛСЯ txt3_element в")
  print(name_new)
  print(link)
  print("")
}


txt8_elements <- list(html_elements(block, ".txt8"))
txt8_1_elements <- list(html_elements(block, ".txt8-1"))

txt8_2_elements <- html_elements(block, ".txt8-2") # такого элемента не встречал, если будет, то ворнинг нужен
if (length(txt8_2_elements) > 0) {
  print("ПОПАЛСЯ txt8_2_element в")
  print(name_new)
  print(link)
  print("")
}

txt8ot_elements <- list(html_elements(block, ".txt8ot"))
txt8ot_0_elements <- list(html_elements(block, ".txt8ot-0"))
txt8ot_1_elements <- list(html_elements(block, ".txt8ot-1"))

txt8ot_2_elements <- html_elements(block, ".txt8ot-2") # такого элемента не встречал, если будет, то ворнинг нужен
if (length(txt8ot_2_elements) > 0) {
  print("ПОПАЛСЯ txt8ot_2_element в")
  print(name_new)
  print(link)
  print("")
}
  
accum_tbl <<- accum_tbl %>% 
  add_row(id = page_id,
    title = name_modern,
    whole_txt_block = block_list,
    all_headers = headers,
    txt_elements = txt_elements,
    txt0_elements = txt0_elements,
    txt1_elements = txt1_elements,
    txt8_elements = txt8_elements,
    txt8ot_elements = txt8ot_elements,
    txt8ot_0_elements = txt8ot_0_elements,
    txt8ot_1_elements = txt8ot_1_elements)


# отдельно вытаскивание жирных (это основное оригинальное заглавие)

# отдельно обработка сценариев, когда у нас несколько изданий на 
# странице (продолжения, переименования, варианты) -- когда у нас несколько
# раз вытаскивалось что-то с жирным начертанием

}


process_link <- function(link, id) {
  tryCatch(
    expr = {
      page <- read_html(link)
      process_page(page, id)
    },
    error = function(e){
      return}
  )
}


# 7044

accum_tbl <- tibble(
  id = NA,
  title = NA,
  whole_txt_block = NA,
  all_headers = NA,
  txt_elements = NA,
  txt0_elements = NA,
  txt1_elements = NA,
  txt8_elements = NA,
  txt8ot_elements = NA,
  txt8ot_0_elements = NA,
  txt8ot_1_elements = NA)

# 7046

start_time = Sys.time()
for (i in 11:7046) {
  if (i < 6090) {
    zeros <- strrep("0", 4 - str_length(i))
    link <- paste0("https://feb-web.ru/feb/periodic/lb-abc/lb1/lb1-", 
                   zeros,
                   i,
                   ".htm?cmd=2&amp;istext=1")
  }
  else {
    link <- paste0("https://feb-web.ru/feb/periodic/lb-abc/lb2/lb2-", 
                   i,
                   ".htm?cmd=2&amp;istext=1")
  }
  process_link(link, i)
  
  if (i %% 10 == 0) {
    print(i)
  }

  # Sys.sleep(0.1)
}
end_time = Sys.time()
print(end_time - start_time)

accum_tbl <- accum_tbl %>% 
  na.omit()

html2txt <- function(html) {
  txt <- html %>% 
    read_html() %>% 
    html_text2() %>% 
    str_replace_all("[\r\r]", " ")
  return(txt)
}

# присоединяем столбец с html содержимым в виде строки
test_tbl <- accum_tbl %>% 
  mutate(clean_txt = map_chr(whole_txt_block, html2txt)) 

# присоединяем столбец if multiple items
multiple_items <- function(string) {
  if (str_detect(string, "а[)] ")) {
    return(T)
  }
  else {return(F)}
}

test_tbl <- test_tbl %>% 
  mutate(multiple_items = map(clean_txt, multiple_items))
    
# присоединяем столбец if multiple titles (несколько <b>)
multiple_titles <- function(string) {
  return(str_count(string, "<b>[^п]") - 1)
}

test_tbl <- test_tbl %>% 
  mutate(multiple_titles = map(whole_txt_block, multiple_titles))

# вытаскиваем первые наименования, упомянутые на странице
get_first_title <- function(string) {
  title <- read_html(string) %>% 
    html_element("b") %>% 
    html_text2() %>% 
    str_remove("[:punct:]$")
  return(title)
}

test_tbl <- test_tbl %>% 
  mutate(first_title = map_chr(whole_txt_block, get_first_title))

# добавляем даты (векторы в конце)
id_tbl <- tibble(id = ids, year = years)

all_tbl <- tibble(id = 1:7044) %>% 
  left_join(id_tbl) %>% 
  fill(year, .direction = "down")

all_tbl$year[11] <- 1703

test_tbl <- test_tbl %>% 
  left_join(all_tbl)


# достаём номера по Лисовскому и помечаем безнумерные
get_first_par <- function(string) {
  pars <- read_html(string) %>% 
    html_elements("p")
  first_par <- pars[2] %>% 
    html_text2()
  return(first_par)
}
  
get_lisovsky_num <- function(string) {
  first_par <- get_first_par(string)
  if (!(grepl("^\\d", first_par))) {
    return("")
  }
  else {
    num <- str_extract(first_par, "^\\d+")
    return(num)
    }
}

test_tbl <- test_tbl %>% 
  mutate(lisovsky_num = map_chr(whole_txt_block, get_lisovsky_num))

# достаём полные названия

get_full_title <- function(string) {
  first_par <- get_first_par(string)
  
  if (!(grepl("^\\d", first_par))) {
    full_title <- str_extract(first_par, "^.+?\\. ") %>% 
      str_replace("\\. ", "") %>% 
      str_replace("^а\\) ", "") %>% 
      str_replace(", (изд|издан|издав)$", "") 
    if (is.na(full_title)) {
      full_title <- first_par %>% 
        str_replace("^а\\) ", "") %>% 
        str_replace(", (изд|издан|издав)$", "")
    }
  }
  else {
    full_title <- str_extract(first_par, "\\d+\\. .+?((\\. )|$)") %>% 
      str_replace("^\\d+\\. ", "") %>% 
      str_replace("\\. ", "") %>% 
      str_replace("^а\\) ", "") %>% 
      str_replace(", (изд|издан|издав)$", "")
  }
  return(full_title)
}

test_tbl <- test_tbl %>% 
  mutate(full_title = map_chr(whole_txt_block, get_full_title))


# вытаскиваем все персоналии на странице
get_names <- function(string) {
  html_vec <- read_html(string) %>% 
    html_elements("i") 
  
  names <- html_vec %>% 
    map_chr(html_text2) %>% 
    str_c(collapse = ", ")
  
  return(names)
}

test_tbl <- test_tbl %>% 
  mutate(names = map_chr(whole_txt_block, get_names))

# достаём формат

get_format <- function(string) {
  process <- function(str) {
    res <- str %>% 
      str_replace_all(" ", "") %>% 
      str_replace_all("\\(", "") %>% 
      str_replace_all("\\.", "") %>% 
      str_replace_all("º", "") 
    return(res)
  }
  
  format <- str_extract_all(string, " [^ \\n]+º")[[1]]
  if (length(format) > 0) {
    format <- format %>% 
      map_chr(process) %>% 
      unique() %>% 
      str_c(collapse = ", ")
  return(format)
  }
  else {
    return(NA)
  }
}

test_tbl <- zapasnoy

test_tbl <- test_tbl %>% 
  mutate(format = map_chr(clean_txt, get_format))



# достаём регулярность

get_frequency <- function(string) {
  freq_pattern <- paste("[^\\.]*[еЕ]же.[^ ]*о [^\\.]*",
                        "[^\\.]*[еЕ]же.[^ ]*о,[^\\.]*",
                        "[^\\.]*[еЕ]же.[^ ]*о\\.",
                        "Неопредѣленно[^\\.]*",
                        "[^\\.]*о частям[^\\.]*",
                        "[^\\.]*неопредѣл[^\\.]*",
                        "[^\\.]*въ (нед|мес|мѣс|год)[^\\.]*",
                        "Чрезъ[^\\.]*",
                        sep = "|")
  
  freq <- string %>% 
    str_extract(freq_pattern) %>% 
    str_replace("^ ", "") %>% 
    str_replace("\\.$", "")
  
  return(freq)
}

test_tbl <- test_tbl %>% 
  mutate(frequency = map_chr(clean_txt, get_frequency))


# смотрим элементы
get_txt1 <- function(string) {
  txt1_list <- read_html(string) %>% 
    html_elements(".txt1")
  
  
  %>% 
    html_text2() %>% 
    str_remove("[:punct:]$")

    return(title)
}

txt1_elements <- list(html_elements(block, ".txt1"))


# удаляем <span class="page> и после этого пустые <p> в whole_txt_block
# после этого делаем новую версию clean_txt

remove_pages <- function(string) {
  node <- read_html(string) 
  pages <- html_elements(node, ".page")
  xml_remove(pages)
  node <- html_children(html_children(html_children(node))) 
  
  node <- as.character(node)
  
  node <- str_c(node, collapse="") %>% 
    str_replace_all("<p>\\\n</p>", "") 
  
  return(node)
}

test_tbl <- test_tbl %>% 
  mutate(html_data = map_chr(whole_txt_block, remove_pages)) 


test_tbl <- test_tbl %>% 
  mutate(clean_data = map_chr(html_data, html2txt)) 


checker <- test_tbl %>% 
  select(title)

test_block <- test_tbl$whole_txt_block[1]

z <- remove_pages(test_block)





# Таблица личных имён

names_html <- read_html("https://feb-web.ru/feb/periodic/chronics/lb2/lb22a092.htm?cmd=2") %>% 
  html_element(".text")

names <- html_children(names_html)

# идём по списку, берём элемент h5, достаём из него title, это пойдёт в первый столбец
# берём следующий элемент (проверяем, чтобы он был uk8 или uk81, если нет, то print)
# это пойдёт во второй столбец
# пристёгиваем новую строку к тибблу

names_tbl <- tibble(modern_name = NA,
                    old_name = NA)

last_item_was_h5 <- F
h5_content <- ""

for (i in 1:length(names)) {
  current_element <- names[i]
  if (last_item_was_h5) {
    last_item_was_h5 <- F
    if (html_attrs(current_element) %in% c("uk8", "uk81")) {
      p_content <- html_text2(current_element)
      
      names_tbl <- names_tbl %>% 
        add_row(modern_name = h5_content, old_name = p_content)
    }
    else{print(current_element)}
  }
  else {
    if (html_name(current_element) == "h5") {
      last_item_was_h5 <- T
      h5_content <- html_attr(current_element, "title")
    }
  }
}

t_names_tbl <- names_tbl %>% 
  separate(col=old_name, into=c("old_name_sep", "id"), sep = "(?<=\\D\\.) ?(?=[0-9])") 

t_names_tbl <- t_names_tbl %>% 
  separate(col=old_name_sep, into=c("old_name_sep", "id2"), sep = "(?<=\\D) (?=[0-9])") 

t_names_tbl <- t_names_tbl %>% 
  unite("id", id:id2, na.rm = T)
  
t_names_tbl <- t_names_tbl %>% 
  separate(id, sep = ",", into = c())


test <- t_names_tbl %>%
  mutate(id = strsplit(id, ",")) %>%
  unnest(id) %>%
  group_by(old_name_sep) %>%
  mutate(row = row_number()) %>%
  spread(row, id)

test <- test %>% 
  pivot_longer(cols = 3:21,
               values_to = "id") %>% 
  na.omit() %>% 
  mutate(id=str_squish(id))

t_names_tbl <- test %>% 
  select(-name)


# тематический указатель

topics_html <- read_html("https://feb-web.ru/feb/periodic/chronics/lb2/lb228972.htm?cmd=2") %>% 
  html_element(".text")

topics <- html_children(topics_html)

topics_tbl <- tibble(name = NA,
                     id = NA,
                     level = NA,
                     topic = NA,
                     subtopic = NA,
                     city = NA) %>% 
  na.omit()

# идём по очереди, в h4 l=1 берём из title уровень (спб/мск/провинция)
# в h4 l=2 из title берём рубрику
# в h4 l=3 из title берём подрубрику
# каждый table обрабатываем, учитывая текущие уровень, рубрику, подрубрику:
# html_table превращаем в tibble, na.omit(),
# mutate level, topic, subtopic = соответственно,
# к общему тибблу всё полученное rbind(общий тиббл, текущий)

level <- ""
topic <- ""
subtopic <- "1. Журналы"
current_tibble <- ""



for (i in 1:length(topics)) {
  current_element <- topics[i]
  
  if (html_name(current_element) == "h4") {
    if (html_attr(current_element, "l") == "1") {
      level <- html_attr(current_element, "title")
    }
    if (html_attr(current_element, "l") == "2") {
      topic <- html_attr(current_element, "title")
    }
    if (html_attr(current_element, "l") == "3") {
      subtopic <- html_attr(current_element, "title")
    }
  }
  
  if (html_name(current_element) == "table") {
    current_tibble <- html_table(current_element)[[1]] %>% 
      na.omit()
    if (ncol(current_tibble) == 3) {
      current_tibble <- current_tibble %>% 
        select(-2)
    }
    colnames(current_tibble) <- c("name", "id")
    current_tibble <- current_tibble %>% 
      mutate(level = level, topic = topic, subtopic = subtopic, city = NA)
    topics_tbl <- rbind(topics_tbl, current_tibble)
    previous_tibble <- current_tibble

  }
  
} 

# делим название по точке с пробелом, 
# спб и москву -- выделяем годы издания и не паримся
# провинциальные -- отделяем город, отделяем годы,
# не забываем, что может быть в скобках ещё инфа
# Окраина. (1890—1893. Самаркандъ). 1894—1898. Ташкентъ

topics_test_tbl <- topics_tbl %>% 
  mutate(city = ifelse(level=="Издания петербургские",
                       "Санкт-Петербург",
                       ifelse(level=="Издания московские",
                              "Москва",
                              NA)))
    
topics_spb <- topics_test_tbl %>% 
  filter(city=="Санкт-Петербург")

topics_mos <- topics_test_tbl %>% 
  filter(city=="Москва")

topics_prov <- topics_test_tbl %>% 
  filter(is.na(city))

topics_prov <- topics_prov %>% 
  # mutate(city = str_extract(name, "(?<=[(од)\\d]\\. )[^\\.]*(\\.)?$")) %>% 
  mutate(city = str_extract(name, "(?<=((од)|\\d)\\. ).*$"))

  # separate(col=name, into=c("name", "city_"), sep = ) 

topics_test <- rbind(topics_spb, topics_mos, topics_prov)



# выявилось, что не все лисовские нумера вытащены аккуратно, исправляем вручную

test_tbl$lisovsky_num[409] <- "383"
test_tbl$full_title[409] <- "Архангельскія Губ. Вѣд"

test_tbl$lisovsky_num[480] <- "460"
test_tbl$full_title[480] <- "Ковенскія Губ. Вѣд"

test_tbl$lisovsky_num[515] <- "493"
test_tbl$full_title[515] <- "Донскія Войсковыя Вѣдомости"

test_tbl$lisovsky_num[520] <- "497"
test_tbl$full_title[520] <- "Ставропольскія Губернскія Вѣдомости"

test_tbl$lisovsky_num[536] <- "511"
test_tbl$full_title[536] <- "Самарскія Губ. Вѣд"

test_tbl$lisovsky_num[744] <- "712"
test_tbl$full_title[744] <- "Кіевскія Епарх. Вѣд"

test_tbl$lisovsky_num[828] <- "792"
test_tbl$full_title[828] <- "Кубанскія Войсков. Вѣд"

test_tbl$lisovsky_num[864] <- "827"
test_tbl$full_title[864] <- "Вологодскія Епарх. Вѣд"

test_tbl$lisovsky_num[903] <- "862"
test_tbl$full_title[903] <- "Забайкальскія Област. Вѣд"

test_tbl$lisovsky_num[932] <- "890"
test_tbl$full_title[932] <- "Воронежскія Епарх. Вѣд"

test_tbl$lisovsky_num[978] <- "935"
test_tbl$full_title[978] <- "Варшавскія Губ. Вѣд"

test_tbl$lisovsky_num[987] <- "944"
test_tbl$full_title[987] <- "Волынскія Епарх. Вѣд"

test_tbl$lisovsky_num[994] <- "951"
test_tbl$full_title[994] <- "Вѣдомости Одесскаго Градоначальства"

test_tbl$lisovsky_num[1022] <- "978"
test_tbl$full_title[1022] <- "Терскія Областн. Вѣдомости"

test_tbl$lisovsky_num[2145] <- "1973"
test_tbl$full_title[2145] <- "Архангельскія Епарх. Вѣд"

test_tbl$lisovsky_num[2684] <- "2463"
test_tbl$full_title[2684] <- "Владикавказскія Епархіальныя Вѣдомости"

test_tbl$lisovsky_num[2928] <- "2693"
test_tbl$full_title[2928] <- "Олонецкія Епарх. Вѣдом"

test_tbl$lisovsky_num[3137] <- "2880"
test_tbl$full_title[3137] <- "Забайкальскія Епарх. Вѣдом"

# - вытащить записи с лисовски_айди, которых нет в таблице (как, например, 2880) -- лежат в l_out
z <- test_tbl %>% select(lisovsky_num) %>% filter(lisovsky_num != "")
l_out <- anti_join(tibble(lisovsky_num = as.character(1:2879)), z)
# 29, 392, 402, 403, 404, 415, 416, 417, 1109: странный адрес у страницы, внести руками
# 951 косо спарсилось, неправильно вытащился хтмл и клин текст, названия и номер поправил руками


odds_tbl <- tibble(
  id = character(),
  title = character(),
  whole_txt_block = character())

odd_lis_nums <- c(29, 392, 402, 403, 404, 415, 416, 417, 1109)
odd_links <- c("https://feb-web.ru/feb/periodic/lb-abc/lb1/lb1-013a.htm?cmd=2&istext=1", # 29
               "https://feb-web.ru/feb/periodic/lb-abc/lb1/lb1-091a.htm?cmd=2&istext=1", # 392
               "https://feb-web.ru/feb/periodic/lb-abc/lb1/lb1-092a.htm?cmd=2&istext=1", # 402
               "https://feb-web.ru/feb/periodic/lb-abc/lb1/lb1-092b.htm?cmd=2&istext=1", # 403
               "https://feb-web.ru/feb/periodic/lb-abc/lb1/lb1-092c.htm?cmd=2&istext=1", # 404
               "https://feb-web.ru/feb/periodic/lb-abc/lb1/lb1-093b.htm?cmd=2&istext=1", # 415
               "https://feb-web.ru/feb/periodic/lb-abc/lb1/lb1-093c.htm?cmd=2&istext=1", # 416
               "https://feb-web.ru/feb/periodic/lb-abc/lb1/lb1-093d.htm?cmd=2&istext=1", # 417
               "https://feb-web.ru/feb/periodic/lb-abc/lb1/lb1-2793-2.htm?cmd=2&istext=1") # 1109 
odd_ids <- c("013a", # 29
             "091a", # 392
             "092a", # 402
             "092b", # 403
             "092c", # 404
             "093b", # 415
             "093c", # 416
             "093d", # 417
             "2793-2") # 1109

process_page_ad_hoc <- function(page, page_id) {
  # название на новом русском
  name_modern <- html_nodes(page, xpath="//meta[@name='title']") %>% 
    html_attr(name="content")
  # весь текстовый блок -- узлом, чтобы потом можно было доковыряться и вообще проверить
  block <- html_element(page, ".text")
  
  # block_list <- paste(capture.output(block, file=NULL), collapse="")
  block_list <- as.character(block)
  
  odds_tbl <<- odds_tbl %>% 
    add_row(id = page_id,
            title = name_modern,
            whole_txt_block = block_list)
}

for (i in 1:length(odd_links)) {
  page <- read_html(odd_links[i])
  process_page_ad_hoc(page, odd_ids[i])
}

odds_tbl <- odds_tbl %>% 
  mutate(year=c("1772",
                "1838",
                "1838",
                "1838",
                "1838",
                "1838",
                "1838",
                "1838",
                "1872")) %>% 
  mutate(lisovsky_num = odd_lis_nums) %>% 
  mutate(clean_txt = map_chr(whole_txt_block, html2txt)) %>% 
  mutate(first_title = map_chr(whole_txt_block, get_first_title)) %>% 
  mutate(full_title = map_chr(whole_txt_block, get_full_title)) %>% 
  mutate(names = map_chr(whole_txt_block, get_names)) %>% 
  mutate(multiple_titles = map(whole_txt_block, multiple_titles)) %>% 
  mutate(multiple_items = map(clean_txt, multiple_items)) %>% 
  mutate(html_data = map_chr(whole_txt_block, remove_pages)) %>% 
  mutate(clean_data = map_chr(html_data, html2txt))

odds_tbl <- odds_tbl %>% 
    select(-clean_txt)

test_tbl <- rbind(test_tbl, odds_tbl) 

test_tbl <- test_tbl %>% 
  arrange(year) %>% 
  unique()


no_lis_num <- test_tbl %>% 
  filter(lisovsky_num == "") %>% 
  group_by(year) %>% 
  count()

by_years <- test_tbl %>%
  select(year, lisovsky_num) %>%
  filter(lisovsky_num != "") %>% 
  group_by(year) %>% 
  summarize(min = min(lisovsky_num), max = max(lisovsky_num))



# исправляю ошибку с годами
test_tbl$year[13] <- "1763"
test_tbl$year[21] <- "1769"
test_tbl$year[46] <- "1777"
test_tbl$year[67] <- "1784"
test_tbl$year[102] <- "1792"
test_tbl$year[130] <- "1800"
test_tbl$year[135] <- "1801"
test_tbl$year[197] <- "1809"
test_tbl$year[198] <- "1809"
test_tbl$year[200] <- "1809"
test_tbl$year[201] <- "1809"
test_tbl$year[202] <- "1809"
test_tbl$year[203] <- "1809"
test_tbl$year[204] <- "1809"
test_tbl$year[205] <- "1809"
test_tbl$year[206] <- "1809"
test_tbl$year[199] <- "1809"
test_tbl$year[2620] <- "1895"





# оставляем в рабочем тиббле только нужную информацию

zapasnoy <- test_tbl

test_tbl <- zapasnoy %>% 
  select(id, 
         year,
         lisovsky_num,
         title, 
         first_title,
         full_title,
         names,
         format,
         frequency,
         multiple_items, 
         multiple_titles,
         whole_txt_block, 
         html_data,
         clean_data)

test_tbl <- test_tbl %>% 
  mutate(multiple_items = as.character(multiple_items)) %>% 
  mutate(multiple_titles = as.character(multiple_titles)) 
  
  


to_export <- test_tbl

write_csv(to_export, file="lisovsky.csv")

write_csv(topics_test, file="lis_topics.csv")

write_csv(t_names_tbl, file="lis_names.csv")










test_block <- accum_tbl$whole_txt_block[3]

get_first_par(test_block)
get_full_title(test_block)











  



to_export <- test_tbl %>% 
  select(year, title, first_title, multiple_titles, multiple_items, clean_txt, whole_txt_block) %>% 
  mutate(multiple_titles = as.character(multiple_titles)) %>% 
  mutate(multiple_items = as.character(multiple_items)) 

write_csv(to_export, file="lisovsky_first_try.csv")

# КАК СОБРАТЬ ВСЕ ЭТИ ССЫЛКИ ИЛИ ХОТЯ БЫ КОНСТРУКТОР, ИЗ КОТОРОГО МОЖНО БУДЕТ
# СОБРАТЬ ТАКИЕ ПРАВИЛЬНЫЕ ССЫЛКИ?




ids <- c(0012,
         0021,
         0051,
         0052,
         0061,
         0081,
         0082,
         0091,
         0092,
         0094,
         0101,
         0102,
         0121,
         0122,
         0124,
         0134,
         0137,
         0138,
         0141,
         0143,
         0151,
         0161,
         0164,
         0171,
         0175,
         0183,
         0191,
         0193,
         0211,
         0215,
         0222,
         0231,
         0242,
         0256,
         0272,
         0282,
         0301,
         0306,
         0315,
         0323,
         0332,
         0344,
         0346,
         0353,
         0357,
         0363,
         0371,
         0385,
         0393,
         0412,
         0425,
         0445,
         0456,
         0492,
         0499,
         0506,
         0534,
         0551,
         0553,
         0581,
         0596,
         0614,
         0625,
         0626,
         0631,
         0645,
         0661,
         0671,
         0675,
         0694,
         0713,
         0723,
         0751,
         0763,
         0771,
         0796,
         0805,
         0824,
         0842,
         0853,
         0873,
         0894,
         0951,
         0974,
         0995,
         1034,
         1062,
         1074,
         1081,
         1101,
         1121,
         1143,
         1162,
         1165,
         1175,
         1201,
         1216,
         1231,
         1243,
         1271,
         1312,
         1375,
         1481,
         1574,
         1676,
         1774,
         1845,
         1936,
         2011,
         2096,
         2174,
         2307,
         2393,
         2521,
         2641,
         2756,
         2891,
         2993,
         3083,
         3174,
         3303,
         3434,
         3564,
         3714,
         3851,
         4061,
         4244,
         4382,
         4565,
         4734,
         4881,
         5005,
         5141,
         5263,
         5412,
         5573,
         5732,
         5891,
         6092,
         6233,
         6393,
         6544,
         6694,
         6873)

years <- c(1703,
           1728,
           1748,
           1755,
           1756,
           1758,
           1759,
           1760,
           1762,
           1763,
           1764,
           1765,
           1766,
           1767,
           1769,
           1770,
           1771,
           1772,
           1773,
           1774,
           1775,
           1776,
           1777,
           1778,
           1779,
           1780,
           1781,
           1782,
           1783,
           1784,
           1785,
           1786,
           1787,
           1788,
           1789,
           1790,
           1791,
           1792,
           1793,
           1794,
           1795,
           1796,
           1798,
           1799,
           1800,
           1801,
           1802,
           1803,
           1804,
           1805,
           1806,
           1807,
           1808,
           1810,
           1811,
           1812,
           1813,
           1814,
           1815,
           1816,
           1817,
           1818,
           1819,
           1820,
           1821,
           1822,
           1823,
           1824,
           1825,
           1826,
           1827,
           1828,
           1829,
           1830,
           1831,
           1832,
           1833,
           1834,
           1835,
           1836,
           1837,
           1838,
           1839,
           1840,
           1841,
           1842,
           1843,
           1844,
           1845,
           1846,
           1847,
           1848,
           1849,
           1850,
           1851,
           1852,
           1853,
           1854,
           1855,
           1856,
           1857,
           1858,
           1859,
           1860,
           1861,
           1862,
           1863,
           1864,
           1865,
           1866,
           1867,
           1868,
           1869,
           1870,
           1871,
           1872,
           1873,
           1874,
           1875,
           1876,
           1877,
           1878,
           1879,
           1880,
           1881,
           1882,
           1883,
           1884,
           1885,
           1886,
           1887,
           1888,
           1889,
           1890,
           1891,
           1892,
           1893,
           1894,
           1895,
           1896,
           1897,
           1898,
           1899,
           1900)