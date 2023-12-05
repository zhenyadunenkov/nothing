# install.packages(c("rvest", "purrr", "tidyverse", "stringr", "tidytext", "udpipe"))

library(rvest)
library(purrr)
library(tidyverse)
library(stringr)

library(tidytext)
library(udpipe)

# ---------------------------------------------------
# СКАЧИВАЕМ И ОБРАБАТЫВАЕМ "ПРЕСТУПЛЕНИЕ И НАКАЗАНИЕ"
# ---------------------------------------------------

dost_url <- "https://ilibrary.ru/text/69/index.html"

# ПОЛУЧАЕМ СОДЕРЖАНИЕ СО ССЫЛКАМИ НА ГЛАВЫ

dost_contents <- read_html(dost_url, encoding = "windows-1251") %>% 
  html_elements(".t")
dost_contents <- dost_contents[2] 
dost_contents <- dost_contents%>% 
  html_elements("a")

dost_tibble <- tibble(
  parts = numeric(),
  chapters = character(),
  url = character(),
  content = character(),
)

part <- 0

for (elem in seq_along(dost_contents)) {
  current_element <- dost_contents[elem]
  chapter <- html_text2(current_element)
  if (html_text2(current_element) == "") {
    next
  }
  if (grepl("»", chapter, fixed = TRUE)) {
    next
  }
  if (grepl("Часть", chapter, fixed = TRUE)) {
    part <- part + 1
    next
  }
  if (grepl("Эпилог", chapter, fixed = TRUE)) {
    part <- part + 1
    next
  }
  link <- html_attr(current_element, "href")
  dost_tibble <- add_row(dost_tibble, parts = part, chapters = chapter, url = link)
}

# ИДЁМ ПО СОДЕРЖАНИЮ И СОБИРАЕМ ТЕКСТЫ ГЛАВ

get_text <- function(url) {
  url <- paste0("https://ilibrary.ru", url)
  print(paste("Парсим", url))
  page <- read_html(url, encoding = "windows-1251")
  paragraphs <- html_elements(page, ".p")
  txt <- ""
  for (p in (1 : (length(paragraphs)-1))) {
    paragraph <- html_text2(paragraphs[p]) %>% 
      str_replace_all("\u0097", "--")
    txt <- paste(txt, paragraph)
  }
  Sys.sleep(0.1)
  return(txt)
}

dost_tibble <- dost_tibble %>% 
  mutate(content = map_chr(url, get_text)) 

dost_tbl <- dost_tibble %>% 
  select(-url) %>% 
  mutate(id = 1:nrow(dost_tibble)) %>% 
  mutate(doc_num = paste0("doc", as.character(id))) %>% 
  select(-id)


# ЛЕММАТИЗАЦИЯ

# udpipe_download_model(language = "russian-gsd")
russian_gsd <- udpipe_load_model(file = "russian-gsd-ud-2.5-191206.udpipe")

dost_ann <- udpipe_annotate(russian_gsd, dost_tbl$content) %>% 
  as_tibble()

dost_source <- dost_ann %>% 
  select(doc_id, sentence_id, upos) %>% 
  filter(upos != "PUNCT")

# ДОБАВЛЯЕМ ЧАСТИ И ГЛАВЫ

parts_and_chapters <- dost_tbl %>% 
  select(doc_num, parts, chapters)

colnames(parts_and_chapters) <- c("doc_id", "part", "chapter")

dost_source <- left_join(dost_source, parts_and_chapters) %>% 
  relocate(doc_id, .before = sentence_id) %>% 
  relocate(part, .before = sentence_id) %>%
  relocate(chapter, .before = sentence_id) %>% 
  select(part, chapter, sentence_id, upos)


# ---------------------------------------------------
# ИССЛЕДУЕМ ДАННЫЕ
# ---------------------------------------------------

# ДЛИНЫ ПРЕДЛОЖЕНИЙ

# Ниже длины предложений по главам, интересных закономерностей не найдено
# sent_length <- dost_source %>% 
#   select(-upos) %>% 
#   group_by(part, chapter) %>% 
#   count(sentence_id) %>% 
#   mutate(p_ch = paste0(part, "-", chapter))
# 
# ggplot(sent_length, aes(x=sentence_id, y=n)) + 
#   geom_col() +
#   facet_wrap(~p_ch) +
#   theme_test() 
# 

# Заодно посмотрим распределение длин предложений по всему произведению

total_sent_length <- dost_source %>% 
  select(-upos) %>%
  group_by(part, chapter) %>%
  count(sentence_id) %>%
  ungroup() %>% 
  select(n)

ggplot(total_sent_length, aes(n)) + 
  geom_histogram(binwidth = 1, color="#9999CC", fill="#9999CC") +
  theme_test() +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) +
  ylab("Number of sentences") +
  xlab("Words in a sentence") 

# ИЗУЧАЕМ РИСУНОК ЧАСТЕЙ РЕЧИ

# Ниже версия, в которой мы считаем сущ-глагол-сущ и глагол-сущ-сущ разными типами
# (оказалось, что в такой версии слишком много уникальных предложений)
# parts_order <- dost_source %>% 
#   group_by(part, chapter, sentence_id) %>% 
#   mutate(order = paste0(upos, collapse = ", ")) %>% 
#   ungroup() %>% 
#   select(-upos) %>% 
#   unique() %>% 
#   select(order) %>% 
#   count(order)


parts_order <- dost_source %>%
  group_by(part, chapter, sentence_id) %>%
  arrange(part, chapter, sentence_id, upos) %>% 
  mutate(order = paste0(upos, collapse = ", ")) %>%
  ungroup() %>%
  select(-upos) %>%
  unique() %>%
  select(order) %>%
  count(order) %>% 
  arrange(-n) 

ggplot(parts_order, aes(x=n)) + 
  geom_histogram(color="#9999CC", fill="#9999CC") +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  theme_test() +
  ylab("Number of sentences") +
  xlab("Number of occurences in the text") 




# ПОДСЧИТЫВАЕМ ЧАСТИ РЕЧИ ПО ГЛАВАМ

parts_of_speech_by_chapters <- dost_source %>% 
  filter(upos %in% c("ADV", "ADJ", "NOUN", "VERB")) %>% 
  group_by(part, chapter, upos) %>% 
  count(part, chapter, upos) %>% 
  mutate(p_ch = paste0(part, "-", chapter)) 

ggplot(parts_of_speech_by_chapters, aes(x=upos, y=n, fill=upos)) + 
  geom_col() +
  facet_wrap(~p_ch) +
  theme_test()

# СЧИТАЕМ КОЭФФИЦИЕНТ ДЕЙСТВИЯ ПО ГЛАВАМ

normalize <- function(value) {
  z <- (value - min(verb_coeff$verb_c)) / (max(verb_coeff$verb_c) - min(verb_coeff$verb_c)) %>% 
    round(2)
  return(z)
}

verb_coeff <- parts_of_speech_by_chapters %>% 
  ungroup() %>% 
  pivot_wider(names_from = upos, values_from = n) %>% 
  mutate(verb_c = VERB / (ADJ + ADV + NOUN)) %>% 
  select(p_ch, verb_c) %>% 
  mutate(verb_c_normalized = map_dbl(verb_c, normalize)) %>% 
  left_join(parts_of_speech_by_chapters)
  

ggplot(verb_coeff, aes(x=upos, y=n, fill=upos, alpha = verb_c)) + 
  geom_col() +
  facet_wrap(~p_ch) +
  theme_test() +
  scale_fill_manual(values=c("gray", "#9999CC", "#66CC99", "#c1121f"),
                    name = "Parts of speech") +
  scale_alpha(guide = 'none') 



total_parts <- dost_source %>% 
  filter(upos %in% c("ADV", "ADJ", "NOUN", "VERB")) %>% 
  count(upos)

total_verb_coeff <- total_parts$n[4] / (total_parts$n[1] + total_parts$n[2] + total_parts$n[3])
  

contrast <- function(value) {
  if (value < total_verb_coeff) {
    return(0)}
  if (value < total_verb_coeff + (1 - total_verb_coeff) / 2) {
    return(0.5)}
  if (value > total_verb_coeff + (1 - total_verb_coeff) / 2) {
    return(1)}
      
  }


v <- verb_coeff %>% 
  mutate(verb_c_contrast = map_dbl(verb_c_normalized, contrast))


ggplot(v, aes(x=upos, y=n, fill=upos, alpha = verb_c_contrast)) + 
  geom_col() +
  facet_wrap(~p_ch) +
  theme_test() +
  scale_fill_manual(values=c("gray", "#9999CC", "#66CC99", "#c1121f"),
                    name = "Parts of speech") +
  scale_alpha(guide = 'none') 


