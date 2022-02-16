### 0. 감정사전 불러오기

install.packages("tidyverse")
install.packages("tidytext")

library(dplyr)
library(readr)
dic <- read_csv("knu_sentiment_lexicon.csv")

head(dic)

new_dic <- dic %>%
  mutate(polarity = ifelse(word %in% c("있지만"), 0, polarity))

dic %>%
  filter(polarity == 2) %>%
  arrange(word)

dic %>%
  filter(polarity == -2) %>%
  arrange(word)

#분석할 텍스트파일 불러오기

raw_comment= read_csv("survey_sentimental2.csv",locale=locale('ko',encoding='euc-kr'))

#전처리
#install.packages("textclean")
#library(textclean)

#토큰화
library(tidytext)
word_comment1 <- raw_comment %>%
  unnest_tokens(input = sem1,
                output = word,
                token = "words",
                drop = F)

word_comment2 <- raw_comment %>%
  unnest_tokens(input = sem2,
                output = word,
                token = "words",
                drop = F)

word_comment3 <- raw_comment %>%
  unnest_tokens(input = sem3,
                output = word,
                token = "words",
                drop = F)

#감정점수 부여
word_comment1 <- word_comment1 %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

word_comment2 <- word_comment2 %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

word_comment3 <- word_comment3 %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

#감정 분류하기
word_comment1 <- word_comment1 %>%
  mutate(sentiment = ifelse(polarity == 2, "pos",
                     ifelse(polarity == -2, "neg", "neu")))

word_comment2 <- word_comment2 %>%
  mutate(sentiment = ifelse(polarity == 2, "pos",
                            ifelse(polarity == -2, "neg", "neu")))

word_comment3 <- word_comment3 %>%
  mutate(sentiment = ifelse(polarity == 2, "pos",
                            ifelse(polarity == -2, "neg", "neu")))

### 1. top10 막대그래프 만들기


library(ggplot2)

top10_sentiment1 <- word_comment1 %>%
  filter(sentiment != "neu") %>%
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10)

top10_sentiment1

ggplot(top10_sentiment1, aes(x = reorder(word, n),
                             y = n,
                             fill = sentiment)) +
  geom_col()+
  coord_flip()+
  #geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = NULL, y = '2020년도 1학기') +
  theme(text = element_text(family = "nanumgothic"))

##############
top10_sentiment2 <- word_comment2 %>%
  filter(sentiment != "neu") %>%
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10)

top10_sentiment2

ggplot(top10_sentiment2, aes(x = reorder(word, n),
                             y = n,
                             fill = sentiment)) +
  geom_col()+
  coord_flip()+
  #geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = NULL, y = '2020년도 2학기') +
  theme(text = element_text(family = "nanumgothic"))

################
top10_sentiment3 <- word_comment3 %>%
  filter(sentiment != "neu") %>%
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10)

top10_sentiment3

ggplot(top10_sentiment3, aes(x = reorder(word, n),
                             y = n,
                             fill = sentiment)) +
  geom_col()+
  coord_flip()+
  #geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = NULL, y = '2021년도 1학기') +
  theme(text = element_text(family = "nanumgothic"))

### 2. 댓글별 감정점수 구하기
score_comment1 <- word_comment1 %>%
  group_by(sem1) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

score_comment1 %>%
  select(score, sem1)

score_comment1 %>%
  select(score, sem1) %>%
  arrange(-score)

score_comment1 %>%
  select(score, sem1) %>%
  arrange(score)

#감정 경향 살펴보기
score_comment1 %>%
  count(score) %>%
  print(n = Inf)

score_comment1 <- score_comment1 %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                     ifelse(score <= -1, "neg", "neu")))

frequency_score1 <- score_comment1 %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)

frequency_score1

frequency_score1$dummy <- 0

ggplot(frequency_score1, aes(x = dummy, y = ratio, fill = sentiment)) +
  geom_col() +
  coord_fixed(ratio = 0.05) +
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme (axis.text.x = element_blank(),
         axis.ticks.x = element_blank()) +
  labs(x = '2020년도 1학기', y = NULL)


####################
score_comment2 <- word_comment2 %>%
  group_by(sem2) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

score_comment2 <- score_comment2 %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))

frequency_score2 <- score_comment2 %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)

frequency_score2$dummy <- 0

ggplot(frequency_score2, aes(x = dummy, y = ratio, fill = sentiment)) +
  geom_col() +
  coord_fixed(ratio = 0.05) +
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = '2020년도 2학기', y = NULL)

####################
score_comment3 <- word_comment3 %>%
  group_by(sem3) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

score_comment3 <- score_comment3 %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))

frequency_score3 <- score_comment3 %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)

frequency_score3$dummy <- 0

ggplot(frequency_score3, aes(x = dummy, y = ratio, fill = sentiment)) +
  geom_col() +
  coord_fixed(ratio = 0.05) +
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = '2021년도 1학기', y = NULL)


library("stringr")

### 3. 감정 범주별 단어 빈도 구하기
comment1 <- score_comment1 %>%
  unnest_tokens(input = sem1,
                output = word,
                token = "words",
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") & #한글 추출
         str_count(word) >= 2) #두 글자 이상 추출

frequency_word1 <- comment1 %>%
  count(sentiment, word, sort = T)

frequency_word1 %>%
  filter(sentiment == "pos")

frequency_word1 %>%
  filter(sentiment == "neg")

#상대적으로 자주 사용된 단어 비교
library(tidyr)

comment_wide1 <- frequency_word1 %>%
  filter(sentiment != "neu") %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n =0))

comment_wide1

comment_wide1 <- comment_wide1 %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                              ((neg + 1) / (sum(neg + 1)))))

comment_wide1         

#로그 오즈비가 가장 큰 단어 10개씩 추출
top10_1 <- comment_wide1 %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10)

top10_1 %>% print(n = Inf)

#로그 오즈비가 동점인 단어 제외하고 추출
top10_1 <- comment_wide1 %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

top10_1 %>% print(n = Inf)

ggplot(top10_1, aes(x = reorder(word, log_odds_ratio),
                    y = log_odds_ratio,
                    fill = sentiment)) +
  geom_col()+
  coord_flip() +
  labs(x = NULL, y = "2020년 1학기") +
  theme(text = element_text(family = "nanumgothic"))


##############################
comment2 <- score_comment2 %>%
  unnest_tokens(input = sem2,
                output = word,
                token = "words",
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") & #한글 추출
           str_count(word) >= 2) #두 글자 이상 추출

frequency_word2 <- comment2 %>%
  count(sentiment, word, sort = T)

frequency_word2 %>%
  filter(sentiment == "pos")

frequency_word2 %>%
  filter(sentiment == "neg")

comment_wide2 <- frequency_word2 %>%
  filter(sentiment != "neu") %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n =0))

comment_wide2 <- comment_wide2 %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                ((neg + 1) / (sum(neg + 1)))))


top10_2 <- comment_wide2 %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

top10_2 %>% print(n = Inf)

ggplot(top10_2, aes(x = reorder(word, log_odds_ratio),
                    y = log_odds_ratio,
                    fill = sentiment)) +
  geom_col()+
  coord_flip() +
  labs(x = NULL, y = "2020년 2학기") +
  theme(text = element_text(family = "nanumgothic"))


##############################
comment3 <- score_comment3 %>%
  unnest_tokens(input = sem3,
                output = word,
                token = "words",
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") & #한글 추출
           str_count(word) >= 2) #두 글자 이상 추출

frequency_word3 <- comment3 %>%
  count(sentiment, word, sort = T)

frequency_word3 %>%
  filter(sentiment == "pos")

frequency_word3 %>%
  filter(sentiment == "neg")

comment_wide3 <- frequency_word3 %>%
  filter(sentiment != "neu") %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n =0))

comment_wide3 <- comment_wide3 %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                ((neg + 1) / (sum(neg + 1)))))


top10_3 <- comment_wide3 %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

top10_3 %>% print(n = Inf)

ggplot(top10_3, aes(x = reorder(word, log_odds_ratio),
                    y = log_odds_ratio,
                    fill = sentiment)) +
  geom_col()+
  coord_flip() +
  labs(x = NULL, y = "2021년 1학기") +
  theme(text = element_text(family = "nanumgothic"))
