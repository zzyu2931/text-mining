setwd("C:/R")


library(rJava)
library(memoise)
library(KoNLP)
library(stringr)
library(devtools)
library(wordcloud2)

buildDictionary(ext_dic="woorimalsam")
user_d <- data.frame(term = "오프라인인", tag='ncn')
buildDictionary(ext_dic="woorimalsam",user_dic=user_d)

library(readr)
wc <- read_csv("survey_wordcloud.csv",locale=locale('ko',encoding='euc-kr'))



#2020 - 1학기 1번문항
ctl1 <- wc$a1
ctl2 <- wc$a2
ctl3 <- wc$a3
ctl4 <- wc$a4
ctl5 <- wc$a5
ctl6 <- wc$a6
survey_1 <- sapply(ctl1, extractNoun, USE.NAMES=F)
survey_2 <- sapply(ctl2, extractNoun, USE.NAMES=F)
survey_3 <- sapply(ctl3, extractNoun, USE.NAMES=F)
survey_4 <- sapply(ctl4, extractNoun, USE.NAMES=F)
survey_5 <- sapply(ctl5, extractNoun, USE.NAMES=F)
survey_6 <- sapply(ctl6, extractNoun, USE.NAMES=F)


#1
noun<- unlist(survey_1)
wordcount1 <- table(noun)
head(sort(wordcount1,decreasing=TRUE),100)

noun <- gsub("강의","",noun)
noun <- gsub("생각","",noun)
noun <- gsub("진행","",noun) 
noun <- gsub("수강","",noun) 
noun <- gsub("수업","",noun)
noun <- gsub("사용","",noun) 
noun <- gsub("시간","",noun)
noun <- gsub("상황","",noun) 
noun <- gsub("경우","",noun)
noun <- gsub("하지","",noun)
noun <- gsub("하시","",noun)
noun <- gsub("들이","",noun)
noun <- gsub("하기","",noun)
noun <- gsub("해서","",noun)
noun <- gsub("때문","",noun)
noun <- gsub("해주","",noun)
noun <- gsub("관련","",noun)
noun <- gsub("정하","",noun)
noun <- gsub("정해","",noun)
noun <- gsub("정한","",noun)
noun <- gsub("정도","",noun)
noun <- gsub("어디","",noun)
noun <- gsub("없습니","",noun)
noun <- gsub("만큼","",noun)
noun <- gsub("온라인","",noun)
noun <- gsub("교수님","",noun)
noun <- gsub("전반적","",noun) 
noun <- gsub("학생","",noun)
noun <- gsub("교수","",noun)
noun <- gsub("부분","",noun)
noun <- gsub("좋겠","",noun)
noun <- gsub("좋았","",noun)
noun <- gsub("좋겠습니","",noun)
noun <- gsub("동안","",noun)

wordcount1 <- table(noun)
head(sort(wordcount1,decreasing=TRUE),100)

noun <- noun[nchar(noun)>1]
wordcount1 <- table(noun)
head(sort(wordcount1,decreasing=TRUE),100)


wordcount1 <- head(sort(wordcount1, decreasing = T), 100)
#wordcount <- data.frame(worrdcount)
#wordcount <- wordcount[!(wordcoun4$Freq < 3), ]


wordcount1

set.seed(1234)
pal <- brewer.pal(12, "Paired")[1:12]
wordcloud2(data = wordcount1,
           size = 0.7,
           fontFamily = "Tmon몬소리",
           color = pal)


#2
noun<- unlist(survey_2)
wordcount2 <- table(noun)
head(sort(wordcount2,decreasing=TRUE),100)

noun <- gsub("강의","",noun)
noun <- gsub("생각","",noun)
noun <- gsub("진행","",noun) 
noun <- gsub("수강","",noun) 
noun <- gsub("수업","",noun)
noun <- gsub("사용","",noun) 
noun <- gsub("시간","",noun)
noun <- gsub("상황","",noun) 
noun <- gsub("경우","",noun)
noun <- gsub("하지","",noun)
noun <- gsub("하시","",noun)
noun <- gsub("들이","",noun)
noun <- gsub("하기","",noun)
noun <- gsub("해서","",noun)
noun <- gsub("때문","",noun)
noun <- gsub("해주","",noun)
noun <- gsub("관련","",noun)
noun <- gsub("정하","",noun)
noun <- gsub("정해","",noun)
noun <- gsub("정한","",noun)
noun <- gsub("정도","",noun)
noun <- gsub("어디","",noun)
noun <- gsub("없습니","",noun)
noun <- gsub("만큼","",noun)
noun <- gsub("온라인","",noun)
noun <- gsub("교수님","",noun)
noun <- gsub("전반적","",noun) 
noun <- gsub("학생","",noun)
noun <- gsub("교수","",noun)
noun <- gsub("부분","",noun)
noun <- gsub("좋겠","",noun)
noun <- gsub("좋았","",noun)
noun <- gsub("좋겠습니","",noun)
noun <- gsub("동안","",noun)
noun <- gsub("습니니","",noun)

wordcount2 <- table(noun)
head(sort(wordcount2,decreasing=TRUE),100)

noun <- noun[nchar(noun)>1]
wordcount2 <- table(noun)
head(sort(wordcount2,decreasing=TRUE),100)


wordcount2 <- head(sort(wordcount2, decreasing = T), 100)
#wordcount <- data.frame(worrdcount)
#wordcount <- wordcount[!(wordcoun4$Freq < 3), ]



wordcloud2(data = wordcount2,
           size = 0.7,
           fontFamily = "Tmon몬소리",
           color = pal)


#3
noun<- unlist(survey_3)
wordcount3 <- table(noun)
head(sort(wordcount3,decreasing=TRUE),100)

noun <- gsub("강의","",noun)
noun <- gsub("생각","",noun)
noun <- gsub("진행","",noun) 
noun <- gsub("수강","",noun) 
noun <- gsub("수업","",noun)
noun <- gsub("사용","",noun) 
noun <- gsub("시간","",noun)
noun <- gsub("상황","",noun) 
noun <- gsub("경우","",noun)
noun <- gsub("하지","",noun)
noun <- gsub("하시","",noun)
noun <- gsub("들이","",noun)
noun <- gsub("하기","",noun)
noun <- gsub("해서","",noun)
noun <- gsub("때문","",noun)
noun <- gsub("해주","",noun)
noun <- gsub("관련","",noun)
noun <- gsub("정하","",noun)
noun <- gsub("정해","",noun)
noun <- gsub("정한","",noun)
noun <- gsub("정도","",noun)
noun <- gsub("어디","",noun)
noun <- gsub("없습니","",noun)
noun <- gsub("만큼","",noun)
noun <- gsub("온라인","",noun)
noun <- gsub("교수님","",noun)
noun <- gsub("전반적","",noun) 
noun <- gsub("학생","",noun)
noun <- gsub("교수","",noun)
noun <- gsub("부분","",noun)
noun <- gsub("좋겠","",noun)
noun <- gsub("좋았","",noun)
noun <- gsub("좋겠습니","",noun)
noun <- gsub("동안","",noun)
noun <- gsub("습니니","",noun)

wordcount3 <- table(noun)
head(sort(wordcount3,decreasing=TRUE),100)

noun <- noun[nchar(noun)>1]
wordcount3 <- table(noun)
head(sort(wordcount3,decreasing=TRUE),100)


wordcount3 <- head(sort(wordcount3, decreasing = T), 100)
#wordcount <- data.frame(worrdcount)
#wordcount <- wordcount[!(wordcoun4$Freq < 3), ]



wordcloud2(data = wordcount3,
           size = 0.7,
           fontFamily = "Tmon몬소리",
           color = pal)


#4
noun<- unlist(survey_4)
wordcount4 <- table(noun)
head(sort(wordcount4,decreasing=TRUE),100)

noun <- gsub("강의","",noun)
noun <- gsub("생각","",noun)
noun <- gsub("진행","",noun) 
noun <- gsub("수강","",noun) 
noun <- gsub("수업","",noun)
noun <- gsub("사용","",noun) 
noun <- gsub("시간","",noun)
noun <- gsub("상황","",noun) 
noun <- gsub("경우","",noun)
noun <- gsub("하지","",noun)
noun <- gsub("하시","",noun)
noun <- gsub("들이","",noun)
noun <- gsub("하기","",noun)
noun <- gsub("해서","",noun)
noun <- gsub("때문","",noun)
noun <- gsub("해주","",noun)
noun <- gsub("관련","",noun)
noun <- gsub("정하","",noun)
noun <- gsub("정해","",noun)
noun <- gsub("정한","",noun)
noun <- gsub("정도","",noun)
noun <- gsub("어디","",noun)
noun <- gsub("없습니","",noun)
noun <- gsub("만큼","",noun)
noun <- gsub("온라인","",noun)
noun <- gsub("교수님","",noun)
noun <- gsub("전반적","",noun) 
noun <- gsub("학생","",noun)
noun <- gsub("교수","",noun)
noun <- gsub("부분","",noun)
noun <- gsub("좋겠","",noun)
noun <- gsub("좋았","",noun)
noun <- gsub("좋겠습니","",noun)
noun <- gsub("동안","",noun)
noun <- gsub("습니","",noun)

wordcount4 <- table(noun)
head(sort(wordcount4,decreasing=TRUE),100)

noun <- noun[nchar(noun)>1]
wordcount4 <- table(noun)
head(sort(wordcount4,decreasing=TRUE),100)


wordcount4 <- head(sort(wordcount4, decreasing = T), 100)
#wordcount <- data.frame(worrdcount)
#wordcount <- wordcount[!(wordcoun4$Freq < 3), ]



wordcloud2(data = wordcount4,
           size = 0.7,
           fontFamily = "Tmon몬소리",
           color = pal)



#5
noun<- unlist(survey_5)
wordcount5 <- table(noun)
head(sort(wordcount5,decreasing=TRUE),100)

noun <- gsub("강의","",noun)
noun <- gsub("생각","",noun)
noun <- gsub("진행","",noun) 
noun <- gsub("수강","",noun) 
noun <- gsub("수업","",noun)
noun <- gsub("사용","",noun) 
noun <- gsub("시간","",noun)
noun <- gsub("상황","",noun) 
noun <- gsub("경우","",noun)
noun <- gsub("하지","",noun)
noun <- gsub("하시","",noun)
noun <- gsub("들이","",noun)
noun <- gsub("하기","",noun)
noun <- gsub("해서","",noun)
noun <- gsub("때문","",noun)
noun <- gsub("해주","",noun)
noun <- gsub("관련","",noun)
noun <- gsub("정하","",noun)
noun <- gsub("정해","",noun)
noun <- gsub("정한","",noun)
noun <- gsub("정도","",noun)
noun <- gsub("어디","",noun)
noun <- gsub("없습니","",noun)
noun <- gsub("만큼","",noun)
noun <- gsub("온라인","",noun)
noun <- gsub("교수님","",noun)
noun <- gsub("전반적","",noun) 
noun <- gsub("학생","",noun)
noun <- gsub("교수","",noun)
noun <- gsub("부분","",noun)
noun <- gsub("좋겠","",noun)
noun <- gsub("좋았","",noun)
noun <- gsub("좋겠습니","",noun)
noun <- gsub("동안","",noun)
noun <- gsub("습니","",noun)

wordcount5 <- table(noun)
head(sort(wordcount5,decreasing=TRUE),100)

noun <- noun[nchar(noun)>1]
wordcount5 <- table(noun)
head(sort(wordcount5,decreasing=TRUE),100)


wordcount5 <- head(sort(wordcount5, decreasing = T), 100)
#wordcount <- data.frame(worrdcount)
#wordcount <- wordcount[!(wordcoun4$Freq < 3), ]



wordcloud2(data = wordcount5,
           size = 0.7,
           fontFamily = "Tmon몬소리",
           color = pal)


#6
noun<- unlist(survey_6)
wordcount6 <- table(noun)
head(sort(wordcount6,decreasing=TRUE),100)

noun <- gsub("강의","",noun)
noun <- gsub("생각","",noun)
noun <- gsub("진행","",noun) 
noun <- gsub("수강","",noun) 
noun <- gsub("수업","",noun)
noun <- gsub("사용","",noun) 
noun <- gsub("시간","",noun)
noun <- gsub("상황","",noun) 
noun <- gsub("경우","",noun)
noun <- gsub("하지","",noun)
noun <- gsub("하시","",noun)
noun <- gsub("들이","",noun)
noun <- gsub("하기","",noun)
noun <- gsub("해서","",noun)
noun <- gsub("때문","",noun)
noun <- gsub("해주","",noun)
noun <- gsub("관련","",noun)
noun <- gsub("정하","",noun)
noun <- gsub("정해","",noun)
noun <- gsub("정한","",noun)
noun <- gsub("정도","",noun)
noun <- gsub("어디","",noun)
noun <- gsub("없습니","",noun)
noun <- gsub("만큼","",noun)
noun <- gsub("온라인","",noun)
noun <- gsub("교수님","",noun)
noun <- gsub("전반적","",noun) 
noun <- gsub("학생","",noun)
noun <- gsub("교수","",noun)
noun <- gsub("부분","",noun)
noun <- gsub("좋겠","",noun)
noun <- gsub("좋았","",noun)
noun <- gsub("좋겠습니","",noun)
noun <- gsub("동안","",noun)
noun <- gsub("습니","",noun)

wordcount6 <- table(noun)
head(sort(wordcount6,decreasing=TRUE),100)

noun <- noun[nchar(noun)>1]
wordcount6 <- table(noun)
head(sort(wordcount6,decreasing=TRUE),100)


wordcount6 <- head(sort(wordcount6, decreasing = T), 100)
#wordcount <- data.frame(worrdcount)
#wordcount <- wordcount[!(wordcoun4$Freq < 3), ]



wordcloud2(data = wordcount6,
           size = 0.7,
           fontFamily = "Tmon몬소리",
           color = pal)













