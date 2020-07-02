R.version
Sys.getenv("JAVA_HOME")

#패키지 설치
install.packages("rJava")
install.packages("memoise")
install.packages("KoNLP")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("wordcloud")

koinstall = c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools','vctrs')
install.packages(koinstall)
##-------------------------------------패키지 설치 다시하면 안된다------------------------------------------------##

#패키지 로드
library(rJava)
library(KoNLP)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

pal <- brewer.pal(8,"Dark2")

useNIADic()
useSejongDic()
##---------------------------------------------------------------------------------------------------------------##

extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")

txt <- readLines("hiphop.txt")
head(txt)

nouns <- extractNoun(txt)

class(nouns)

#추출한 명사 list를 문자열 벡터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))
wordcount

#데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringsAsFactors = F) #f : false
head(df_word)

#변수명 수정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

#두 글자 이상 단어 추출
df_word <- filter(df_word,nchar(word)>=2)

top_20 <-df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)

top_20

ggplot(data = top_20,aes(x=word,y=freq))+geom_col()

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4,0.3),
          colors = pal)

pal <- brewer.pal(9,"Blues")[5:9]
set.seed(1234)

rm(list = ls())
twitter <- read.csv("twitter.csv",
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = "UTF-8"
                    )

#변수명 수정
twitter <- rename(twitter,
                  no = 번호,
                  id= 계정이름,
                  date = 작성일,
                  tw = 내용)

#특수문자 제거
twitter$tw <- str_replace_all(twitter$tw, "\\W"," ")

#트윗에서 명사추출
nouns <- extractNoun(twitter$tw)
class(nouns)

#추출한 명사 list를 문자열 백터로 변환, 단어별 빈도표 생성
wordcount <- table(unlist(nouns))
class(wordcount) #table

#데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word

#변수명 수정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)
df_word

#두 글자 이상 단어만 추출
df_word <- filter(df_word, nchar(word)>=2)

#상위 20개 추출
top20 <- df_word %>% 
  arrange(desc(freq)) %>% #빈도수 높은순대로 정렬 
  head(20)  #상위 20개만
top20

order <- arrange(top20, freq)$word # 빈도 순서 변수 생성 word값을 order에 넣어준다.
order

ggplot(data = top20, aes(x= word, y= freq))+
  ylim(0,2500)+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limit = order)+  #빈도수 막대 정렬
  geom_text(aes(label = freq), hjust = -0.3)  #빈도 표시


display.brewer.all()
pal <- brewer.pal(8,"Dark2")

wordcloud(words = df_word$word,   #단어
          freq = df_word$freq,    #빈도
          min.freq = 10,          #최소 단어 빈도
          max.words = 200,        #표현 단어 수
          random.order = F,       #고빈도 단어 중앙 배치
          rot.per = .1,           #회전 단어 비율
          scale = c(6,0.2),       #단어 크기 범위
          colors = pal)           #색상 목록
#빈도수 차이가 너무 커서 '북한'만 크게 나온다.
##---------------------------------------------------------------------------------------------------------------##
rm(list = ls()) #기존 데이터 삭제

install.packages("RCurl")
install.packages("rvest")
install.packages("stringr")
install.packages("XML")
library(RCurl)
library(rvest)
library(stringr)
library(XML)
library(dplyr)

#제목, 작성날짜, 주소 저장할 부분
title <- c()
day <- c()
url <- c()
class(url)

query <- curlEscape("아이즈원")


for(i in 0:99){ #100번 반복
  url[i+1]<-paste0("https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=",
                   query,"&sm=tab_pge&srchby=all&st=sim&where=post&start=",as.character(10*i+1));
  lotus1 <- read_html(url[i+1]);
  title1 <- lotus1 %>%  #제목 가지고 옴
    html_nodes('._sp_each_title') %>% # .:class 
    html_text;
  title <- c(title, title1); #타이틀 누적 #여기까지 타이틀 가져옴
  day1 <- str_split(lotus1 %>% #공백을 기준으로 자름
                      html_nodes('.txt_inline') %>% 
                      html_text,' ')
  temp <-c()
  for(j in 1:10){
    temp[j] <- day1[[j]][1]
  }
  day <- c(day,temp);
} #title부터 day가 나옴

csv1 <- data.frame(title,day)
head(csv1)
write.csv(csv1,"web crawling.csv");

#--------------------------------------------------------------------------------------------------
#csv1을 이용하여 간단한 워드 클라우드 제작해보기

library(KoNLP)
useNIADic()

#read.csv(file.choose(),header=T)
webc <- csv1
class(webc$title)
title1 <- as.character(webc$title)

title1 <- str_replace_all(title1, "\\W", " ")
head(title1)
nouns <- extractNoun(title1) #명사 추출 리턴값 : 리스트
nouns <- unlist(nouns)       #리스트를 일반형태로

nouns <-gsub("아이즈원","",nouns) #특정 단어 제거
nouns <- gsub("아이즈"," ",nouns)
nouns <- gsub("IZ"," ",nouns)
nouns <- gsub("ONE"," ",nouns)

wordcount <- table(nouns)
wordcount

#데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
head(df_word)

#변수명 수정
df_word <- rename(df_word,
                  word = nouns,
                  freq = Freq)

#단어수 하나는 제거
df_word <- filter(df_word, nchar(word)>=2)

pal <- brewer.pal(8,"Dark2") #색상추출
set.seed(1234)
windowsFonts(font=windowsFont("맑은 고딕")) #폰트 설정

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(3,0.6),
          colors = pal,
          family = "font")
#------------------------------------------------------------------------------------------------------------------
#----------------------------영화 리뷰 추출------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
rm(list = ls()) #기존 데이터 삭제
review <- c()
url <- c()
content <- c()
class(url)


for(i in 0: 20){
  url[i+1]<-paste0("https://movie.daum.net/moviedb/grade?movieId=135891&type=netizen&page=",as.character(i+1));
     htxt <- read_html(url[i+1]);
     review1 <- html_nodes(htxt, '.review_info')
     content1 <- html_nodes(htxt, '.desc_review')
     review1 <- html_text(content1)
     if(length(review1)==0){break}
     review <- c(review, review1)
     print(i)
   }


write.table(review, 'daum.txt')

#--------------------------------------------------------------------------------------------------
#all.reviews을 이용하여 간단한 워드 클라우드 제작해보기

library(KoNLP)
useNIADic()

#read.csv(file.choose(),header=T)
webs2 <- review
class(webs2)
#title1 <- as.character(webc$title)

txt<-readLines("daum.txt")
txt <- str_replace_all(txt, "\\W", " ")
head(txt)
nouns <- extractNoun(webs2) #명사 추출 리턴값 : 리스트
nouns <- unlist(nouns)       #리스트를 일반형태로

wordcount <- table(nouns)
wordcount

#데이터 프레임으로 변환
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
head(df_word)

#변수명 수정
df_word <- rename(df_word,
                  word = nouns,
                  freq = Freq)

#단어수 하나는 제거
df_word <- filter(df_word, nchar(word)>=2)

pal <- brewer.pal(8,"Dark2") #색상추출
set.seed(1234)
windowsFonts(font=windowsFont("맑은 고딕")) #폰트 설정

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(3,0.6),
          colors = pal,
          family = "font")

