install.packages("ggiraphExtra")
install.packages("maps")
install.packages("mapproj")
library(mapproj)
library(ggiraphExtra)

str(USArrests)
head(USArrests) 
library(tibble) #테이블보다는 기능이 좋아진 패키지

crime <- rownames_to_column(USArrests, var="state")
crime$state <- tolower(crime$state)

head(crime)
library(ggplot2)
states_map <-map_data("state")
str(states_map)

head(states_map) #위도 경도값 들어가 있음

ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id = state),
             map = states_map,
             interactive = T)

#-----------------------------------------11-2---------------------------------------------------------------------------------------------#
install.packages("stringi")
install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")

library(kormaps2014)
korpop1 #한글이 다 깨짐을 확인

str(changeCode(korpop1)) #한글깨짐 고침

library(dplyr)
korpop1 <- rename(korpop1,
                  pop = "총인구_명",
                  name = "행정구역별_읍면동") #한글이 변수명이면 문제가 생길수도 있으니 이름변경

korpop1$name <- iconv(korpop1$name, "UTF-8","CP949")
korpop1

str(changeCode(korpop1))

ggChoropleth(data = korpop1,
             aes(fill = pop,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)

str(changeCode(tbc))
tbc$name <- iconv(tbc$name,"UTF-8", "CP949")
ggChoropleth(data = tbc,
             aes(fill =NewPts,
                 map_id= code,
                 tooltip = name),
             map = kormap1,
             interactive = T)
