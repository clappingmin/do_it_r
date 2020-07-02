rm(list=ls())
install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare <-read.spss(file="Koweps_hpc10_2015_beta1.sav", to.data.frame = T)
head(raw_welfare)

welfare <- raw_welfare
dim(welfare)  #몇행, 몇열
str(welfare)  #구조 확인

welfare <- rename(welfare, 
                  gender =h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7
                  )

welfare %>% select(gender,birth,marriage,religion,income,code_job,code_region)%>%head

class(welfare$gender)
class(welfare$income)

table(welfare$gender)

welfare[c(1000,3000),"gender"]<-9

welfare$gender <- ifelse(welfare$gender ==9, NA, welfare$gender)

table(is.na(welfare$gender))

welfare$gender <- ifelse(welfare$gender == 1, "male", "female")

table (welfare$gender)

qplot(welfare$gender)

summary(welfare$income)

qplot(welfare$income)+xlim(0,1000)

welfare$income <- ifelse(welfare$income %in% c(0,9999),NA,welfare$income)

table(is.na(welfare$income))

gender_income <- welfare %>%
  filter(!is.na(income) & !is.na(gender))%>%
  group_by(gender)%>%
  summarise(mean_income = mean(income))

gender_income

ggplot(data = gender_income, aes(x=gender, y=mean_income))+geom_col()


class(welfare$birth)
summary(welfare$birth)

qplot(welfare$birth)

table(is.na(welfare$birth))

welfare$age <- 2015-welfare$birth + 1
summary(welfare$age)

qplot(welfare$age)

age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))

head(age_income)

age_income

ggplot(data = age_income, aes(x=age, y=mean_income)) +geom_line()

welfare <- welfare %>%
  mutate(ageg = ifelse(age<30, "young",
                       ifelse(age<59,"middle","old")))

table(welfare$ageg)
table(is.na(welfare$ageg))

ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income = mean(income))

ggplot(data = ageg_income, aes(x=ageg, y=mean_income))+geom_col()+ scale_x_discrete(limits=c("young","middle","old"))

welfare <- welfare %>%
  mutate(ageg2 = ifelse(age<20, "10대", 
                        ifelse(age<30,"20대",
                               ifelse(age<40,"30대",
                                      ifelse(age<50,"40대",
                                             ifelse(age<60,"50대",
                                                    ifelse(age<70,"60대","70대이상")))))))
table(welfare$ageg2)

ageg2_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg2)%>%
  summarise(mean_income=mean(income))

ageg2_income

ggplot(data = ageg2_income, aes(x=ageg2, y=mean_income))+geom_col()
R.version

gender_income2 <-welfare %>%
  filter(!is.na(income) &!is.na(gender)) %>%
  group_by(ageg,gender) %>%
  summarise(mean_income = mean(income))

gender_income2

ggplot(data=gender_income2, aes(x=ageg,y=mean_income,fill = gender))+
  geom_col(position = "dodge")+
  scale_x_discrete(limits = c("young","middle","old"))

gender_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age,gender)%>%
  summarise(mean_income = mean(income))
gender_age

ggplot(data = gender_age, aes(x= age, y = mean_income, col = gender))+geom_line()

class(welfare$code_job)
table(welfare$code_job)

library(readxl) #에러날 경우 install.packages("readxl")
list_job <- read_excel("Koweps_codebook.xlsx", col_names = T, sheet = 2)
head(list_job)
class(list_job)
dim(list_job)

welfare <- left_join(welfare, list_job, id = "code_job")

welfare %>%
  filter(!is.na(code_job)) %>%
  select(code_job,job) %>% 
  head(10)

job_income <- welfare %>% 
  filter(!is.na(income)&!is.na(job)) %>% 
  group_by(job) %>%   #같은 직업별로 묶을꺼
  summarise(mean_income = mean(income)) #같은 직업별로 평균계산

head(job_income)

top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% #큰값부터 정렬 
  head(10)
top10

ggplot(data = top10, aes(x=reorder(job,mean_income),y=mean_income))+
  geom_col()+
  coord_flip() #오른쪽으로 90도 회전

bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)
bottom10

ggplot(data = bottom10, aes(x=reorder(job,mean_income), y= mean_income))+
  geom_col()+
  coord_flip()+
  ylim(0,850) #y축 범위 지정

class(welfare$code_region)

table(welfare$code_region)
table(is.na(welfare$code_region))

list_region <- data.frame(code_region = c(1:7),  #엑셀파일로 저장안되어 있어서 직접 넣는다
                          region =c("서울",
                                    "수도권(인천/경기)",
                                    "부산/경남/울산",
                                    "대구/경북",
                                    "대전/충남",
                                    "강원/충북",
                                    "광주/전남/전북/제주도"))

list_region

welfare <- left_join(welfare,list_region, id= "code_region") #왼쪽에 있는게 다 들어오고 오른쪽은 데이터가 없을경우 안들어온다

welfare %>% 
  select(code_region, region) %>% 
  tail

region_ageg <- welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n=n()) %>%  #건수 세는거
  mutate(pct= round(n/sum(n)*100,2)) #새로운 변수 추가

region_ageg

ggplot(data = region_ageg, aes(x=region, y = pct,fill= ageg))+geom_col()+coord_flip()

list_order_old <- region_ageg %>% 
  filter(ageg=="old") %>% 
  arrange(pct)  #비율 오름차순으로

list_order_old

order <- list_order_old$region #지역명 순서 변수 만들기

ggplot(data = region_ageg, aes(x=region, y = pct,fill= ageg))+geom_col()+coord_flip()+
  scale_x_discrete(limits=order)

class(region_ageg$ageg)

levels(region_ageg$ageg)

region_ageg$ageg <- factor(region_ageg$ageg,
                           level = c("old","middle","young"))

class(region_ageg$ageg)

levels(region_ageg$ageg)

ggplot(data = region_ageg, aes(x=region, y= pct, fill=ageg))+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limits = order)
