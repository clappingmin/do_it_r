rm(list = ls()) #console창 제거는 ctrl+l

exam<- read.csv("csv_exam.csv")
class(exam)

# exam[] : 전체 가져오기 / data.frame이라서 행과 열이 있음
exam[1,] # 1행 추출

exam[exam$class == 1,]
exam[exam$class == 1 & exam$math >=50,] # 1반이면서 수학 점수가 50점 이상
exam[, "math"] # math 변수 출력력
exam[, c("class", "math", "english")]

var1 <- c(1,2.3,1,2) #연속 변수 생성
var2 <- factor(c(1,2,3,1,2))  #범주 변수 생성

var1
var2

var1+2 #연속 변수로 연산
var2+2 #범주 변수로 연산, 에러 뜬다!
