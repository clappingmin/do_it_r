table(is.na(mpg$hwy))
mpg %>%
  filter(!is.na(hwy)) %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))

outlier <-data.frame(gender = c(1,2,1,3,2,1),
                     score=c(5,4,3,4,2,6))

table(outlier)
table(outlier$sex)
table(outlier$score)

outlier$gender <- ifelse(outlier$gender ==3, NA, outlier$gender)
outlier

outlier$score <- ifelse(outlier$score ==6, NA, outlier$score)
table(outlier$gender)
table(outlier$score)
outlier

boxplot(exam)
boxplot(exam$math)

boxplot(mpg$hwy)

mpg <- ifelse(mpg$hwy <12 | mpg$hwy >37, NA, mpg$hwy)
mpg
table(mpg$hwy)
table(mpg)
table(is.na(mpg$hwy))

mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy, na.rm = T))
table(mpg)

table(mpg$drv)

mpg <- as.data.frame(ggplot2::mpg)
table(mpg$drv)
mpg$drv <- ifelse(mpg$drv %in% c("4","f","r"), mpg$drv, NA) #매칭되는지 확인
table(mpg$drv)

boxplot(mpg$cty)$status

mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)

boxplot(mpg$cty)

mpg %>%
  filter(!is.na(drv)&!is.na(cty)) %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(cty))
