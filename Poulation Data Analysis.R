#####인구수 추정

#데이터 전처리작업
setwd("C:/workspace/portfolio/Predict")
.libPaths("C:/workspace/portfolio/Predict") #해당 디렉토리와 패스는 코드 다운로딩 받은 폴더로 수정 필요함.
install.packages("dplyr")
library(dplyr)
worldbankData <- read.csv("worldbank1.csv",header = T)
worldbankSeries <- read.csv("worldbank2.csv",header= T)
str(worldbankData)
COUNTRYCODE <- unique(worldbankData$Country.Code)
COUNTRYNAME <- unique(worldbankData$癤풠ountry.Name)

########################################
#264국 2019년 기준 총 인구수 정리
########################################
COUNTRY <- c()
TOTAL_POPULATION = c()
for(i in 1:265){
  dataSource = worldbankData %>% filter(Country.Code == COUNTRYCODE[i])
  populationData = dataSource %>%  filter(Series.Name%in%c("Population ages 0-14, total",
                                          "Population ages 15-64, total",
                                          "Population ages 65 and above, total")) 
  NA.CHECKER = which(populationData$X2019..YR2019.=="..") #조건식의 결과가 참이면 정수형이 반환되고, 거짓이면 0이 반환=".."면 NA임
  if(sum(NA.CHECKER)==6){TOTAL = NA}else{TOTAL = sum(as.numeric(populationData$X2019..YR2019))}#NA체커값이 6 = 인구수 X란뜻
  COUNTRY = c(COUNTRY,COUNTRYNAME[i])
  TOTAL_POPULATION = c(TOTAL_POPULATION,TOTAL)}
popResult <- data.frame(COUNTRY,TOTAL_POPULATION)
popResult

#알파벳 기준 상위 20개국 결과값 확인
print(head(popResult,20))

#########################################################
#국가별 가장 큰 도시에 사는 인구수 정리(2019 기준)
#########################################################
worldbankData <- read.csv("worldbank1.csv",header = T, na.strings=c("..", "NA","")) 
#불러올 때 결측값 NA로 표기

#Population in largest city의 2019년 기준 데이터 추출
worldbankData %>% 
       filter(Series.Name == "Population in largest city") %>% 
       select(Country.Code, Series.Name, X2019..YR2019.)

######################################################################
#국가별 총 인구수 대비 가장 큰 도시에 사는 인구의 비율
######################################################################
COUNTRY <- c()
CITY_RATIO <- c()
for(i in 1:265){
  dataSource = worldbankData %>% filter(Country.Code == COUNTRYCODE[i])
  ratioLcity = dataSource %>%  filter(Series.Name == "Population in the largest city (% of urban population)") 
  NA.CHECKER = which(ratioLcity$X2019..YR2019.=="..") #조건식의 결과가 참이면 정수형이 반환되고, 거짓이면 0이 반환=".."면 NA임
  if(length(NA.CHECKER)==1){RATIO = NA}else{RATIO = ratioLcity$X2019..YR2019.}#NA체커값이 1 = 데이터가 NA
  COUNTRY = c(COUNTRY,COUNTRYNAME[i])
  CITY_RATIO = c(CITY_RATIO,RATIO)
}
COUNTRY <- COUNTRY[-265]
ratioResult <- data.frame(COUNTRY,CITY_RATIO)
ratioResult

#####################################################################################
#총 인구수 대비 국가에서 가장 큰 도시에 사는 인구의 비율이 가장 높은 10개국
#####################################################################################
COUNTRY <- c() #나라
CITY_RATIO <- c() #비율
for(i in 1:265){
  dataSource = worldbankData %>% filter(Country.Code == COUNTRYCODE[i])
  ratioLcity = dataSource %>%  filter(Series.Name == "Population in the largest city (% of urban population)") 
  NA.CHECKER = which(ratioLcity$X2019..YR2019.=="..") #조건식의 결과가 참이면 정수형이 반환되고, 거짓이면 0이 반환=".."면 NA임
  if(length(NA.CHECKER)==1){RATIO = NA}else{RATIO = ratioLcity$X2019..YR2019.}#NA체커값이 1 = 데이터가 NA
  COUNTRY = c(COUNTRY,COUNTRYNAME[i])
  CITY_RATIO = c(CITY_RATIO,RATIO)
}
COUNTRY <- COUNTRY[-265]
ratioResult <- data.frame(COUNTRY,CITY_RATIO)
ratioResult1 = na.omit(ratioResult) %>% arrange(desc(as.numeric(CITY_RATIO))) %>% slice(1:10)
ratioResult1

################################################################################
#총 인구수 대비 국가에서 가장 큰 도시에서 사는 비율이 가장 낮은 10개국
################################################################################
COUNTRY <- c()
CITY_RATIO <- c()
for(i in 1:265){
  dataSource = worldbankData %>% filter(Country.Code == COUNTRYCODE[i])
  ratioLcity = dataSource %>%  filter(Series.Name == "Population in the largest city (% of urban population)") 
  NA.CHECKER = which(ratioLcity$X2019..YR2019.=="..") #조건식의 결과가 참이면 정수형이 반환되고, 거짓이면 0이 반환=".."면 NA임
  if(length(NA.CHECKER)==1){RATIO = NA}else{RATIO = ratioLcity$X2019..YR2019.}#NA체커값이 1 = 데이터가 NA
  COUNTRY = c(COUNTRY,COUNTRYNAME[i])
  CITY_RATIO = c(CITY_RATIO,RATIO)
}
COUNTRY <- COUNTRY[-265]
ratioResult <- data.frame(COUNTRY,CITY_RATIO)
ratioResult
ratioResult2 = na.omit(ratioResult) %>% arrange(as.numeric(CITY_RATIO)) %>% slice(1:10)
ratioResult2
str(ratioResult)

#########################################################
#데이터가 누락된 국가의 리스트 확인
#########################################################
Missingvalue <- table(is.na(ratioResult$CITY_RATIO)) #결측값 개수 확인
Missingvalue

ratioResult2 <- ratioResult %>% filter(is.na(CITY_RATIO)) #결측값이 있는 나라 추출
View(ratioResult2)
