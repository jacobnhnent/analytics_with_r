########################################
## 데이터 분할하기 
## test train validation
## cv (cross validation)
## 모형평가하기 ROC Curve 그리
########################################

getwd()
setwd("D://920.adp//data_files")
getwd()

########################################
## 필요한 패키지 설치 및 로드하기
########################################
##install.packages("reshape2")
##install.packages("data.table")
##install.packages("caret")
##install.packages("pshych")
##install.packages("detectCores")
##install.packages("doBy")
##install.packages("sqldf")
##install.packages("reshape2")
##install.packages("plyr")
##install.packages("ggplot2")

########################################

library(reshape2)
library(data.table)
library(caret)
library(psych)
#library(detectCores)
library(doBy)
library(sqldf)
library(reshape2)
library(plyr)
library(ggplot2)



########################################
## 파일의 encoding 확인
## 데이터 순서형으로 만들어 살펴보기 
iris
orderBy( ~ Sepal.Width, iris)

## %in%  
## colnames(df) <- c("c1","c2","c3","c4","c5")

head(iris)

## 해당하는 컬럼만을 포함하여 df 를 만들 수 있다
iris[,  names(iris) %in% c("Sepal.Width", "Species")]

## 해당하는 컬럼만을 제외하고 df 를 만들 수 있다
iris[, !names(iris) %in% c("Sepal.Width", "Species")]

#Excel 표 느낌으로 데이터를 볼 수 있다 5건만 데이터를 볼때 사용
View(iris[1:5, ])
View(iris)

#as.factor(iris$Species)
#
iris$Petal.Width <- as.factor(iris$Petal.Width)
str(iris)


########################################
## 데이터 나누기
## 샘플링 / Test / Train / Validation
## CV CrossFolder Validation
########################################





########################################
## 데이터읽어들이기 *파일입출력
########################################
getwd()
ls()
df901 <- read.csv("month_december.csv", header=TRUE, stringsAsFactors = FALSE, na.strings=c("NIL-UserDefined"))
test <- fread("month_december.csv", sep="\t", header=FALSE)
test <- fread("month_december.csv", sep=",", header=TRUE)
colnames(test)


# 모두 NA 이거나 특정컬럼값이 NA 인 컬럼은 제외하고 data.frame 을 만들 수 있는 방법은 없는가 ?
str(df901)
# header(컬럼명)이 없는 경우
## colnames(df) <- c("c1","c2","c3","c4","c5")

summary(df901)
is.na(df901)
head(df901)

# 행번호(row.names)를 제외하고 저장하면 불필요한 행번호를 저장하지 않는다 
write.csv(df901, file="df_write_file_testing.csv", row.names=FALSE)

# 다시 읽어들여 보기 테스팅
df902 <- read.csv("df_write_file_testing.csv", header=TRUE, stringsAsFactors = FALSE)
head(df902)


#NA 결측치의 처리
#NA 값이 하나라도 포함된 행들 자체를 제외
head(  na.omit(jacob_data)  ) 
head(  na.omit(jacob_data[, 8:12 ])  ) 

#NA 가 포함된 행을 제외

#객체 목록 보기
ls()
## rm(XXX) ##
obj_list=ls()
length(obj_list)

print(head( obj_list[3], 3))
print(head( obj_list[i], 3))

f_object_view_x <- function(x){
  for(i in 1:length(obj_list)){
  a <- obj_list[i]
  print(a)
  }
}


f_object_view_x()

colnames(jacob_data)
str(jacob_data)
quantile(jacob_data$DATE)

########################################
## 데이터 연결하기 추가 파생변수 연결하기(+ 컬럼)
########################################
## 만약 월별 데이터가 별도의 파일로 존재한다면
## 필요한 정보항목이 분리되어 있다면

# row bind 월별 데이터를 모아서 보고자 할 때
rbind

# 추가적인 정보 컬럼을 옆으로 붙여서 보고자 할 때 
cbind

## 파생변수 
## 자기그룹내에서의 순위 그리고 그룹 기초통계량과의 차이  (ex. over(partition by order by))
## 직전과 자신과의 차이   (ex. LEAD, LAG)



## mutate 
## transform 보다는 mutate 를 사용하여 계산한 컬럼을 추가
head(baseball)
baseball[id == "ansonca01",]

## cyear 이라는 변수를 추가로 생성함 ex.경력 (year-min(year))
head(ddply(baseball, .(id), transform, gap_year=year-min(year) + 1), 20)


############### Begins mutate    ###############

## mutate 는 여러컬럼을 추가할 수 있게 transform 에서 발전함 
head(ddply( baseball, .(id), mutate, gap_year=year-min(year)+1, log_cyear=log(gap_year) ), 50)

############### End of mutate    ###############


############### Begins Summarise ###############
## 변수를 이해하고 어떤 파생변수를 만들지에 대한 충분한 고민이 필요함
## 파생변수 생성은 단순 describe 로는 안되고 데이터를 보고 이해하고 창의적으로 만들어야 함

colnames(baseball)

head(ddply( baseball, .(id), summarise, 
            min_year = min(year), 
            max_year = max(year), 
            med_year = median(year), 
            total_years = max_year - min_year + 1, 
            unique_years = length(unique(year)),
            length_years = length(year),
            empty_years = total_years - unique_years,
            unique_teams = length(unique(team)))
     ,10)

#=======================================================
#group by 하는 변수를 여러개를 함께 넣는 방법을 모르겠음
#=======================================================
head(ddply( baseball, .(team), summarise, 
            min_year = min(year), 
            max_year = max(year), 
            med_year = median(year), 
            total_years = max_year - min_year + 1, 
            unique_years = length(unique(year)),
            length_years = length(year),
            empty_years = total_years - unique_years,
            unique_teams = length(unique(team)))
     ,10)


## ex. 각 선수별로 가장 많은 수의 게임을 플레이한 해의 기록
head(ddply(baseball, .(id), subset, g==max(g)))

desc(baseball)
############### End of Summarise ###############




############### Begins reshape2  ###############
## 



############### End of reshape2  ###############


?summarise

str(baseball)



########################################
## 제  목 : 기본환경확인
## 작성자 : 조현기
## 작성일 : 2017.04.22
##
##
##
##
########################################
Sys.setenv(R_ZIPCMD= "C:/Rtools/bin/zip") 

ls()
## rm(XXX) ##
## Memory Garbage Collection 
gc()          
## ====================================================
##          used  (Mb) gc trigger  (Mb) max used  (Mb)
## Ncells 2673273 142.8    4703850 251.3  3886542 207.6
## Vcells 5261913  40.2    9033501  69.0  9033501  69.0
## ====================================================
## 작업중인 R 객체를 저장하여 둘 수 있다

list=ls()
list

save(list=ls(), file="working_100.RData")
ls()
getwd()
rm(df901)
ls()

load("working_100.RData")

## 회사에서는 가능한 Full Name 을 쓰기로 했ㅇ
## 어떤 데이터를 통해 살펴보는 것을 어느 정도까지 표준화를 할 수도 있을 것 같음
## 아래와 같은 식을 쓴다고 하면 Naming 을 어느정도 고정한다면 변경을 최소화할 수 있음
## input dataframe  df_src 
## df_src$cat1  categorical
## df_src$dt    date
## df_src$dow   day of week
## df_src$num1  첫번째 숫자형
## df100 원천data frame (fact 테이블  code 테이블 날짜 테이블 고객테이블 주문테이블 상품목록테이블 주문상품테이블 ? )
## df200 
## df300 df110 df120
## df100$bjx100  기본변수중 첫번째 것을 이름
## featurePlot(iris[, 1:4], iris$Species, plot="density")

# 1초 sleep 을 3번 수행함
?system.time
system.time( for(i in 1:100000) mad(runif(1000)) ) # mad (Median Absolute Deviation)
system.time( lapply(1:3, function(xx){ Sys.sleep(1) }) )


# 내 PC의 core수에 맞게
?detectCores
install.packages("detectCores")
library(detectCores)
detectCores()
library(doParallel)
registerDoParallel(cores=detectCores())
foreach(i=1:3) %dopar% sqrt(i)


## 메모리 reset 하기 
## 필요한 데이터셋만 메모리에 남기고 지우
## rm()

# 엔터하시면 아래와 같이 파일을 선택할 수 있는 창이 뜹니다. 
# Data 를 읽어들일 때 Factorization 되지 않도록 일단 갖고 온다
file.choose()
jacob_data <-read.csv(file.choose(), stringsAsFactors = FALSE)
jacob_data <-read.csv("month_december.csv", stringsAsFactors = FALSE)


## NA 가 하나라도 포함되어 있는 행을 확인함 (complete 하지 않는)
jacob_data
jacob_data[ complete.cases(jacob_data),]
jacob_data[!complete.cases(jacob_data),]
describe(jacob_data)

unique(jacob_data$DATE)

class(jacob_data)
mtrx <- as.matrix(jacob_data)
class(mtrx)
describe(mtrx)
describeData(mtrx)

## 전체데이터의 describe 정보
describe(jacob_data)  ##describeData(jacob_data)

## jacob_data$CHANNEL 기준별로 describe 정보가 만들어 짐
## 변수 값의 개수만큼 각각 별도로 보여줌
describeBy(jacob_data, jacob_data$CHANNEL)


#############################################################
## 컬럼명 읽으면 전처리 (띄어쓰기 "_", 대문자 모두 소문자 시키기 Replace , Lowercase )

# featurePlot(jacob_data[, 8:9], jacob_data$MEDIA_TYPE)
# Error in stack.data.frame(x) : no vector columns were selected
# NA 값 확인이 필요함 
describe(jacob_data, na.rm = TRUE,  interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,type=3,check=TRUE)
describe(jacob_data, na.rm = FALSE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,type=3,check=TRUE)


#DATE,MON,WEEK,WK,CHANNEL,MEDIA TYPE,MEDIA, TTL_PV, Impression,Fill Rate(%),Click,CTR, CPC, eCPM, 광고 취급액, 수수료적용 광고 취급액, TCOST, RCOST,Cost Ratio,
#,,,,,,,,,1일 기준 전환비용,1일 기준 전환매출,1일 기준 ROAS,광고주별,파트너별,,,
#2016-07-10,JUL,SUN,WK28,PC,비언론사,티제이튠," 20,606,634 "," 5,719,580 ",27.76%,"10,070 ",0.18%, 176 , 310 ," 1,775,895 "," 1,598,306 "," 1,038,898 "," 1,038,898 ",58.50%,,,,,,,,,,"32,222.74",8,0.00%,보기,보기,,,
#가능하면 row명에서 공백을 "_" 로 치환하여 처리하기
#colnames 를 전처리하여 집어넣기 자동으로 (replace  처리)
#반복문 사용하여 만들 것

colnames(jacob_data)[6] <- 'MEDIA_TYPE'

plot(jacob_data$DATE)
plot(jacob_data$TTL_PV)
?describe

# 객체의 모든 변수에 낱건으로 NULL 이 저장되어 있는지 확인할 수 있음
is.na(jacob_data)

# 변수에 NULL 이 저장되어 있는지 확인
is.null(jacob_data$x1)





########################################
## 데이터 탐색 (요약 및 분산, 결측치 이상치) 대략을 파악한다 
########################################

str(mtcars)
NROW(mtcars)    # nrow 에 비해 NROW 는 벡터와 행렬에 모두 가능
summary(mtcars)
??describe

psych::describe(mtcars)
psych::describe(iris)


describe(mtcars, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,type=3,check=TRUE)
describeData(x,head=4,tail=4) 


# missing or outlier 데이터 살펴보기
# column names 확인하기 & 필요시 변경하기
colnames(mtcars)

###########################################
## 시각화 Visualization

## plot()
## pch  점의 종류
## cex  점의 크기
## col  색상
## type 그래프유형
## xlim=c(0,100)
## ylim=c(0,90)


# 한면에 여러 그림 그리기
multi_plot <- par(mfrow=c(1,2)) ## 1행 2열의 그래프 
par(multi_plot)



??mfrow
plot(mtcars)
## 모자이크 (변수의 종류수를 줄여야 확인 가능함)
mosaicplot(Titanic, color=TRUE)
mosaicplot(mtcars, color=TRUE)
mosaicplot(mtcars[, 1:2], color=TRUE)


mtcars[, 1:3]            # 1:3 의 컬럼만 사용
mtcars[, c('mpg', 'cyl')]
plot(mtcars[, 1:3])

plot(mtcars$mpg, mtcars$cyl, xlab="x축", ylab="y축", main="그래프 제목 : 차 성능" )

str(mtcars)
plot(mtcars$names)
plot(mtcars)

# 상호작용 교차표 그리기
plot(mtcars$mpg)
plot(mtcars$cyl)
plot(mtcars$disp)
plot(mtcars$hp)
plot(mtcars$wt)

#교호작용 및 잔차분석
??interplot

# row name 변경하기
row.names(mtcars)

# 그룹핑하여 보기
colnames(iris)
plot(iris)
levels(iris$Species)
## group by 해서 보기
plot(iris$Species) # 그룹별로 갯수를 맞춤 히스토그램
plot(Species ~ Sepal.Length, data=iris)

##------------------------------------------------------------
with(iris,{
  plot(Sepal.Length, Sepal.Width, pch=as.numeric(Species))
  legend("topright", legend=levels(iris$Species), pch=1:3) })
##------------------------------------------------------------

## feature 와 분류간의 관련성 시각화 확
## 그래프의 종류  상자(box), 줄무늬(strip), 밀도(density), 산점도(pairs), 타원(ellipse)
caret::featurePlot(iris[, 1:4], iris$Species, "ellipse")

featurePlot(iris[, 1:4], iris$Species)
featurePlot(iris[, 1:4], iris$Species, plot="box")
featurePlot(iris[, 1:4], iris$Species, plot="density")
featurePlot(iris[, 1:4], iris$Species, plot="strip")
featurePlot(iris[, 1:4], iris$Species, plot="pairs")
featurePlot(iris[, 1:4], iris$Species, plot="ellipse") # error




row.names(iris)
########################################
## 데이터전처리하기
########################################



########################################
##  data.table dcast 교차표 
########################################

##교차표(또는 분할표)의 빈도나 백분율이 아닌 교차표에 양적 자료의 합계(또는 평균)를 구하고 싶으며,
##교차표 형태로 출력하고 싶을 때에는 data.table 패키지에 있는 dcast() 함수를 이용한다.


# data.frame을 data.table로 변환
diamonds.dt = as.data.table(diamonds)

# 색인추가 setkey()
setkey(diamonds.dt, cut)
setkey(diamonds.dt, price)

## 먼저 명시한 것을 먼저 보여줌
diamonds.dt[J("Good","Premium")]
## price 가 326 이상인 값만 보여줌
diamonds.dt[price >= 326,]


# R에서 일반적으로 제공하는 교차표
diamonds.dt[ , .(price.sum=sum(price)), by=c("cut")]
diamonds.dt[ , .(price.sum=sum(price)), by=c("cut", "color")]


#  dcast() 결과 (vertical::cut X  horizontal::color )
dcast(diamonds.dt, cut ~ color, fun=sum,                  value.var="price")
dcast(diamonds.dt, cut ~ color, fun=mean,                 value.var="price")

## 하나의 행에 sum, mean, var 가 오른쪽으로 계속 붙어 확장됨
dcast(diamonds.dt, cut ~ color, fun=list(sum, mean, var), value.var="price")  

##  세로는 색상종류별  그리고 가로는 품질별(Fair ~  Very Good ~ Premium ~ Ideal)
dcast(diamonds.dt, color ~ cut, fun=sum, value.var="price")

##______________________________________________________
## color    Fair    Good Very Good  Premium    Ideal
## 1:     D  699443 2254363   5250817  5820962  7450854
## 2:     E  824838 3194260   7715165  8270443 10138238
## 3:     F 1194025 3177637   8177367 10081319 12912518
## 4:     G 1331126 3591553   8903461 13160170 18171930
## 5:     H 1556112 3001931   8272552 12311428 12115278
## 6:     I  819953 2650994   6328079  8491146  9317974
## 7:     J  592103 1404271   3460182  5086030  4406695
##______________________________________________________



########################################
##   
########################################


mtcars[1:5, c(3,5)]
mtcars[mtcars$mpg > 25 & mtcars$cyl == 4, ]

## tapply
mean(mtcars$mpg)
unique(mtcars$am)
## am 별 mpg 의 mean 을 구함
tapply(mtcars$mpg, mtcars$am, mean)
## cyl 별 mpg 의 mean 을 구함
tapply(mtcars$mpg, mtcars$cyl, mean)

## aggregate
## select am, cyl, mean(mpg) 
##   from mtcars group by am, cyl
aggregate(mpg~am+cyl, data=mtcars, mean)

## table (각각 x, y 축을 만들어 줌)
## select am, cyl, count(*) 
##   from mtcars group by am, cyl
table(mtcars$am, mtcars$cyl)
table(transmission=mtcars$am, cylinder=mtcars$cyl)

## ------------------------
##                 cylinder
##                  4  6  8
## transmission  0  3  4 12
##               1  8  3  2
## ------------------------


sapply(dt.crimes,function(x) length(unique(x)))

url <- "http://steviep42.bitbucket.org/YOUTUBE.DIR/chi_crimes.csv"
download.file(url,"chi_crimes.csv")
system.time(df.crimes <- read.csv("chi_crimes.csv", header=TRUE,sep=","))

## -- data.table --
library(data.table)
dt <- data.table(mtcars)
class(dt)
dt[ ,mean(mpg)]
## 
## If we want to more clearly label the computed average
dt[mpg>20, .(avg=mean(mpg)), by=.(am,cyl)]

######################################################
## Data Ordering
## 정렬 ascending and descending with 20 rows
## sort()와 order()를 설명한다
## sort()  는 주어진 데이터를 직접 정렬해 주는 함수이며, 
## (데이터셋 자체의 순서가 변함 <- 한번 정리해서 쓰기에 효과적일 듯 함)
## order() 는 데이터를 정렬했을 때의 순서를 반환한다.

dt[order(cyl, -mpg)][1:20]
## 값의 종류수 count(distinct(x))
sapply(dt, function(x) length(unique(x)))
sapply(dt, function(x) mean(x))

describe(dt)

## SQL 아래와 동일한 결과
## SELECT * FROM mtcars ORDER BY cyl, mpg, wt
orderBy( ~ cyl + mpg + wt, mtcars)


## order 는 정렬한 순서를 반환한다
## 이를 이용해 데이터프레임 자체를 정렬할 수 있다 
iris[order(iris$Sepal.Length, iris$Petal.Length), ]


## 조건에 맞는 데이터 찾기 subset
dt_subset_cyl_4 <- subset(dt, cyl == 4 )


## Sampling 
## 
str(dt)

## 각 cyl 별로 10% 의 데이터를 추출하는 방식 
##
sampleBy( ~ cyl, frac = 0.1, replace=FALSE, data = mtcars)


## data
head(french_fries)
str(french_fries)
describe(french_fries)
## 두개이상의 변수값의 그룹의 데이터별로 describe


describeBy(french_fries, list(french_fries$time, french_fries$subject), mat=TRUE)
statsBy(french_fries, c("time", "treatment"))


head(describeBy(french_fries, list(french_fries$time, french_fries$subject), mat=TRUE))

## 매우 상세하게 그룹핑된 결과를 순차적으로 만들어서 보기 쉽게 하려면
## sort, order 의 적용을 검토해 볼 
(describeBy(french_fries, list(french_fries$time, french_fries$subject), mat=TRUE))

eda_ff <- describeBy(french_fries, list(french_fries$time, french_fries$subject), mat=TRUE)
str(eda_ff)
dt_ff <- data.table(eda_ff)
dt_ff[order(-mean)]

################################################ ################################################
## melt ## ()
## "time""treatment""subject""rep" ::: 종류( "potato""buttery"grassy""rancid""painty")
## 컬럼을 행으로 바꿔서 보고 싶을 때 사용 (종류가 variable 컬럼값으로 치환되면 행이 5X 됨)
################################################ ################################################

df_melt <- melt(french_fries, id.vars=1:4, na.rm=FALSE)
head(french_fries)
colnames(french_fries)
head(df_melt)

ddply(df_melt, .(variable), summarise, mean=mean(value, na.rm=TRUE))

## 굳이 ddply 를 복합컬럼으로 나눠서 보지 않는다면 모든 컬럼의 통계값을 describe 로 보는 것이 효과적임
describe(french_fries)

## NA 가 하나라도 포함되어 있는 행을 확인함 (complete 하지 않는)
french_fries
french_fries[ complete.cases(french_fries),]
french_fries[!complete.cases(french_fries),]








