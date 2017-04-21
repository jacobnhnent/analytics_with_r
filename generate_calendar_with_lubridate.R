#############################################################################
# GENRATING CALENDAR FOR TIME SERIES ANALYSIS
# Created Date : 2015.01.18
# Version.0.1
# Designed by Jacob (cafe.naver.com/dategeeks / calebpro@gmail.com)
# Fieilds : "date_char" "jdate" "m12" "q4" "s4" "w7" 
#           "workingday" "whattodo" "d31" "d365" 
#############################################################################

library(sqldf, lubridate)

# number of days for 30 years (considered leap month for every 4 years i.e.2012.02.29 )
# 30년 만큼의 일수생성 (4년마다 윤달 366일  i.e.2012.02.29)
(days <- (30*365.2564)-1) 

head(num_table <- as.data.frame(c(0:days))); colnames(num_table) <- "num";
str(num_table);head(num_table, 3);tail(num_table, 3);
jcalendar <- sqldf("select strftime('%Y-%m-%d %H:%M:%S', 
                   julianday('1990-01-01') + num) as date_char  from num_table")
jcalendar$jdate <- as.Date(jcalendar$date_char, format="%Y-%m-%d")
str(jcalendar);head(jcalendar, 3);tail(jcalendar, 3);

#-------------------------------------------------------------
# Checking if the all years are generated and each years has its days as you wanted.
(sqldf("select substr(date_char,1,4) as  yyyy, count(*) 
       from jcalendar 
       group by substr(date_char,1,4) order by substr(date_char,1,4)"))

jcalendar$jPOSIXlt <- strptime(jcalendar$date_char, "%Y-%m-%d %H:%M:%S")
head(jcalendar)

###################################################
# 요일(주말여부), 시간대, 해당월을 산출할 수 있음
###################################################
str(jcalendar)
unclassdate <- unclass(jcalendar$jPOSIXlt )
str(unclassdate)
m12  <- data.frame(m12  = unclassdate$mon  +1)   # starting with zero 0
q4 <- data.frame(q4 = ifelse(m12$m12<=3, '1q', ifelse(m12$m12<=6, '2q', ifelse(m12$m12<=9, '3q', '4q'))))
s4 <- data.frame(s4 = ifelse(m12$m12>=3&m12$m12<=5 , 'spring', 
                             ifelse(m12$m12>=6&m12$m12<=8 , 'summer', 
                                    ifelse(m12$m12>=9&m12$m12<=11, 'autumn', 'winter' ))))
w7   <- data.frame(w7   = unclassdate$wday   )   # starting with zero 0 (Sunday==0)
workingday <-data.frame(workingday = ifelse(w7$w7==0 | w7$w7==6, 'N', 'Y')) #weekday=0, weekend=1        
d31  <- data.frame(d31  = unclassdate$mday   )   # starting with 1
d365 <- data.frame(d365 = unclassdate$yday +1)   # starting with zero 0
colnames(jcalendar)

#-------------------------------------------------------------
whattodo <-data.frame(whattodo = ifelse(workingday=='N', 'go camping', '')) 
colnames(whattodo) <- "whatdodo"
jcalendar <- cbind(jcalendar[,1:2], m12, q4, s4, w7, workingday, whattodo=whattodo, d31, d365)
jcalendar[substr(jcalendar$date_char,1,10)>='2014-12-20' & substr(jcalendar$date_char,1,7)<='2015-02-20',]
#-------------------------------------------------------------

#update local non-working day such as Lunar new year and Thanks giving holiday
jcalendar[jcalendar$jdate =='2015-02-18', "workingday"] <- "N"
jcalendar[jcalendar$jdate =='2015-02-19', "workingday"] <- "N"
jcalendar[jcalendar$jdate =='2015-02-20', "workingday"] <- "N"

jcalendar[jcalendar$jdate =='2015-02-18', ] #checking the change
jcalendar[jcalendar$jdate =='2015-02-19', ] #checking the change
jcalendar[jcalendar$jdate >='2015-02-01' & jcalendar$jdate <'2015-03-01', ] #checking the change
#-------------------------------------------------------------
whattodo <-data.frame(whattodo = ifelse(workingday=='N', 'go camping', '')) 
jcalendar <- cbind(jcalendar[,1:2], m12, q4, s4, w7, workingday, whattodo, d31, d365)
jcalendar[substr(jcalendar$date_char,1,10)>='2014-12-20' & substr(jcalendar$date_char,1,7)<='2015-02-20',]
#-------------------------------------------------------------
# 1990 ~ 2019  calendar 
head(jcalendar); tail(jcalendar);
colnames(jcalendar)
#############################################################################