##################################################
## 지정 디렉토리에 있는 모든 txt 파일 읽어들이기                    
##################################################
#working directory 
#text 파일들이 들어있는 폴더를 지정합니다

###############  BEGIN  ###############
folder <-"D:\\700.Jacob\\onlytexts"

#all files in the directory
filelist <- list.files(folder)
length(filelist)

#empty list
data<-vector("list", length(filelist))
(names(data)<-filelist)


#경로테스트
paste(folder, filelist[1], sep="\\" )

# Error 발생시 for 문에 추가하여 확인시 사용
# writeLines(paste(i, filelist[i]," checking checking !!", sep=" "))

for (i in 1:length(filelist)) {
  data[[i]] <- readLines(paste(folder, filelist[i], sep="\\"))
}

class(data)
#데이터가 정상적으로 들어왔는지 확인해 본다
#파일 하나만 찍어본다, 그리고 나머지를 다 찍어본다
data[1]
data
str(data)

###############  END  ###############


