#행선택하기
boiler9_eng[c(1:50),]

#열선택하기
data.frame[[1]]<-boiler9_eng[[1]]
data.frame[[1]]
boiler9_eng[1]
boiler9_eng[c(1,2)]
boiler9_eng[["EFF"]]
boiler9_eng[c("EFF","NOX")]

#원하는 조건의 행/열 선택하기
subset(boiler9_eng,select=c(EFF,NOX))
subset(boiler9_eng,select=c(-EFF,-NOX))
subset(boiler9_eng,select="EFF",subset=(EFF>50))
subset(boiler9_eng,subset=(EFF>50))
g
#행끼리 묶기
data.frame_1<-head(boiler9_eng)
data.frame_2<-tail(boiler9_eng)
rbind(data.frame_1,data.frame_2)

#열끼리 묶기
data.frame_1<-head(boiler9_eng)[c(1:2)]
data.frame_2<-tail(boiler9_eng)[c(3:4)]
cbind(data.frame_1,data.frame_2)

#공통된 행KEY 값으로 열 병합하기
intersect(names(data.frame_1),names(data.frame_2))
merge(x=data.frame_1,y=data.frame_2,by="NAME")
merge(x=data.frame_1,y=data.frame_2,by=NULL)
merge(x=data.frame_1,y=data.frame_2,by ="NAME", all.x = TRUE)

#변수명을 알고싶을경우
names(boiler9_eng)

#변수이름 Set 만들기
model.var<-names(boiler9_eng)[c(1:2)]
model.var2<-c("EFF","NOX")

#데이터 오름차순/내림차순 정렬
data.frame<-data.frame_1[order(data.frame_1$EFF),]
data.frame<-data.frame_1[order(data.frame_1$EFF,data.frame_1$NOX),]
data.frame<-data.frame_1[order(-data.frame_1$EFF),]
data.frame<-data.frame_1[order(-data.frame_1$EFF,-data.frame_1$NOX),]

#NA값을 가진 행 삭제하기
new.data<-na.omit(data.frame)

#NA값 중앙값으로 대체하기(for 구문이용하면 모든 열 대체가능)
new.data<-replace(data.frame$EFF,which(is.na(data.frame$EFF)),median(data.frame$EFF,na.rm=TRUE))

#중복되는 모든 행 삭제
new.data<-data.frame[!duplicated(data.frame[,'EFF']),]

#Random Sampling
data.frame<-boiler9_eng[sample(1:nrow(boiler9_eng),10,replace=FALSE),]

#데이터 특정열 기준으로 분리하기
group<-split(boiler9_eng,data.frame$EFF)
group[[2]]
group[[2]]$EFF

#행과 열 합계 구하기
rowSums(boiler9_eng)
colSums(boiler9_eng)
tail(rbind(boiler9_eng, Total=colSums(boiler9_eng)))
head(cbind(boiler9_eng, Total=rowSums(boiler9_eng)))

#여러개의 데이터를 열들로 묶은 데이터프레임으로 만들기
head(data.frame(EFF=boiler9_eng$EFF, NOx=boiler9_eng$NOX))
