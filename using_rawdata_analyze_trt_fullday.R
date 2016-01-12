

library(xlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(ggmap)
library(rjson)
library(rlist)
# ??rlist
# 
#  install.packages("rlist")
library(RJDBC)

# ??RJDBC
#RJDBCcon <-odbcConnect("test2",uid="root",pwd="ceciits")
# ?dbListTables
# channel <- odbcDriverConnect("test1")

dev<-JDBC("com.mysql.jdbc.Driver",
          "C:\\jar\\mysql-connector-java-5.1.38\\mysql-connector-java-5.1.38-bin.jar"
)

conn<-dbConnect(dev,
                "jdbc:mysql://localhost:3306/etag",
                "root",
                "ceciits"
)
dbListTables(conn)

#   
#   x.dates<-c("17.03.2001", "16.03.2001", "15.03.2001")
#   x.times<-c("23:39:15", "23:50:00", "22:23:43")
#   x.datetime <- paste(x.dates,x.times)
#   str(strptime(x.datetime, "%d.%m.%Y %H:%M:%S")) # revise to %d.%m.%Y
#   Sys.setlocale("LC_TIME", lct)
#   

# lct <- Sys.getlocale("LC_TIME")
# Sys.setlocale("LC_TIME", "C")
# str(z)

# 
# date<-"2013-11-14"
# date<-as.Date(date)
# ss<- weekdays(date)

test_data<-dbGetQuery(conn,"select * from tce01_20151201")


test_data<-mutate(test_data,full_time=as.POSIXct(strptime(paste(date,time),"%Y-%m-%d %H:%M:%S")))

date_start_time<-as.POSIXct(test_data$date[1])
date_start_time<-as.numeric(date_start_time)

o_point<-"fetag-01"
d_point<-"fetag-04"

tt_epc_trt<-list()

#for(ts in 1:48){ #30mins一個time range
  
  ts=35
  pass_time<-as.numeric(test_data$full_time)
  od_set<-filter(test_data,as.numeric(test_data$full_time) < date_start_time+30*60*ts 
                           & as.numeric(test_data$full_time)>=date_start_time+30*60*(ts-1))
  
  #o_point<-

  od_set_o<-filter(od_set,od_set$station==o_point)   #時間範圍內過o點的所有epc
  
  epc_list<-data.frame(od_set_o$epc,od_set_o$full_time)
  colnames(epc_list)<-c("epc","ps_o_time")
  

  for(i in 1:nrow(epc_list)){


  o_epc<-as.character(epc_list$epc[i])
  #o_epc
  
  ps_o_time<-as.numeric(epc_list$ps_o_time[i]) #目標epc過o點時間
  #ps_o_time
  #從找出後來的所有過點資料中找出此epc之通過設定之d點的時間
  
  od_set_o<-filter(test_data,epc==o_epc & station==d_point 
                                        & as.numeric(full_time)>ps_o_time)
  if(nrow(od_set_o)==0){
    
    next;
    
  }
  
  else{
  ps_d_time<-as.numeric(sort(od_set_o$full_time))#目標epc過d點時間
  epc_od_trt<-ps_d_time-ps_o_time
  #epc_od_trt

  single_epc_trt<-c(o_epc,epc_od_trt)
  
  tt_epc_trt<-c(tt_epc_trt,list(single_epc_trt))
  }
  }
  tt_epc_trt
  

  
  
  
#   tt_epc_trt
#   ddf<-as.data.frame(tt_epc_trt)
#     
#   f1<-c(1,2)
#   f2<-c(1,3)
#   f3<-list(f1=c(1,4))
#   
#   f_list1<-list(f1=f1,f2=f2)
#   
#   f_list1<-f_list1.app
#   f_list1
#   f_list2<-append(f_list1,f3)
#   
#   f_list2<-c(f_list1,f3)
#   f_list2
#   f_list1<-f_list1
#   
#   str(f_list2)
#   f_list2[[i]][2]
#   f_list2$
#   
#   dff<-as.data.frame(f_list3)
#   str(dff)
#   name1<-f_list2[[1]]
#   str(f_list2)
#   f_list2.name1
  
    
#     epc_ip=c()
#   trt_t=c()
#     dff<-list(epc_ip=c(),trt_t=c())
#   str(dff)
#   add_ip=1
#   
#   add_trt=23
#   
#   epc_ip=c(epc_ip,add_ip)
#   trt_t=c(trt_t,add_trt)
#   
#   dff<-list(epc_ip=epc_ip,trt_t=trt_t)
#   
#   dff
# 
#   (dff$trt_t==23)
#   }
#   
  
  
  
  
  
  
  #newdata<-na.omit(olddata)

    

  
  cc<-nrow(table(epc_list$epc))
  
  
  
  
  
  
  
  
  
  od_set$station
  
  }
  
  
  
  
}

  
  



kk_m<-mutate(kkk,full_time=as.POSIXct(strptime(paste(date,time),"%Y-%m-%d %H:%M:%S")))
str(kk_m)



kkk$time<-as.POSIXct(kkk$time)
str(kk_m)
dbWriteTable(conn,"test11",kkk)



dbDisconnect(conn)

