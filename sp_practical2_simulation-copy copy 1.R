##  This code simulates a stochastic queuing model depicting a real-life scenario
## where cars go through French and British passport control stations consecutively
## before approaching a French ferry terminal. The entire process is simulated
## per second over a duration of two hours. Cars arrive randomly at French stations
## with a given probability and choose the the shortest queue among a given number 
## of stations. After completion of processing at a French station, a car proceeds
## to one of the British stations by selecting the shortest queue, taking into
## account a capacity constraint. Processing time at both the French and British
## stations follows uniform distributions. The French stations stop check-in thirty 
## minutes prior to the ferry departure.

queue_fill=function(number,queue,tmb=30,trb=40){

  while (number>0){
    random_sample=as.integer(sample(as.character(which(queue==min(queue))),min(number,sum((queue==min(queue))+0)),replace=FALSE))
    queue[random_sample]=queue[random_sample]+1
    number=number-length(random_sample)
  }
  return(queue)
}



qsim=function(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20){
  ## mf=number of french stations; mb=number of british stations
  ## a.rate=arriving rate (a car arrives at probability of a.rate every second)
  ## processing time at british station follows uniform(tmb,tmb+trb)
  ## processing time at british station follows uniform(tmf,tmf+trf)
  ## maxb=maximum amout of cars that a british station can accept
  

  
  second_enter=1.5*3600
  second_nenter=0.5*3600
  f_queue=rep(0,mf)
  f_queue_table=matrix(0,ncol=mf,nrow=second_enter+second_nenter)
  f_time_queue=rep(0,mf)
  f_congestion=rep(0,mf)
  b_queue=rep(0,mb)
  b_queue_table=matrix(0,ncol=mb,nrow=second_enter+second_nenter)
  b_time_queue=rep(0,mb)
  b_vaccancy=mb*maxb
  
  
  for (t in 1:second_enter){
    set.seed(13*t)
    id_come=sample(c(0,1),prob=c(0.9,0.1),1)
    consistent_add_f=as.integer(sample(as.character(which(f_queue==min(f_queue))),1))
    f_add=which(f_queue==0)
    f_time_queue[intersect(consistent_add_f,f_add)]=id_come*runif(1,min=tmf,max=tmf+trf)
    f_queue[consistent_add_f]=f_queue[consistent_add_f]+(1*id_come)
    
    
    f_time_queue[which(f_time_queue>0)]=f_time_queue[which(f_time_queue>0)]-1
    no_maintain=intersect(which(f_time_queue<=0),which(f_queue>0))
    choose=sample(length(no_maintain),min(b_vaccancy,length(no_maintain)),replace=FALSE)
    no_maintain_queue=no_maintain[choose]
    f_queue[no_maintain_queue]=f_queue[no_maintain_queue]-1
    f_time_queue[no_maintain_queue]=runif(length(no_maintain_queue),min=tmf,max=tmf+trf)
    f_queue_table[t,]=f_queue
    
    b_del=intersect(which(b_time_queue<=0),which(b_queue>0))#accomplish processing, should be deleted
    b_queue=queue_fill(length(no_maintain_queue),b_queue)
    b_add=intersect(b_del,which(b_queue>0))
    b_time_queue[b_add]=runif(length(b_add),min=tmb,max=tmb+trb)
    b_time_queue[which(b_time_queue>0)]=b_time_queue[which(b_time_queue>0)]-1
    b_queue[b_del]=b_queue[b_del]-1
    b_queue_table[t,]=b_queue
    
    b_vaccancy=mb*maxb-sum(b_queue)
  }
  
  for (t in second_enter+1:second_nenter){
    
    f_time_queue[which(f_time_queue>0)]=f_time_queue[which(f_time_queue>0)]-1
    no_maintain=intersect(which(f_time_queue<=0),which(f_queue>0))
    choose=sample(length(no_maintain),min(b_vaccancy,length(no_maintain)),replace=FALSE)
    no_maintain_queue=no_maintain[choose]
    f_queue[no_maintain_queue]=f_queue[no_maintain_queue]-1
    f_time_queue[no_maintain_queue]=runif(length(no_maintain_queue),min=tmf,max=tmf+trf)
    f_queue_table[t,]=f_queue
    
    b_del=intersect(which(b_time_queue<=0),which(b_queue>0))#accomplish processing, should be deleted
    b_queue=queue_fill(length(no_maintain_queue),b_queue)
    b_add=intersect(b_del,which(b_queue>0))
    b_time_queue[b_add]=runif(length(b_add),min=tmb,max=tmb+trb)
    b_time_queue[which(b_time_queue>0)]=b_time_queue[which(b_time_queue>0)]-1
    b_queue[b_del]=b_queue[b_del]-1
    b_queue_table[t,]=b_queue
    
    b_vaccancy=mb*maxb-sum(b_queue)
  }
  nf=apply(f_queue_table,1,mean)
  nb=apply(b_queue_table,1,mean)  
  eq=nf*(2*tmf+trf)/2+nb*(2*tmb+trb)/2

  return(list(list(nf,nb,eq),b_queue_table))
}

## Part 2

## this part uses the function constructed above to produce a 4-panel, 2-row,
## 2-column plot. The top-left and top-right plots show the fluctuations in the 
## lengths of French & British queues, and expected queuing time, respectively, 
## over time. The second row shows the same pattern, with the default parameter 
## 'tmb' reset to 40 seconds

## extract the result of nf, nb, eq from "qsim" function 
result1<-qsim()
required_list1<-result1[[1]]

## this will give the 3 vectors nf,nb,eq 
print(required_list1)

nf1<-required_list1[[1]]
nb1<-required_list1[[2]]
eq1<-required_list1[[3]]

result2<-qsim(tmb=40)
required_list2<-result2[[1]]
nf2<-required_list2[[1]]
nb2<-required_list2[[2]]
eq2<-required_list2[[3]]
timex<-1:7200

## "par" function can make a customized layout where each grid contains a plot.
## assign c(2,2) to "mfrow" argument can obtain a layout of 2*2 plot grid.
par(mfrow=c(2,2))

## "plot" function is used to construct the customized plot by setting arguments:
## "type" sets the type of plot, 'l' for line plot; "col" sets the color for the 
## plot; "xlab(ylab)" sets the label for the x(y)-axis; "main" sets the title of the plot.
plot(timex,nf1,type='l',col='red',xlab='time',ylab='queue length',main='queue length over time')

## "lines" function is used to add a second plot to the same plot
lines(timex,nb1,type='l',col='blue')

## "legend" function is used to add keys that helps identify the meaning of various 
## elements within a plot. "lty" sets the line type for the line segments in the  
## legend, 1 for solid line type; "cex" adjusts the size of the text in the legend.
legend("topright", legend = c("nf1", "nb1"), col = c("red", "blue"),lty=1,cex=0.5)
plot(timex,eq1,type='l',xlab='time',ylab='expected queuing time',main='expected queuing time over time')

## The "range" function sets the y-axis limits to encompass all data points. Adjust
## the "ylim" argument in the "plot" function to ensure visibility of all data 
## points within the plot area.
ylim<-range(c(nf2,nb2))

plot(timex,nf2,type='l',col='red',xlab='time',ylab='queue length',main='queue length over time(tmb=40)',ylim=ylim)
lines(timex,nb2,type='l',col='blue')
legend("topright", legend = c("nf2", "nb2"), col = c("red", "blue"),lty=1,cex=0.5)
plot(timex,eq2,type='l',xlab='time',ylab='expected queuing time',main='expected queuing time over time(tmb=40)')

## from the plot, it is evident that resetting tmb to 40 seconds results in the 
## French queue length remaining nearly the same. In contrast, the British queue 
## steadily increases, reaching up to 15 cars per second, nearly 7 times of the 
## maximum default outcome, given the reduced British handling rate. The expected
## queuing time also exhibits a rising trend, reaching nearly 1000 seconds before
## check-in stops due to the dominant effect of changing in British queue length   

## Part 3

## this part utilizes the "qsim" function to run 100 simulations to find the 
## probability of at least one car missing the ferry departure when tmb=40

## for each iteration of this "for" loop, the last row of "b_queue_table" is
## examined. If the sum of all the entries in this row exceeds 0, this indicates 
## that there is at least one car still waiting in the queue at the simulation's
## end. we will count this situation as 1 and repeat this process 100 times 
results_list <- list()
missing_departure<-0
for (i in 1:100){
  
## extract the result of "b_queue_table" from "qsim" function and put it into the
## empty list defined previously
  results_list[[i]]<-qsim(tmb=40)[[2]]
  
## extract the last row from the "b_queue_table" matrix
  last_row<-results_list[[i]][nrow(results_list[[i]]),]
  
  if (sum(last_row)>0){
    missing_departure<-missing_departure+1
  }
}

## the probability of at least one car missing the ferry departure is calculated
## as the frequency of missing_departure over the 100 runs
print(missing_departure/100)
