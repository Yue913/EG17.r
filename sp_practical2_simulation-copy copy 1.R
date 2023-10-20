## Overview:
## This project simulates a stochastic queuing model depicting a real-life scenario where cars go through French and British stations consecutively 
## before approaching a French ferry terminal. 
## The entire process is simulated over a duration of two hours, during which cars arrive randomly at French stations with a given probability 
## and choose the the shortest queue among stations. After completion of processing at a French station, cars can proceed to British stations 
## with the same rule (shortest queues), but should also take into account a capacity constraint. 

## Key assumptions for this project are:
## (1) Arrival of cars only happens at the first 1.5 hours and follows exponential distribution.
## (2) Processing time at both the French and British stations follows uniform distributions. 
## (3) Simulation is updated and proccessed on a second basis. More precisely, cars can only enter and leave queues at the start or end of a second respectively, 
##     ignoring their order of these two activities in second.

## This project mainly consists of three parts: 
## The first part defines a model via function to stimulate a whole queueing process. Main output are average queue length and expected waiting time persecond.
## The second part runs model in part 1 for different hyperparameters to see their impact.
## The third part stimulates for 100 time to find the average probability of not completing processing before ferry take off (i.e., in two hours).



## Part 1 
## This part stimulates a second-wise model, where queue length and remaining processing time of 10 queues (5 for each station: french station and british station) 
##  are updated dynamically every second as time passes. 
## Definition of inputs: 
##        mf=number of french stations; mb=number of British stations
##        a.rate=arriving rate (a car arrives at probability of a.rate every second)
##        processing time at British station follows uniform(tmb,tmb+trb)
##        processing time at British station follows uniform(tmf,tmf+trf)
##        maxb = maximum amount of cars that a British station can accept
## Definition of outputs:
##        nf: a vector giving the average length of the French queues 
##        nb: a vector giving the average length of the British queues
##        eq: a vector giving the average expected waiting time for a car at the start of the french queue
##            expected waiting time evluates the (expected) waiting time of a new comer to this system, evaluated every second.
##            It is defined as the sum of:
##            the average queue length at the French stations multiplied by the expected handling time (expectation of uniform distribution) of a French station, 
##            and the average queue length at the British stations multiplied by the expected handling time of a British station, 

qsim=function(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20){

  ## Step 1: parameters set up
  second_enter=1.5*3600
  second_nenter=0.5*3600

  
  ## Step 2: producing intermediate variable
  ## present length of queues in French and British stations
  f_queue=rep(0,mf)
  b_queue=rep(0,mb)
  ## present the length of processing time for each car in the queues
  f_time_queue=rep(0,mf)
  b_time_queue=rep(0,mb)
  ## store the queue length for every simulation second
  f_queue_table=matrix(0,ncol=mf,nrow=second_enter+second_nenter)
  b_queue_table=matrix(0,ncol=mb,nrow=second_enter+second_nenter)
  ## maximum position that British stations can offer (updated for every second)
  b_vaccancy=mb*maxb

  
  ## Step 3: stimulate the process when new cars come in (i.e. enter the system)
  ## The following process hold for every simulation second
  for (t in 1:second_enter){
    
    ## Step 3.1 whether a car comes in french station; if yes, which queue will it be in
    set.seed(13*t) ## ensure comparability of models with different hyperparameters setting
    id_come=sample(c(0,1),prob=c(1-a.rate,a.rate),size=1,replace=FALSE) ## whether a car comes in
    consistent_add_f=as.integer(sample(as.character(which(f_queue==min(f_queue))),size=1,replace=FALSE)) ## if yes, which queue will it be
    f_queue[consistent_add_f]=f_queue[consistent_add_f]+(1*id_come) ## new car expands the queue by 1
    
    ## Step 3.2 updates of changes in French queues
    f_congestion=rep(0,mf) ## initialize congestion condition
    ## process end, length of corresponding queue should minus 1. if there are car after we should move to process the next car
    f_time_queue[f_time_queue<=0 & f_queue>0]=
      runif(length(intersect(which(f_time_queue<=0),which(f_queue>0))),min=tmf,max=tmf+trf)*f_congestion[f_time_queue<=0 & f_queue>0]
    f_time_queue[f_time_queue>0]=f_time_queue[f_time_queue>0]-1 ## remaining processing time minus 1 as one second pass
    no_maintain=intersect(which(f_time_queue<=0),which(f_queue>0)) ## which queue should have been process next car
    ## Because of capacity constraint of british queues, only limited number of queue can be updated
    no_maintain_queue=no_maintain[sample(length(no_maintain),min(b_vaccancy,length(no_maintain)))] ## which queue can be updated
    f_queue[no_maintain_queue]=f_queue[no_maintain_queue]-1 ## one car leave selected queue
    f_congestion[no_maintain_queue]=1 ## only if the car leave, processing time can be updated
    f_queue_table[t,]=f_queue ## fill the recording table by length of queue this second

    ## Step 3.3 how many cars come in british station, which queue will they be in
    number=length(no_maintain_queue) ## how many new comers to british queues
    while (number>0){
      ## in every iteration, some new comers choose their positions and the others should wait for next iteration
      random_sample=as.integer(sample(as.character(which(b_queue==min(b_queue))),min(number,sum((b_queue==min(b_queue))+0)),replace=FALSE))
      b_queue[random_sample]=b_queue[random_sample]+1 ## chosen queue expand by 1
      number=number-length(random_sample) ## remaining car need to be attributed position
    }
    
    ## Step 3.4 updates of changes in British queues
    ## process end, length of corresponding queue should minus 1. if there are car after we should move to process the next car
    b_time_queue[b_queue>0) & b_time_queue<=0]=
      runif(length(intersect(which(b_queue>0),which(b_time_queue<=0))),min=tmb,max=tmb+trb)
    b_time_queue[b_time_queue>0]=b_time_queue[b_time_queue>0]-1 ## remaining processing time minus 1 as one second pass
    b_queue[b_queue>0 & b_time_queue<=0]=b_queue[b_queue>0 & b_time_queue<=0]-1 ## one car leave the queue ending service
    b_queue_table[t,]=b_queue ## fill the recording table by length of queue this second
    b_vaccancy=mb*maxb-sum(b_queue) ## update the remaining vacancies in british station
  }


  ## Step 4: stimulate the process when no new car can come in
  ## Very similar to step 3. Only difference to Step 3 is that there is no need to update for new comers in French queues
  for (t in second_enter+1:second_nenter){
    
    ## Step 4.1 updates of changes in French queues
    f_congestion=rep(0,mf)
    f_time_queue[f_time_queue<=0 & f_queue>0]=
      runif(length(intersect(which(f_time_queue<=0),which(f_queue>0))),min=tmf,max=tmf+trf)*f_congestion[f_time_queue<=0 & f_queue>0]
    f_time_queue[f_time_queue>0]=f_time_queue[f_time_queue>0]-1
    no_maintain=intersect(which(f_time_queue<=0),which(f_queue>0))
    no_maintain_queue=no_maintain[sample(length(no_maintain),min(b_vaccancy,length(no_maintain)))]
    f_queue[no_maintain_queue]=f_queue[no_maintain_queue]-1
    f_congestion[no_maintain_queue]=1
    f_queue_table[t,]=f_queue

    ## Step 4.2 how many cars come in british station, which queue will they be in
    number=length(no_maintain_queue)
    while (number>0){
      random_sample=as.integer(sample(as.character(which(b_queue==min(b_queue))),min(number,sum((b_queue==min(b_queue))+0)),replace=FALSE))
      b_queue[random_sample]=b_queue[random_sample]+1
      number=number-length(random_sample)
    }
    
    ## Step 4.3 updates of changes in British queues
    b_time_queue[intersect(which(b_queue>0),which(b_time_queue<=0))]=
      runif(length(intersect(which(b_queue>0),which(b_time_queue<=0))),min=tmb,max=tmb+trb)
    b_time_queue[which(b_time_queue>0)]=b_time_queue[which(b_time_queue>0)]-1
    b_queue[intersect(which(b_queue>0),which(b_time_queue<=0))]=b_queue[intersect(which(b_queue>0),which(b_time_queue<=0))]-1
    b_queue_table[t,]=b_queue
    b_vaccancy=mb*maxb-sum(b_queue)
  }

  
  ## Step 5: output calculation
  nf=apply(f_queue_table,1,mean) ## average length of french queues
  nb=apply(b_queue_table,1,mean) ## average length of british queues
  eq=nf*(2*tmf+trf)/2+nb*(2*tmb+trb)/2 ## expected waiting time


  ## Step 6: return results
  ## First element of result list (actually is also a list) gives main result, the second result is for convenience of part 2 and part 3
  return(list(list(nf,nb,eq),b_queue_table))  
}



## Part 2

## this part first uses the function constructed above to produce a 4-panel, 2-row, 2-column plot. 
## The top-left and top-right plots show the fluctuations in the lengths of French & British queues, and expected queuing time, respectively, over time. 
## The second row shows the same pattern, with parameter 'tmb' reset to 40 (seconds).
## Then some analysis of result will be made.

## extract the result of nf, nb, eq from "qsim" function 
required_list1<-qsim()[[1]]
nf1<-required_list1[[1]]
nb1<-required_list1[[2]]
eq1<-required_list1[[3]]

required_list2<-qsim(tmb=40)[[1]]
nf2<-required_list2[[1]]
nb2<-required_list2[[2]]
eq2<-required_list2[[3]]
timex<-1:7200

## "par" function can make a customized layout where each grid contains a plot.
## assign c(2,2) to "mfrow" argument can obtain a layout of 2*2 plot grid.
par(mfrow=c(2,2))

## "plot" function is used to construct the customized plot by setting arguments:
## "type" sets the type of plot ('l' for line); "col" sets the color; "xlab(ylab)" sets the label for the x(y)-axis; "main" sets the title of the plot.
plot(timex,nf1,type='l',col='red',xlab='time',ylab='queue length',main='queue length over time')

## "lines" function adds a second plot to the same plot
lines(timex,nb1,type='l',col='blue')

## "legend" function is used to add keys that helps identify the meaning of various elements within a plot. 
## "lty" sets the line type for the line segments in the legend (1 for solid line type); "cex" adjusts the size of the text in the legend.
ylim<-range(c(nf1,nb1))
legend("topright", legend = c("nf1", "nb1"), col = c("red", "blue"),lty=1,cex=0.5)
plot(timex,eq1, type='l',xlab='time',ylab='expected queuing time',main='expected queuing time over time')

## The "range" function sets the y-axis limits to encompass all data points. 
## Adjust the "ylim" argument in the "plot" function to ensure visibility of all data points within the plot area.
ylim<-range(c(nf2,nb2))
plot(timex,nf2,type='l',col='red',xlab='time',ylab='queue length',main='queue length over time(tmb=40)',ylim=ylim)
lines(timex,nb2,type='l',col='blue')
legend("topright", legend = c("nf2", "nb2"), col = c("red", "blue"),lty=1,cex=0.5)
plot(timex,eq2,type='l',xlab='time',ylab='expected queuing time',main='expected queuing time over time(tmb=40)')


## Analysis of result:
## (1) From the plot, it is evident that resetting tmb to 40 seconds results in the French queue length remaining nearly the same.  
##     In contrast, the British queue dramatically increases, reaching up to 15 cars per second, nearly 7 times of the results with default tmb. 
## (2) The expected queuing time also experience a dramatic increase, about three times as long as default case. 
##     As a result, british queue in tmb=40 case dominates the eq time. 
## (3) French line is first greater than that of british, but it will soon be caught up. 
##     This happens faster in tmb=40 case because of the longer service time.



## Part 3
## this part utilizes the "qsim" function to run 100 simulations to find the probability of at least one car missing the ferry departure when tmb=40

## for each iteration of this "for" loop, the last row of "b_queue_table" is examined. If the sum of all the entries in this row exceeds 0, this indicates 
## that there is at least one car still waiting in the queue at the simulation's end. we will count the presence of this situation in 100 simulations.
results_list <- list()
missing_departure<-0
for (i in 1:100){
  
## extract the result of "b_queue_table" from "qsim" function and put it into the empty list defined previously
  results_list[[i]]<-qsim(tmb=40)[[2]]
  
## extract the last row from the "b_queue_table" matrix
  last_row<-results_list[[i]][nrow(results_list[[i]]),]
  
  if (sum(last_row)>0){
    missing_departure<-missing_departure+1
  }
}

## the probability of at least one car missing the ferry departure is calculated as the frequency of missing_departure over the 100 runs
print(missing_departure/100)
