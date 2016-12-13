library(runjags)
library(ggplot2)

numTrials = 2000

expectation.decay = .005
expectationexpectation.magnitudedecay = .6

offset.decay = .0035
offset.magnitude = 4

certainty.decay = .02
certainty.magnitude = .8

SD = 1

#Returns a random decimal from 0 to 1
randDec = function(){
  runif(1, min = 0, max = 1)
}

#Returns 1 for positive values, -1 for negative values
signOf = function(n){
  if(n > 0){
    1
  }else if (n < 0){
    -1
  }else{
    0
  }
}

#Generates 'value' 'percent' percent of the time, otherwise produces 'otherval'
generateStim = function(percent, value, otherVal){
  if(randDec() > percent){
    value
  } else{
    otherVal
  }
}

#Values for stimuli one are pressumed 
stim1.vals = numeric(numTrials)

#Types of Lags for stim2
constant.stim2.lag = rep(1,numTrials)
consistant.stim2.lag = mapply(function(x){generateStim(.4, 1, rnorm(1, 1, SD))}, numeric(numTrials))
random.stim2.lag = mapply(function(x){rnorm(1, 0, SD)}, numeric(numTrials))
bimodal.stim2.lag = c(mapply(function(x){generateStim(.4, 1, rnorm(1, 1, SD))}, numeric(numTrials/2)), mapply(function(x){generateStim(.4, -1, rnorm(1, -1, SD))}, numeric(numTrials/2)))

#Change condition.vals to be any of the above variables
condition.vals = bimodal.stim2.lag

d <- data.frame(
  S1.obj = stim1.vals,
  S1.expectation = numeric(numTrials),
  S1.offset = numeric(numTrials),
  S1.subj = numeric(numTrials),

  S2.obj = condition.vals,
  S2.expectation = numeric(numTrials),
  S2.certainty = rep(10,numTrials),
  S2.offset = numeric(numTrials),
  S2.subj = condition.vals
)

for(i in 2:(numTrials-1)) {
  d$S2.offset[i] = ((d$S2.offset[i-1]*(1-offset.decay)) + 
                         (dnorm(d$S2.obj[i], d$S1.obj[i], SD)*offset.decay)*signOf(d$S2.obj[i] - d$S1.obj[i])*offset.magnitude)
  d$S2.subj[i] = ((d$S2.obj[i]*(d$S2.certainty[i])) + (d$S2.expectation[i]*(1-d$S2.certainty[i]))) - d$S2.offset[i]
  d$S2.expectation[i+1] = (d$S2.expectation[i]*(1-expectation.decay)) + (d$S2.obj[i] * expectation.decay)
  d$S2.certainty[i+1] = min(1, max(0,(d$S2.certainty[i]*(1-certainty.decay)) + (abs(d$S2.obj[i]-d$S2.expectation[i]))*certainty.decay*certainty.magnitude))

}

results = data.frame(
  d = d
  )

graph = ggplot(results, aes(x = 1:(numTrials)))+
  xlab("trial")+
  ylab("Time from first stimuli")+
  geom_line(aes(y = numeric(numTrials)), colour = "black", size = .5)+
  geom_point(aes(y = d$S2.obj), colour = "black")+
  #geom_line(aes(y = 1-d$S2.certainty), colour = "red", size = 3) + 
  geom_line(aes(y = d$S2.subj), colour = "green")+
  geom_line(aes(y = d$S2.offset), colour = "blue", linetype = "dashed", size = 2)+
  geom_line(aes(y = d$S2.expectation), colour = "yellow", size = (d$S2.certainty+1)*(d$S2.certainty+1)) + 
  theme(panel.background = element_rect(fill = 'gray')) + 
  xlim(1, numTrials-1)+ 
  ylim(-2.5, 2.5)