#PHP 2560
#Lucas Paulo de Lima Camillo
#B01478147


#1)
#returns times to lose, and money
gamble = function(money = 1000, limit = 5000, prob = 0.5){
  count = 0
  while (money>0 & count<limit){
    money = money + 100*(2*rbinom(1,1, p = prob)-1)
    count = count + 1
  }
  if(count == limit){
    return(c(NA, money))
  }else{
    return(c(count, money))
  }
}

n = 1000
simulation1 = replicate(n, gamble())

#a-
prob_a = length(which(simulation1[1,]<100))/n #0.325

#b-
prob_b = length(which(simulation1[1,]<500))/n #0.659

#c-
mean(simulation1[1,], na.rm = TRUE) #510.7832

#d- 
simulation2 = replicate(n, gamble(limit = 100))
mean(simulation2[2, which(is.na(simulation2[1,]))]) #1475.037
var(simulation2[2, which(is.na(simulation2[1,]))]) #590109.5

#e-
simulation3 = replicate(n, gamble(limit = 500))
mean(simulation3[2, which(is.na(simulation3[1,]))]) #2957.558
var(simulation3[2, which(is.na(simulation3[1,]))]) #2477144

#in the while loop that I used in the function, I could just add something like "if (player == bust) break".


#2)

n = 1000
simulation4 = replicate(n, gamble(prob = 18/38))

#a-
prob_a = length(which(simulation4[1,]<100))/n #0.476

#b-
prob_b = length(which(simulation4[1,]<500))/n #0.898

#c-
mean(simulation4[1,], na.rm = TRUE) 

#d- 
simulation5 = replicate(n, gamble(limit = 100, prob = 18/38))
mean(simulation5[2, which(is.na(simulation5[1,]))]) 
var(simulation5[2, which(is.na(simulation5[1,]))]) 

#e-
simulation6 = replicate(n, gamble(limit = 500, prob = 18/38))
mean(simulation6[2, which(is.na(simulation6[1,]))]) 
var(simulation6[2, which(is.na(simulation6[1,]))]) 










