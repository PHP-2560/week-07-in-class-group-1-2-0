#PHP 2560
#Lucas Paulo de Lima Camillo
#B01478147


#1)
#returns times to lose, and money
gamble = function(money = 1000, limit = 5000, prob = 0.5){
  count = 0
  while (money>0 & count<limit){ #keeps going up to the limit of hands and money is bigger than 0
    money = money + 100*(2*rbinom(1,1, p = prob)-1) #random win or lose
    count = count + 1
  }
  if(count == limit){#returns the number of hands it took to lose and the money
    return(c(NA, money)) #if the person did not lose in the limit number of hands, returns NA and the amount of money
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
mean(simulation4[1,], na.rm = TRUE) #212.758

#d- 
simulation5 = replicate(n, gamble(limit = 100, prob = 18/38))
mean(simulation5[2, which(is.na(simulation5[1,]))]) #1211.268
var(simulation5[2, which(is.na(simulation5[1,]))]) #467292.1

#e-
simulation6 = replicate(n, gamble(limit = 500, prob = 18/38))
mean(simulation6[2, which(is.na(simulation6[1,]))]) #2029.213
var(simulation6[2, which(is.na(simulation6[1,]))]) #996864.1


#3)
unfair_game = function(money = 0, prob = 0.48, increment = 0.01){
  for (i in 1:100000){ #uses 100000 hands
    temp = 2*rbinom(1,1,prob) - 1 #either wins or lose
    money = money + 100*temp #+ or - 100
    if (temp == 1){ #if it wins, changes the probability by the increment
      if (prob<1){
        prob = prob + increment
        if (prob>1){
          prob = 1
        }
      }
    } else { #if it loses, resets the probability
      prob = 0.48
    }
  }
  return(money)
}

#a-
simulation4 = RepParallel(100, unfair_game())
mean(simulation4) #expected = -198732

#b- 
simulation5 = replicate(100, unfair_game(prob = 0.5))
mean(simulation5) #-200732
simulation6 = replicate(100, unfair_game(prob = 0.46))
mean(simulation6) #-201574
#fair:
simulation7 = replicate(100, unfair_game(prob = 0.74)) #5436
mean(simulation7)

#c-
#(sort of)fair:
simulation8 = replicate(100, unfair_game(increment = 0.012))
mean(simulation8) #25340










