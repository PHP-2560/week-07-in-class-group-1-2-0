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
  probability = prob
  for (i in 1:100000){ #uses 100000 hands
    temp = 2*rbinom(1,1,probability) - 1 #either wins or lose
    money = money + 100*temp #+ or - 100
    if (temp == 1){ #if it wins, changes the probability by the increment
      if (probability<1){
        probability = probability + increment
        if (probability>1){ #just in case there is a small error in the way R handles doubles and p gets slightly more than 1
          probability = 1
        }
      }
    } else { #if it loses, resets the probability
      probability = prob
    }
  }
  return(money)
}

#a-
simulation4 = replicate(100, unfair_game())
mean(simulation4) #expected = -198732

#b- 
simulation5 = replicate(100, unfair_game(prob = 0.5))
mean(simulation5) #281642
simulation6 = replicate(100, unfair_game(prob = 0.46))
mean(simulation6) #-618958
#fair:
probability = 0.48
fair = 999999
while(fair > 10000 & probability < 0.5){
  probability = probability + 0.002
  simulation7 = replicate(100, unfair_game(prob = probability)) #9488
  fair = abs(mean(simulation7))
}
print(probability)
#fair initial prob = 0.49

#c-
#(sort of)fair:
simulation8 = replicate(100, unfair_game(increment = 0.012))
mean(simulation8) #25340



#4- 
boot_ci = function(datus, repetitions = 1000){
  boot = data.frame(matrix(rep(NA, 2*repetitions), nrow = 2)) #makes a df with two rows full of NAs
  for (i in 1:repetitions){
    temp = sample(datus, length(datus), replace = TRUE) #resample
    boot[1,i] = quantile(temp, probs = c(0.025, 0.975))[[1]] #gets 0.025th percentile statistic
    boot[2,i] = quantile(temp, probs = c(0.025, 0.975))[[2]] #gets 0.975th percentile statistic
  }
  return(c('0.025 percentile CI' =quantile(boot[1,], probs = c(0.05, 0.95)), '0.975 percentile CI' = quantile(boot[2,], probs = c(0.05, 0.95)))) #returns 95% CI
}










