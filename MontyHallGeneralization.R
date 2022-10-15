resample <- function(x, ...) x[sample.int(length(x), ...)]

N <- 10000 # number of games
k <- 100    # number of doors
a <- 2  # number of doors Monty opens
h <- 10 # prizes


# assign the h cars at random among the k possible doors
prizes<- sapply(1:N, function(j) sample(1:k,h,replace = FALSE))
prizes <- matrix(prizes,h,N)
prizes <- t(prizes)

# the contestant picks a single door at random
pick <- sample(1:k,N,replace = TRUE)


# Monty reveals a doors, to reveal goats
monty <- sapply(1:N,function(j) resample(setdiff(1:k,c(pick[j],prizes[j,])),a,replace = FALSE))
monty <- matrix(monty,a,N)

# What would happen if the contestant chooses to repick. 
# In the case of 3 doors with 1 door revealed this amounts to switching
pick2 <- sapply(1:N,function(j) resample(setdiff(1:k,c(pick[j],monty[,j])),1))

games <- cbind(prizes,pick,t(monty),pick2)

#print(games)

# probability of winning if not switching
result.stay <- sapply(1:N, function(j) pick[j] %in% prizes[j,])
print(mean(result.stay))

# probability of winning if re-picking
result.repick <- sapply(1:N, function(j) pick2[j] %in% prizes[j,])
print(mean(result.repick))

# Compare to the "theory"
print(h/k)
print((h/k)*(h-1)/(k-a-1) + (1 - h/k)*h/(k-a-1))