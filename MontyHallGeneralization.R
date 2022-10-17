resample <- function(x, ...) x[sample.int(length(x), ...)] #Code for a new function that is called resample to resample the data after the contestant picks are made

N <- 10000 # number of games that are played in the Monte carlo simulation
k <- 100    # number of doors total that are available
a <- 2  # number of doors Monty opens
h <- 10 # prizes


# assign the h prizes at random among the k possible doors
prizes<- sapply(1:N, function(j) sample(1:k,h,replace = FALSE))
prizes <- matrix(prizes,h,N)
prizes <- t(prizes)

# the contestant picks a single door at random
pick <- sample(1:k,N,replace = TRUE)


# Monty reveals a doors, to reveal goats
monty <- sapply(1:N,function(j) resample(setdiff(1:k,c(pick[j],prizes[j,])),a,replace = FALSE)) #The data is resampled after another door reveals it is not the correct door
monty <- matrix(monty,a,N)

# What would happen if the contestant chooses to repick. 
# In the case of 3 doors with 1 door revealed this amounts to switching
pick2 <- sapply(1:N,function(j) resample(setdiff(1:k,c(pick[j],monty[,j])),1)) #This is a pick functin used when switching doors using the resample function

games <- cbind(prizes,pick,t(monty),pick2)

# probability of winning if not switching
result.stay <- sapply(1:N, function(j) pick[j] %in% prizes[j,]) #This is the proportion of correct choices based on a general number of prizes
print(mean(result.stay)) #This takes the average of the probability of winning

# probability of winning if re-picking
result.repick <- sapply(1:N, function(j) pick2[j] %in% prizes[j,]) #This uses the pick2 estimate which is the pick function used when switching the doors
print(mean(result.repick))

# Compare to the "theory"
print(h/k) #This is the probability of winning given you never switch doors. This probability is h/k because there are h prizes and k doors and you do not swtich
print((h/k)*(h-1)/(k-a-1) + (1 - h/k)*h/(k-a-1)) #This is the theory of conditional probability of the proportion winning given switching when there are h prizes and k doors
