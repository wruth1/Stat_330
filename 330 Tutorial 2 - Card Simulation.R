#Construct the deck
suits = c("d", "c", "h", "s")
types = 1:13
cards = expand.grid(suits, types)
colnames(cards) = c("suit", "type")

n = nrow(cards) #Number of cards in the deck


M = 100000 #Number of hands to draw
count.4 = 0 #Number of hands that contain at least 4 spades
count.5 = 0 #Number of hands that contain 5 spades

for(i in 1:M){
  hand.ind = sample(1:n, size = 5, replace = F) #Indices of the cards I will draw
  hand = cards[hand.ind,] #Draw these cards
  hand.suits = hand$suit
  are.spades = hand.suits == "s"
  count.spades = sum(are.spades)
  #Check whether at least 4 spades were drawn
  if(count.spades >= 4){
    count.4 = count.4 + 1
    #Check whether 5 spades were drawn
    if(count.spades == 5){
      count.5 = count.5 + 1
    }
  }
}

#Estimated probability of getting the specified number of spades
approx.4 = count.4 / M
approx.5 = count.5 / M

#Exact probability of getting the specified number of spades (from our calculations)
exact.4 = (choose(13, 4)*choose(39, 1) + choose(13, 5)) / choose(52, 5)
exact.5 = choose(13, 5) / choose(52, 5)

#Approximate and exact conditional probabilities
approx.conditional = approx.5 / approx.4
exact.conditional = exact.5 / exact.4


### Questions ### (stars correspond roughly to difficulty, more stars mean I think it's harder)

#* Why did I nest the if statements in my for loop instead of separating them?
#** Why does approx.4 seem to be a better estimator of exact.4 than approx.5 is of exact.5? 
#   (Hint: How often do 5-spade hands occur?)