library(pbapply)

suits = c("d", "c", "h", "s")
types = 1:13
cards = expand.grid(suits, types)
colnames(cards) = c("suit", "type")
# cards = paste0(cards.raw[,1], cards.raw[,2])

n = nrow(cards)


M = 1000000

matching = pbsapply(seq_len(M), function(i){
  hand.ind = sample(1:n, size = 5)
  hand = cards[hand.ind,]
  check.4 = sum(hand$suit == "s") >= 4
  check.5 = all(hand$suit == "s")
  return(c(check.4, check.5))
})

approx.both = apply(matching, 1, mean)
approx = approx.both[2]/approx.both[1]

exact = 9/(5*48)

print(approx)
print(exact)