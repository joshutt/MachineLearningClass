results <- replicate(1000, pla(N=10))
print("**** Problem 7,8 ***")
print(mean(results[1,]))
print(mean(results[2,]))


results <- replicate(1000, pla(N=100))
print("**** Problem 9,10 ***")
print(mean(results[1,]))
print(mean(results[2,]))
