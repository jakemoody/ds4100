# single 
start_1 <- Sys.time()
mostStrikes(bird_strikes)
end_1 <- Sys.time()
# double
start_2 <- Sys.time()
mostStrikes(double_bird_strike)
end_2 <- Sys.time()
# quadrouple
start_4 <- Sys.time()
mostStrikes(quad_bird_strike)
end_4 <- Sys.time()

time_1 <- end_1 - start_1
time_2 <- end_2 - start_2
time_4 <- end_4 - start_4

time_2 - time_1
time_4 - time_1

