s <- FielDHub::alpha_lattice(t = 120, k = 10, r = 3, l = 1, plotNumber = 101, seed = 12)
plot(s, optionLayout = 3)

s <- FielDHub::RCBD(t = 24, reps = 3, continuous = T, 
                    l = 1, plotNumber = 101, seed = 12)
plot(s, optionLayout = 1)

plot(s)




optionLayout = 1
planter = "serpentine"
l = 1 
orderReps = "vertical_stack_panel"























