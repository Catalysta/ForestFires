We also noticed a trend in observation order with respect to `Larea`: this is likely due to the fact that this data came from two different data sets which were combined manually. [citation] When combining the two data sets, it seems that the first set was sorted by `area`, while the second set was ordered chronologically:
  
  ```{r}
plot(forest3$Larea, main = "Observation Order vs. log(area)")

```