```{r}
phone <- 80 
transportation <- 20
groceries <- 527
gym <- 10
rent <- 1500
other <- 83
expenses <- c(phone, transportation, groceries, gym, rent, other)
barplot(expenses)

total_expenses <- phone + transportation + groceries + gym + rent + other
semester_expenses <- total_expenses*5
year_expenses <- total_expenses*10
```
```{r}
a <- 3
b <- 4
hypotenuse <- sqrt(a^2 + b^2)
````
```{r}
factorial(5)/(factorial(2) * factorial(3))
choose(n = 5, k = 2)*(.5^5)
```

Probably of getting exactly 3 sixes from 10 fair die rolls
```{r}
choose(n = 10, k = 3)*((1/6)^3)*((5/6)^7)
```

Probability of 2 heads in 5 coin tosses:
```{r}
dbinom(2, size = 5, prob = .5) + dbinom(3, size = 5, prob = .5) + dbinom(4, size = 5, prob = .5) + dbinom(5, size = 5, prob = .5)
```

```{r}
Probability of getting 3 sixes in 3 rolls of a die
dbinom(3, size = 3, prob = 1/6) 
```

If I wanted chance of heads = 35%, I would modify prob = .35

```{r}
Probability of getting 3 heads in 5 tosses w 35% chance of heads
dbinom(3, size = 5, prob = .35) 
```

Calculate: 3x^2+4x+8 when x = 2:
```{r}
x <- 2
3*(x^2) + 4*x + 8
```
Calculate: 3x^2+4x+8 but now with a numeric sequence for x using x <- -3:3
```{r}
x <- -3:3
3*(x^2) + 4*x + 8
```

Files show my directory
Help shows documentation; house shows the home screen of resources
History provides a history of what I have run. To Consol and To Source send commands to each respective place.
I can also save and load history
Environment shows my environment variables. I can load and save variablese as well as import dataset.
