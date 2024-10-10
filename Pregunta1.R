#ensayo de Bernoulli

x = c(0,1)
f = c(0.68, 0.32)

plot(x, f, type='h', ylim= c(0,1), col='red')
points(x,f, pch=16, col='red')

n= 43
muestra = sample(x, n, f, replace=TRUE)
table(muestra)/n
pie(table(muestra))
mean(muestra)

bar= barplot(table(muestra)/n, ylim= c(0,1))
lines(bar, f, type='h', , col='red')
points(bar,f, pch=16, col='red')


muestra = sample(x, n, f, replace=TRUE)
muestra
y = function(i){sum(sample(x, n, f, replace=TRUE))}
y(1)
y(2)

set.seed(123)
m = 400000
barplot(table(sapply(1:m, y)))

encuestas = (sapply(1:m, y))
fr = table(encuestas)/m  
fr['13']             

dbinom(13, 43, 0.32)
xx = names(fr)
br = barplot(table(encuestas)/m)
lines(br, dbinom(1:29, 43, 0.32), type='h', col='red')
points(bar,f, pch=16, col='red')