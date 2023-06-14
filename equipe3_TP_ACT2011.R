###
### Mustapha Bouhsen
###

###
## Question 2
###

# Fonction qui calculer le prix Call Euro
call_p <- function(t, n, s, k, r, sigma)
{
    m <- 0:n
    S <- 1:(n+1)
    p <- 1:(n+1)
    
    
    u <- exp(r/n + sigma*sqrt(t/n))
    d <- exp(r/n - sigma*sqrt(t/n))
    q <- (exp(r*(t/n)) - d) / (u - d)
    
    for(i in 1:(n+1))
    {
        S[i] <- max(s*(u)^(m[i])*(d)^(n - m[i]) - k , 0)
        p[i] <- dbinom(m[i], n , q)
      
    }
    
    resultat <- 0
    
    resultat <- sum(p*S) * exp(-r*t)
    
    resultat
}

call_p(1, 52, 100, 110, 0.0112, 0.1564)


# Fonction qui calcule le prix Put Euro
put_p <- function(t, n, s, k, r, sigma)
{
    m <- 0:n
    S <- 1:(n+1)
    p <- 1:(n+1)
    
    
    u <- exp(r/n + sigma*sqrt(t/n))
    d <- exp(r/n - sigma*sqrt(t/n))
    q <- (exp(r*(t/n)) - d) / (u - d)
    
    for(i in 1:(n+1))
    {
        S[i] <- max(k - s*(u)^(m[i])*(d)^(n - m[i]) , 0)
        p[i] <- dbinom(m[i], n , q)
        
    }
    
    resultat <- 0
    
    resultat <- sum(p*S) * exp(-r*t)
    
    resultat
}

put_p(1, 52, 100, 95, 0.0112, 0.1564)



# Simulation de 5000 

u <- exp(0.0112/52 + 0.1564*sqrt(1/52))
d <- exp(0.0112/52 - 0.1564*sqrt(1/52))
q <- (exp(0.0112/52) - d) / (u - d)

m <- matrix(nrow = 5000, ncol = 53)
m[,1] <- 100
for(i in 1:5000)
{
    for(j in 2:53)
    {
       I <-rbinom(1, 1, q)
       m[i, j] <- m[i, j - 1]*(u^I)*(d^(1 - I))
       
    }
    
}

# Call Asian

moyenne_arithm <- 1:5000

for(i in 1:5000)
{
    moyenne_arithm[i] <- max(mean(m[i,])-110, 0)
}

mean(moyenne_arithm) * exp(-0.0112)

# Put Asian

moyenne_arithm_1 <- 1:5000

for(i in 1:5000)
{
    moyenne_arithm_1[i] <- max(95 - mean(m[i,]), 0)
    
}

mean(moyenne_arithm_1) * exp(-0.0112)

# Put avec Barriere de 105

barriere <- 1:5000
m_1 <- m

for(i in 1:5000)
{
    if(any(m_1[i,] >= 105))
    {
        m_1[i,] <- 0
    }
    
    if(m_1[i,53] != 0)
    {
        m_1[i,53] <- max(95 - m_1[i,53] , 0)
    }
    
}

mean(m_1[,53]) * exp(-0.0112)


###
## Question 3
###

call_1 <- 1:100
put_1 <- 1:100

for(i in 1:100)
{
    call_1[i] <- call_p(1, 52, 100, i + 59, 0.0112, 0.1564)
    put_1[i] <- put_p(1, 52, 100, i + 45 , 0.0112, 0.1564)
    
}

plot(call_1, type="l", col="red", lwd=3, xlab="Strike price", ylab="Call price", main="Variation de prix du Call")
plot(put_1, type="l", col="green", lwd=3, xlab="Strike price", ylab="Put price", main="Variation de prix du Put")


###
## Question 5
###

## Pour le Call 

# B-S
d_1 <- (log(100/110) + (0.0112 + 0.1564^2/2))/0.1564
d_2 <- d_1 - 0.1564
100*pnorm(d_1) - 110*exp(-0.0112)*pnorm(d_2)

# Delta 
pnorm(d_1)

# Gamma
dnorm(d_1)/(100*0.1564)

# Vega
100*dnorm(d_1)/100

# Theta
(-0.0112*110*exp(-0.0112)*pnorm(d_2) - (110*exp(-0.0112)*dnorm(d_2)*0.1564)/2)/365

# Rho
(110*exp(-0.0112)*pnorm(d_2))/100

# Psi
(-100*pnorm(d_1))/100


## Pour le  Put 

# B-S

d1 <- (log(100/95) + (0.0112 + 0.1564^2/2))/0.1564
d2 <- d1 - 0.1564

95*exp(-0.0112)*pnorm(-d2) - 100*pnorm(-d1)

# Delta
-pnorm(-d1)

# Gamma
dnorm(d1)/(100*0.1564)

# Vega
100*dnorm(d1)/100

# Theta
(-0.0112*95*exp(-0.0112)*pnorm(d2) - (95*exp(-0.0112)*dnorm(d2)*0.1564)/2 + 0.0112*95*exp(-0.0112))/365

# Rho
-95*exp(-0.0112)*pnorm(-d2)/100

# Psi
100*pnorm(-d1)/100


