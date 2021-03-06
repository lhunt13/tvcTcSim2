# parameter values

#--------------------------------------------#
# S model (binomial glm)
# intercept, V=none, V=other, cc, L, U, t
#--------------------------------------------#
# linear secular trend
sb <- c(-6.4120244686, -.0487131319, 2.1069529160, -.0033530528, 2.4181326795, -.0001526872, -.0036700691)
sg <- c(-8.989144, -.2018109, 2.021352, -.001353252, 2.72997, .00005017895, -.00512984)
s <- c(sb,sg-sb)

# nonlinear secular trend
sb.sine <- c(-6.4120244686, -.0487131319, 2.1069529160, -.0033530528, 2.4181326795, 1.3 , -.0036700691)
sg.sine <- c(-8.989144, -.2018109, 2.021352, -.001353252, 2.72997, 0, -.00512984)
s.sine <- c(sb.sine,sg.sine-sb.sine)
  
#--------------------------------------------#
# L model (binomial glm)
# intercept, V=none, V=other, cc, L
#--------------------------------------------#
lb <- c(-3.606911083, -0.229221290, -0.231192067, 0.001217097, 8.037740676)
lg <- c(-3.458755, -0.4102037, -0.1493853, 0.00003886894, 8.229348)
l <- c(lb,lg-lb)

#--------------------------------------------#
# V model (multinomial)
# r1 is "none" vs "brand"/"generic", r2 is "other" vs. "brand"/"generic"
# c1 is intercept, c2 is L
#--------------------------------------------#
vb <- matrix(NA,2,2)
vb[1,] <- c(5.0357576, -0.6289020)
vb[2,] <- c(-0.5032673, -0.1250455)
vg <- matrix(NA,2,2)
vg[1,] <- c(4.883411, -0.54845608)
vg[2,] <- c(-1.294050, 0.05590585)
v <- cbind(vb,vg-vb)

#--------------------------------------------#
# D model (multinomial)
# r1 is 15 vs. 1, r2 is 30 vs. 1, etc...
# c1 is intercept, c2 is "other" (no effect of "none" because then D=1 deterministically)
#--------------------------------------------#
db <- matrix(NA,4,2)
db[,1] <- c(1.526492,4.060786,2.134770,1.615714)
db[,2] <- c(-0.20386321,0.08524246, 0.06762563, 0.53257257)
dg <- matrix(NA,4,2)
dg[,1] <- c(1.334763,4.323406,2.553914,2.227432)
dg[,2] <- c(-0.2404519, -0.6021238, -0.7551542, -0.4352028)
d <- cbind(db,dg-db)

#--------------------------------------------#
# C model (linear model)
# intercept, D=15, D=30, D=60, D=90, V="other"
#--------------------------------------------#
cb <- c(3.339590, -0.019814, 0.073039, 0.049527, 0.403011, 0.146056)
cb.sigma <- .6651
cg <- c(2.66949, 0.20714, 0.33877, 0.30702, 0.55730, 0.45174)
cg.sigma <- 0.8614

cpar <- c(cb,cg-cb)
sigma <- c(cb.sigma,cg.sigma-cb.sigma)

#--------------------------------------------#
# baseline and other stuff
#--------------------------------------------#

# global parameters
expit <- function(x) 1/(1 + exp(-x))
q <- seq(0, 1, by = 0.1)
qU <- c(8849, 11211, 12030, 12681, 13441, 14046, 14538, 15023, 15573, 16265, 17166)
ssU <- smooth.spline(q, qU, spar=0.25)
pL1b <- 0.70
pL1g <- 0.30
u_star <- 13369





