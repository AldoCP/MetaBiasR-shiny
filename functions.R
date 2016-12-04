
logL0 <-
function(mu, y, sig, sig_prior=2){
  a <- 0.5 * (sum(1/sig**2) + 1/sig_prior**2)
  b <- -sum(y/sig**2)
  c <- 0.5*sum(y**2/sig**2) + sum(log(sig)) + log(sig_prior) + 0.5*(length(y)+1)*log(2*pi)
  (-(a*(mu**2) + b*mu + c)) }

logL <-
#function(mu, eta, y, sig, sig_prior=2, Z=1.96){
function(mu, eta, y, sig, sig_prior=2, Z=1.96, alpha=5, beta=5){#added alpha and beta
  logdensity <- logL0(mu=mu, y=y, sig=sig, sig_prior=sig_prior)
  SS <- (abs(y) - sig*Z) > 0 #SS: Statistically significant
norm_f <- function(mu, sig, Z=Z) {1/(1 -pnorm(sig*Z, mean=mu, sd=sig) +pnorm(-sig*Z, mean=mu, sd=sig))}# Normalization constant: f(sig)
  f <- norm_f(mu=mu, sig=sig[SS], Z=Z)
  modif <- sum(log(eta + (1-eta)*f )) + sum(SS==F)*log(eta)
#  logdensity + modif}
  logdensity + modif + dbeta(mu, alpha, beta)} #added dbeta

nullmass <-
function(y, sig, sig_prior=2){
  Fn0 <- function(arg) exp(logL0(arg, y=y, sig=sig, sig_prior=sig_prior))
a <- 0.5 * (sum(1/sig**2) + 1/sig_prior**2)
b <- -sum(y/sig**2)
var_ <- 1 / (2*a)
mean_ <- -var_*b
sig_ <- sqrt(var_)
  ans <- integrate(Fn0, lower=mean_-10*sig_, upper=mean_+10*sig_)$value
  ans}

mass <-
#function(y, sig, sig_prior=2, Z=1.96){
function(y, sig, sig_prior=2, Z=1.96, alpha=5, beta=5){
#  Fn <- function(arg1, arg2) exp(logL(arg1, arg2, y=y, sig=sig, sig_prior=sig_prior, Z=Z))
  Fn <- function(arg1, arg2) exp(logL(arg1, arg2, y=y, sig=sig, sig_prior=sig_prior, Z=Z, alpha=alpha, beta=beta))
  Fn2 <- function(x) Fn(x[1], x[2])#"arg1" stands for "mu"; "arg2" stands for "eta"
a <- 0.5 * (sum(1/sig**2) + 1/sig_prior**2)
b <- -sum(y/sig**2)
var_ <- 1 / (2*a)
mean_ <- -var_*b
sig_ <- sqrt(var_)
  ans <- cubature::adaptIntegrate(Fn2, c(mean_-10*sig_, 0), c(mean_+10*sig_, 1))$integral
  ans}

BFbias <-
#function(y, sig, sig_prior=2, Z=1.96){
function(y, sig, sig_prior=2, Z=1.96, alpha=5, beta=5){
ans <- mass(y, sig, sig_prior=sig_prior, Z=Z, alpha=alpha, beta=alpha)/nullmass(y, sig, sig_prior=sig_prior)
#ans <- mass(y, sig, sig_prior=sig_prior, Z=Z)/nullmass(y, sig, sig_prior=sig_prior)
ans}


#plotBFbias <- function(y, sig, sig_prior_min=1e-2, sig_prior_max=1e2, Z=1.96){
plotBFbias <- function(y, sig, sig_prior_min=1e-2, sig_prior_max=1e2, Z=1.96, alpha=5, beta=5){
	sigma_prior <- exp(seq(log(sig_prior_min), log(sig_prior_max), length.out = 40))
#	print("Generating datapoints...");flush.console()
	BF <- rep(NA, length(sigma_prior))
#	for(x in seq_len(length(sigma_prior))) BF[x] <- BFbias(y, sig, sig_prior=sigma_prior[x], Z=Z)
	for(x in seq_len(length(sigma_prior))) BF[x] <- BFbias(y, sig, sig_prior=sigma_prior[x], Z=Z, alpha=alpha, beta=beta)
	df <- as.data.frame(cbind(sigma_prior, BF))
#	print("Plotting...");flush.console()
plot0 <- ggplot(df, aes(sigma_prior, BF)) + geom_smooth() + 
#scale_x_log10(bquote(''*sigma[prior]~''), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
#scale_y_log10("Bayes Factor: p( MixModel | data ) / p( Null | data )", breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)) +
scale_y_log10("Bayes Factor", breaks = trans_breaks("log10", function(x) 10^x)) +
geom_hline(yintercept=1, linetype="dashed", color="red") +
annotate("text", x=quantile(sigma_prior, 0.2), y=1.5, label="BF > 1: evidence of MixModel (bias)") + 
annotate("text", x=quantile(sigma_prior, 0.8), y=min(BF)*2, color="purple", label=paste("Minimum value of BF:", format(min(BF),scientific=T, digits=3)) )

#plot0
	}


#plotL <- function(y, sig, sig_prior=2, Z=1.96, n_elem=80){
plotL <- function(y, sig, sig_prior=2, Z=1.96, alpha=5, beta=5, n_elem=80){
	a <- 0.5 * (sum(1/sig**2) + 1/sig_prior**2)
	b <- -sum(y/sig**2)
	var_ <- 1 / (2*a)
	mean_ <- -var_*b
	sig_ <- sqrt(var_)
mu_value <- rep(seq(mean_-7*sig_, mean_+7*sig_, length.out=n_elem), n_elem)
eta_value <- rep(seq(0, 1, length.out=n_elem), each=n_elem)
results <- as.data.frame(cbind(mu_value, eta_value)); results$L <- NA

#for(k in 1:nrow(results)){ results[k,"L"] <- exp(logL(mu_value[k], eta_value[k], y=y, sig=sig, sig_prior=sig_prior, Z=Z))}
for(k in 1:nrow(results)){ results[k,"L"] <- exp(logL(mu_value[k], eta_value[k], y=y, sig=sig, sig_prior=sig_prior, Z=Z, alpha=alpha, beta=beta))}

#results <- matrix(NA, n_elem, n_elem)
#	for(k in 1:n_elem){
#		for(j in 1:n_elem){results[k,j] <- exp(logL(mu_value[k], eta_value[j], y=y, sig=sig, sig_prior=sig_prior, Z=Z))}
#	}

pl <- ggplot(results, aes(x=mu_value, y=eta_value, z=L, fill=L)) + geom_tile() + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + scale_fill_distiller(palette = "Spectral")

}


