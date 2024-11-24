table.stats <- function(contable) {  ## This function requires observations and
	## predictions, using 1s and 0s as your binary
	## operators.

	#contable <- con.table(y,y.hat)  ## This is a separate function required by table.stats

	aa <- contable[1,1]
	bb <- contable[1,2]
	cc <- contable[2,1]
	dd <- contable[2,2]

	## We should create a vector of labels so we know what we are viewing.

	labels <- c("PC","CSI","BIAS","FAR","POD","POFD","HSS","TSS")

	## Now, lets compute each of these contingency stats.

	PC <- (aa + dd) / sum(aa,bb,cc,dd)  ## Compute the percent correct
	CSI <- (aa) / (aa+bb+cc)     ## Compute the critical success index
	BIAS <- (aa + bb)/(aa+cc)	   ## Compute the bias of the data
	FAR <- bb/(aa+bb)			   ## Compute the false alarm ratio
	POD <- aa/(aa+cc)            ## Compute the probability of detection
	POFD <- bb/(bb+dd) 	     ## Compute the probability of false detection
	HSS <- (2*(aa*dd-bb*cc))/((aa+cc)*(cc+dd)+(aa+bb)*(bb+dd))
	TSS <- (aa*dd-bb*cc)/((aa+cc)*(bb+dd))

	## We can combine all of these into two separate vectors and then output
	## our results.

	contin.stats <- c(PC,CSI,BIAS,FAR,POD,POFD,HSS,TSS)
	output <- data.frame(cbind(labels,contin.stats))

	return(output)

} ## End the function

stepwise <- function(y,x,method="forward") {

	## This function will conduct a stepwise regression either based off of
	## forward or backward selection.  The selection is based off of the
	## F statistic.  It requires the anova.new function provided in class.

	## We need to create an anova.new function that is compatible with this
	## function.  It is built in here:

	## First, we assume that our initial y.intercept is simply the mean of y, and
	## that we have no other variables.  Next, we need to determine the
	## individual capabilities of the different variables, so we will compute
	## their individual linear regressions.


	max.F <- 0
	dims <- dim(x)
	size <- dims[2] * 4
	positions <- c(1:dims[2])
	bestpreds <- matrix(numeric(size),ncol=4)
	#best.x <- rep(1,dims[1])

	if (method=="forward") { ## For the forward selection of predictors

		for (j in 1:dims[2])  { ## Big for loop for all of the variables
			m <- dims[2]-(j-1)
			max.F <- 0
			for (i in 1:m) { ## Begin the for loop for each step

				if (j < dims[2]) {testvec <- x[,i]}
				if (j == dims[2]) {testvec <- x}
				if (j == 1) {
					predictor.matrix <- testvec
					df.MSR <- 1
					df.MSE <- length(predictor.matrix)-2

				} else { ## Begin the else statement

					predictor.matrix <- cbind(best.x,testvec)
					df.MSR <- dim(predictor.matrix)[2]-1
					df.MSE <- dims[1]-dim(predictor.matrix)[2]

				} ## End the else statement

				test.model <- lm(y~predictor.matrix)
				F.stat <- summary(test.model)$fstatistic[1]

				if (F.stat > max.F) {
					best.pred <- positions[i]
					best.i <- i
					max.F <- F.stat
					#p.value <- 1 - pf(F.stat,df.MSR,df.MSE)
					MSE <- sum(test.model$resid^2)/df.MSE
					r.squared <- summary(test.model)$r.squared

				} ## End the if statement

			} ## End the for loop for each step

			bestpreds[j,1] <- best.pred
			bestpreds[j,2] <- round(MSE,3)
			bestpreds[j,3] <- round(r.squared,3)
			bestpreds[j,4] <- round(max.F,3)
			#bestpreds[j,5] <- round(p.value,3)

			if (j==1) {
				addedx <- x[,best.i]
				best.x <- addedx
				x <- x[,-best.i]
				positions <- positions[-best.i]
			}

			if (j!=1 && j != dims[2]) {
				addedx <- x[,best.i]
				best.x <- cbind(best.x,addedx)
				x <- x[,-best.i]
				positions <- positions[-best.i]
			}

			if (j==dims[2]) {
				addedx <- x
				best.x <- cbind(best.x,addedx)
			}

		} ## End the big for loop

	} ## End the forward selection section

	## Now for the backward selection section

	if (method=="backward") { ## Begin the backward selection if statement

		for (j in 1:dims[2]) { ## Begin the for loop for the x parameters
			m <- dims[2] - (j-1)
			max.F <- 0


			for (i in 1:m) { ## Begin the loop for each individual model
				if (m > 1) { ## If test for predictor matrix
					predictor.matrix <- x[,-i]
				} else { ## Predictor matrix
					predictor.matrix <- x
				} ## End if else

				if (dim(cbind(predictor.matrix,predictor.matrix))[2]==2) {
					df.MSR <- 1
					df.MSE <- dims[1]-2
				} else { ## Else statement for if test of dim or length
					df.MSR <- dim(predictor.matrix)[2]
					df.MSE <- dims[1]-dim(predictor.matrix)[2]
				}

				## We need to remove a column to try to find the best setup

				test.model <- lm(y~predictor.matrix)
				F.stat <- summary(test.model)$fstatistic[1]

				if (F.stat > max.F) { ## F.stat if test
					best.pred <- positions[i]
					best.i <- i
					max.F <- F.stat
					MSE <- sum(test.model$resid^2)/df.MSE
					r.squared <- summary(test.model)$r.squared

				} ## F.stat end if

			} ## End the for loop for the individual tests

			if (j == 6) {
				test.model <- lm(y~1)

				df.MSE <- dims[1] - 1
				MSE <- sum(test.model$resid^2)/df.MSE
			}

			bestpreds[j,1] <- best.pred
			bestpreds[j,2] <- round(MSE,3)
			bestpreds[j,3] <- round(r.squared,3)
			bestpreds[j,4] <- round(max.F,3)

			if (j < dims[2]) {
				x<- x[,-best.i]
				positions <- positions[-best.i]
			}

		} ## End the for loop for the x parameters

	} ## End the backward selection if statement

	colnames <- c("Var #","MSE","R-squared","F-stat")
	rownames <- bestpreds[,1]
	output <- rbind(colnames,bestpreds)
	output <- as.data.frame(output,row.names=rownames)
	return(output)

} ## End the function

skewness<-function(x) {
	mean.x<-mean(x)
	n<-length(x)
	output<-(sum((x-mean.x)^3)/n)/(sum((x-mean.x)^2)/n)^(3/2)

	return(output)
}



reinterpolate <- function(datavec,oldlat,oldlon,newlats,newlons) {

	# Newlat is a vector of gridded latitudes
	# Newlon is a vector of gridded longitudes

	newlength <- length(newlats)*length(newlons)

	d2r <- pi/180

	delta.n <- 300000
	kappa.const <- 5.052
	kappa <- kappa.const * (2*delta.n/pi)^2

	newdata <- numeric(newlength)
	newlat <- rep(newlats,each=length(newlons))
	newlon <- rep(newlons,times=length(newlats))

	for (m in 1:newlength) {

		distances <- numeric(length(data))
		weights <- numeric(length(data))

		gcd.1 <- sin((d2r*newlat[m] - d2r*oldlat)/2)^2
		gcd.2 <- cos(d2r*oldlat)*cos(d2r*newlat[m])*sin((d2r*oldlon-d2r*newlon[m])/2)^2
		distances <- 2*6371000*asin(sqrt(gcd.1 + gcd.2))

		# Now compute the weights
		weights <- exp(-(distances^2)/kappa)

		newdata[m] <- sum(weights*datavec)/sum(weights)

	}          # End for the new longitudes
	return(newdata)

}   # End the function




predictlm<-function(model,inputdata) {

	junk<-cbind(inputdata,inputdata) # Need this step to find the row
	# dimensionality regardless of if
	# it is a vector or matrix.
	dims<-dim(junk)


	if (dims[2] == 2) {  # Perform the prediction for a single input sample
		ones <- rep(1,dim(junk)[1])
		inputdata<-cbind(ones,inputdata)
		predictions <- inputdata %*% model$coefficients

	} else {  # End the prediction if for a single input statement

		ones<-rep(1,dim(junk)[1])

		inputdata<-cbind(ones,inputdata)

		predictions<-inputdata %*% model$coefficients

	} # End the if statement for multiple inputs

	return(predictions)  # Return the predictions

}


plot.weathermap <- function(x,y,dataset,filled=FALSE,interval=100)  {

	## This simple function will plot weather data on a map
	## of the world. It will zoom to the particular region
	## you desire and plot the weather data there.  You can
	## only plot a single variable in this version, although
	## it is possible to plot additional weather data manually.

	## Data must be a matrix that has the same structure as the
	## grid of the x,y data, with the longitudes as columns.
	## For example, if you have a 15 longitude by
	## 10 latitude grid, you would need a 10X15 data matrix to use as
	## input.

	## This function requires the fields package.

	## levels is a vector of desired contour levels that the user can input.
	## Without it specified, the function will use the default values.

	## Check to be sure the dimensions are correct.
	par(cex=1.25)
	#require('fields')
	require('maps')
	if (length(x) != dim(dataset)[2] || length(y) != dim(dataset)[1]) { ## Check for errors in the data dimensions.
		stop("The dimensions are incorrect.  You may want to try the transpose.")
	} else {

		key.axis <- pretty(range(dataset),round((max(dataset)-min(dataset))/interval))

		#    ## Set up window margins
		#    marg.1 <- (range(y)[2]-range(y)[1])/1.375
		#    marg.2 <- (range(x)[2]-range(x)[1])/1.75
		#    x11(marg.2,marg.1)

		par(mar=c(5,5,4,2)+0.1)
		dev.new(width=length(x)/9,height=length(y)/5)

		if (filled==TRUE) { ## shaded is true
			par(mai=c(1.1,1.1,1.1,1.75)) ## Make room for color bar on right side
			rgbpalette <- colorRampPalette(c("blue", "green", "yellow","orange","red"),space = "rgb")
			image(x,y,t(dataset),col=rgbpalette(100),xlab="Longitude (E)",ylab="Latitude (N)")
			image.plot(legend.only=T,zlim=range(dataset),col=rgbpalette(100))
			map(add=T)
			try(map('state',add=TRUE),silent=T)

		} else { ## end if shaded is true
			par(mai=c(1.1,1.1,1.1,1.1))
			contour(x,y,t(dataset),levels=key.axis,labcex=1.2,col="black",xlab="Longitude (E)",ylab="Latitude (N)",lwd=1)
			map(add=T)
			try(map(database='state',add=TRUE),silent=T)
			box()
		} ## End the else statement

	} ## End the error checking else statement



} ## End the function


plot.ci <- function(x,barwidth=0.1,min.y=NULL,max.y=NULL,xlabel=NULL,ylabel=NULL) {
	## This function expects a matrix with 3 rows and a number
	## of columns corresponding to the number of values you're comparing
	## The function assumes the first row is the lower limit, the middle
	## row is the median, and the top row is the upper limit
	if (barwidth > 0.5) {
		warning("Warning:  Error bars will overlap unless you reduce barwidth")
	}

	n.plots <- length(x[1,])

	## Determine maximum and minimum values of x for the plotting routine.
	## The function figures this out if it is not specified.

	if (is.null(min.y) && is.null(max.y)) {
		min.y <- min(x)-2*sd(x[2,])
		max.y <- max(x)+2*sd(x[2,])
	}

	plot(x[2,],xlab=xlabel,ylab=ylabel,xlim=c(0,dim(x)[2]+1),ylim=c(min.y,max.y),pch=16,axes=F)

	for (i in 1:n.plots) {
		lines(c(i-barwidth,i+barwidth),c(x[1,i],x[1,i]))
		lines(c(i-barwidth,i+barwidth),c(x[3,i],x[3,i]))
		lines(c(i,i),c(x[1,i],x[3,i]))

	}

} ## End function plot.ci


permutationTestMeans <- function(x,y,B=2000) {
	n1 <- length(x)
	n2 <- length(y)
	delta <- abs(mean(x) - mean(y))
	fullset <- c(x,y)
	new.delta <- numeric(B)
	for (i in 1:B) {
		sample.1 <- sample(fullset,n1,replace=T)
		sample.2 <- sample(fullset,n2,replace=T)
		new.delta[i]<- abs(mean(sample.1) - mean(sample.2))
	}

	counts <- ifelse(new.delta >= delta,1,0)
	p.value <- sum(counts) / B
	return(p.value)
}



Mode <- function(x) {

	# This function will compute the mode of a vector of data

	mode.x <- names(sort(-table(x)))[1]
	no.mode <- ifelse(sort(-table(x))[1]==sort(-table(x)),1,0)
	if (sum(no.mode) == length(x)) {
		return("There is no mode.")
	} else if (sum(no.mode) > 1) {
		mode.x <- names(no.mode[no.mode==1])
		warning("There are multiple modes in this dataset.")
	}
	mode.x <- as.double(mode.x)
	return(mode.x)

} ## End the function


limits.ci <-function(x,alpha=0.05) {  # This function will create CIs for
	# the mean of x

	n <- length(x)# Need this to compute the CIs.

	if (n <=100) {

		lower.ci <- mean(x) + qt(alpha/2,n-1)*(sd(x)/sqrt(n))
		upper.ci <- mean(x) + qt(1-alpha/2,n-1)*(sd(x)/sqrt(n))

	} else { # End the if statement for the t-distribution

		lower.ci <- mean(x) + qnorm(alpha/2)*(sd(x)/sqrt(n))
		upper.ci <- mean(x) + qnorm(1-alpha/2)*(sd(x)/sqrt(n))

	}# End the else statement for the z-distribution

	output<-data.frame(lower.ci,upper.ci)

	return(output)

}


kurtosis<-function(x) {
	mean.x<-mean(x)
	n<-length(x)
	output<-(sum((x-mean.x)^4)/n)/(sum((x-mean.x)^2)/n)^2

	return(output)
}


con.table <- function(observed,predicted) {
	aa <- 0	## I called these double letters because single c is a function in R.
	bb <- 0
	cc <- 0
	dd <- 0

	for (i in 1:length(observed)) { ## Lets first create our contingency matrix

		if (observed[i] == 1 && predicted[i] == 1) { # Test for correct yes forecasts
			aa<-aa+1
		}  # End the correct yes forecasts test

		if (observed[i] == 0 && predicted[i] == 1) { # Test for incorrect yes forecasts
			bb<-bb+1
		}  # End the incorrect yes forecasts test

		if (observed[i] == 1 && predicted[i] == 0) { # Test for incorrect no forecasts
			cc<-cc+1
		}  # End the incorrect no forecasts test

		if (observed[i] == 0 && predicted[i] == 0) { # Test for correct no forecasts
			dd<-dd+1
		}  # End the correct no forecasts test

	} ## End the for loop

	# Now we can simply put this together in a nice contingency matrix.

	contingency.matrix <- matrix(c(aa,bb,cc,dd),ncol=2,byrow=T)

	# Lets return our contingency matrix.

	return(contingency.matrix)

} ## End the function


congruence <- function(cor.matrix, load.matrix)  {  ## Begin the congruence coefficient function

	## Remember that cor.matrix is the first correlation matrix
	## and load.vector is a loading vector.

	m <- dim(load.matrix)[1]
	n <- dim(load.matrix)[2]

	conoutput <- numeric(n)

	for (j in 1:n)  { ## Begin computing congruence over all loading columns

		load.vector <- load.matrix[,j]
		testvalue <- max(abs(load.vector))

		bestcolumn <- 0

		for (i in 1:m)  { ## Check for the largest absolute loading value

			if (abs(load.vector[i]) ==  testvalue) { ## If test for testvalue
				bestcolumn <- i
			} ## End the if

		}  ## End the for


		conoutput[j] <- sum(cor.matrix[,bestcolumn] * load.vector)/sqrt(sum(cor.matrix[,bestcolumn]^2) * sum(load.vector^2))

	} ## End the big for loop

	return(conoutput)

} ## End the total congruence function

chi.square <- function(dataset,dataset2=NULL,dist.name="norm",n.bins,dist.params=c(0,1)) {
	set.seed(5)  ## Set random number seed to ensure repeated values
	vec.breaks <- seq(min(dataset),max(dataset),by=(max(dataset)-min(dataset))/n.bins)
	vec.breaks2 <- vec.breaks
	vec.counts <- hist(dataset,breaks=vec.breaks,plot=F)$counts
	vec.probs <- vec.counts/length(dataset)
	len.data <- length(dataset)
	dist.param.list <- NULL

	## If comparing a dataset to a particular distribution specified by dist.name
	if(is.null(dataset2)) {

		for (k in 1:length(dist.params))  {
			if (k==1) {
				dist.param.list <- paste(dist.params[k],sep='')
			} else {
				dist.param.list <- paste(dist.param.list,",",dist.params[k],sep='')
			}
		}
		random.cmd <- paste('comparison <- r',dist.name,'(10000,',dist.param.list,')',sep='')
		eval(parse(text=random.cmd))

		if (min(comparison) < min(dataset))  ## Fix the end points of the breaks vector
			vec.breaks2[1] <- min(comparison)

		if (max(comparison) > max(dataset))  ## Fix the end points of the breaks vector
			vec.breaks2[n.bins+1] <- max(comparison)

		vec.counts2 <- hist(comparison,breaks=vec.breaks2,plot=F)$counts
		vec.probs2 <- vec.counts2/10000

		chi.square.stat <- sum(((vec.counts-(len.data*vec.probs2))^2)/(len.data*vec.probs2))
		if (is.na(chi.square.stat)) {
			stop("Check your distribution parameters or n.bins, these definitely do not match")
		}
		n.dist.params <- length(dist.params)

	} else {  ## End if for is.null(dataset2)

		## This section allows you to compare one dataset to another directly, regardless of their distribution

		if (min(dataset2) < min(dataset))
			vec.breaks2[1] <- min(dataset2)
		if (max(dataset2) > max(dataset))
			vec.breaks2[n.bins+1] <- max(dataset2)

		vec.counts2 <- hist(dataset2,breaks=vec.breaks2,plot=F)$counts
		vec.probs2 <- vec.counts2/length(dataset2)

		chi.square.stat <- sum(((vec.counts)-len.data*(vec.probs2)^2)/(len.data*vec.probs2))

		if (chi.square.stat==Inf) {
			stop("You have zero probabilities in dataset2.  Your chi-square test won't work on these data.")
		}

		if (is.na(chi.square.stat)) {
			stop("Check your datasets or n.bins; these definitely do not match")
		}
		n.dist.params <- 0

	} ## End else statement for if you have two datasets to compare

	df.val <- n.bins - n.dist.params - 1
	p.value <- pchisq(chi.square.stat,df.val)
	return(p.value)


} ## End chi.square test

brier <- function(obs,pred) {
	bs.pred <- (1/length(obs))*sum((obs-pred)^2)
	climo.prob <- sum(obs)/length(obs)
	bs.climo <- (1/length(obs))*(sum((obs-climo.prob)^2))
	bss <- 1-(bs.pred/bs.climo)
	return(c(bs.pred,bs.climo,bss))
}


autocor.lag <- function(x,lags) {
	## This function expects a vector and will lag the data by
	## the value lags.  It can be positive or negative.
	if(lags > 0) {
		lag.x <- c(x[(1+lags):length(x)])
	}
	if(lags < 0) {
		lag.x <- c(x[1:(length(x)+lags)])
	}
	return(lag.x)
}
