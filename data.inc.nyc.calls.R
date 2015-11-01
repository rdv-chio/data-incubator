options(digits=10)
#What fraction of complaints are associated with the 2nd most popular agency?

which.max(table(nyc311calls$Agency))
#HPD 
# 34
table(nyc311calls$Agency)[34]/dim(nyc311calls)[1]
#[1] 0.3538337998

#What is the distance (in degrees) between the 90% and 10% percentiles of degrees latitude?
quantile(nyc311calls$Latitude,probs=c(.9),na.rm=T)-quantile(nyc311calls$Latitude,probs=c(.1),na.rm=T)
#        90% 
#0.235790831

#What is the difference between the expected number of calls received during the most and least popular whole hours of the day? (Remove points which do not seem to accurately reflect the actual time they were reported.)


get_hour <- function(x)
{
	hour<-as.numeric(substr(strsplit(x," ")[[1]][2],1,2))
	if (strsplit(x," ")[[1]][3] == "PM")
	{
		hour=hour+12
	}
	return(hour)
}

#What is the most 'surprising' complaint type when conditioned on a borough? That is, what is the largest ratio of the conditional probability of a complaint type given a specified borough divided by the unconditioned probability of that complaint type?

#Let's estimate the area that 311 supports. Suppose calls are 2D normally distributed on the surface of the earth with mean and standard deviation given by those of the latitude and longitude. How many square kilometers is the single-standard-deviation ellipse?

#What is the standard deviation in seconds of the time between consecutive calls? (Remove points which do not seem to accurately reflect the actual time they were reported.)