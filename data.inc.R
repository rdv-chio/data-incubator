options(digits=10)
#generate data
#save sample data
data<-list()
for(i in 1:1000000)
{
	#initialize the path
	path<-matrix(data = NA, nrow = 1001, ncol = 2)
	#initialize the current position in the matrix in Broadway Ave and Broadway St (0,0)
	path[1,]<-current<-c(0,0)
	for(j in 2:1001) 
	{
		#decide whether I am turning N/S or E/W
		position<-sample(c(1,2),1)
		#decide whether I am advancing or retreating and taking a step
		current[position]<-current[position]+sample(c(1,-1),1)
		path[j,]<-current
	}
	data[[i]]<-path
	print(i)
}

#What is the probability that the tourist is at least 3 city blocks (as the crow flies) from Broadway and Broadway after 10 moves?
count.final.10<-0
for (i in 1:1000000)
{
	if (abs(data[[i]][11,1])>=3 | abs(data[[i]][11,2])>=3)
	{
		count.final.10<-count.final.10+1
	}
}
count.final.10/1000000
#[1] 0.452853

#What is the probability that the tourist is ever at least 5 city blocks (as the crow flies) from Broadway and Broadway within 10 moves?
count.path.10<-0
for (i in 1:1000000)
{
	for (j in 6:11) #need at least 5 moves
	{
		if (abs(data[[i]][j,1])>=5 | abs(data[[i]][j,2])>=5)
		{
		count.path.10<-count.path.10+1
		}
	}
}
count.path.10/11000000
#[1] 0.02063981818

#What is the probability that the tourist is at least 10 city blocks (as the crow flies) from Broadway and Broadway after 60 moves?
count.final.60<-0
for (i in 1:1000000)
{
	if (abs(data[[i]][61,1])>=10 | abs(data[[i]][61,2])>=10)
	{
		count.final.60<-count.final.60+1
	}

}
count.final.60/1000000
#[1] 0.157293

#What is the probability that the tourist is ever at least 10 city blocks (as the crow flies) from Broadway and Broadway within 60 moves?
count.path.60<-0
for (i in 1:1000000)
{
	for (j in 11:61) #need at least 10 moves
	{
		if (abs(data[[i]][j,1])>=10 | abs(data[[i]][j,2])>=10)
		{
			count.path.60<-count.path.60+1
		}
	}
}
count.path.60/61000000
#[1] 0.0473515082


#What is the probability that the tourist is ever east of East 1st Avenue but ends up west of West 1st Avenue in 10 moves?
east2west.10<-0
denominator<-0
for (i in 1:500000)
{
	for (j in 1:991)
	{
		if (data[[i]][j,2] < 0 & 0 < data[[i]][j+10,2])
		{
			east2west.10<-east2west.10+1
		}
		denominator<-denominator+1
	}
}
east2west.10/denominator
#[1] 0.01524501
#denominator
#[1] 495500000

#What is the probability that the tourist is ever east of East 1st Avenue but ends up west of West 1st Avenue in 30 moves?
east2west.30<-0
denominator<-0
for (i in 1:500000)
{
	for (j in 1:941)
	{
		if (data[[i]][j,2] < 0 & 0 < data[[i]][j+30,2])
		{
			east2west.30<-east2west.30+1
		}
		denominator<-denominator+1
	}
}
east2west.30/denominator
#[1] 0.03421364
#denominator
#[1] 470500000

#What is the average number of moves until the first time the tourist is at least 10 city blocks (as the crow flies) from Broadway and Broadway.
first.10<-numeric()
for (i in 1:500000)
{
	for (j in 11:1001)
	{
		if (abs(data[[i]][j,1])>=10 | abs(data[[i]][j,2])>=10)
		{
			first.10[i]<-j
			break
		}
		if (j == 1001)
		{
			first.10[i]<- -1
		}
	}
	print (i)
}
mean(first.10[which(first.10!=-1)])
#[1] 118.7245652

#What is the average number of moves until the first time the tourist is at least 60 city blocks (as the crow flies) from Broadway and Broadway.
first.60<-numeric()
for (i in 1:500000)
{
	for (j in 61:1001)
	{
		if (abs(data[[i]][j,1])>=60 | abs(data[[i]][j,2])>=60)
		{
			first.60[i]<-j
			break
		}
		if (j == 1001)
		{
			first.60[i]<- -1
		}
	}
	print (i)
}
mean(first.60[which(first.60!=-1])
#[1] 826.0125735