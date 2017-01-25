
### Kristin Meier
### R script for MSiA 400 Lab 1 
### Excersise 1 Problem 2
### 10/16/2016

# Write your own one-way ANOVA function in R which takes two vectors
# (i.e. data and group labels) 
# as input and the F-test result (e.g. reject null hypothesis or not) as output. 
  # calculate data for anova
  # calculate anova
  # return results

library(plyr)

ex1_anova <- function(ydata = c(1,2,3,4,5,6,7,8),
                      groupdata = c("a","b","c"),
                      alpha = 0.05){
                      
 ## inputs are the data and group labels or group label data
 ## if only the group labels are entered they need to be replicated
 ## assume this is the case if vector length is shorter than data
 if(length(groupdata) < length(ydata)){
   
   n.groups <- length(groupdata)
   n.y <- length(ydata)
   groupdata <- gl(n.groups, 1, length = n.y)
   
   }

  ############################################################
  # Create data frame and calc all data needed for anova table
  ############################################################

    # make data frame
    aovdt <- data.frame(groupdata,ydata)
    aovdt$group <- as.factor(aovdt$group)
    # total obs mean
    meany <- mean(aovdt$ydata)
    # total # groups
    grouplevels <- nlevels(aovdt$group)
    # total # obs
    n <- nrow(aovdt)
    
    # calculate necessary info by group
    # mean, num. observations
    groupdt <- ddply(aovdt, .(group), summarize, 
                      mean=mean(ydata), 
                      count=length(ydata))
    # group mean - tot mean
    groupdt$facmean_mean <- groupdt$mean - meany
    
    # merge group mean to obs data
    aovdt <- merge(aovdt,groupdt[c("group","mean")],by="group",all.x=T)
    
    
    ############################################################
    # Calculate anova table
    ############################################################
    
    # degrees of freedom
    df.treat <- grouplevels - 1
    df.tot <- n - 1
    df.res <- df.tot - df.treat
    
    # ss
    ss.treat <- sum(((groupdt$facmean_mean)^2)*groupdt$count)
    ss.res <- sum((aovdt$ydata - aovdt$mean)^2)
    ss.tot <- ss.treat + ss.res
    
    # ms
    ms.treat <- ss.treat/df.treat
    ms.res <- ss.res/df.res
    
    # f value
    f <- round(ms.treat/ms.res,4)
    # reject if Fstat > f_r,n-(p+1),alpha
    fcritical <- qf((1-alpha), df1=df.treat, df2=df.res) 
    # pval
    pvalue <- round(1-pf(f,df.treat,df.res),4)
    
  ############################################################
  # Return results
  ############################################################

    # reject or not
    if(f > fcritical){
      print(paste("The null hypothesis is rejected at the ", (1-alpha)*100, 
                  "% significance level. The p-value is ", pvalue,". The F-value is ", f,".",sep=""))
    }else{
      print(paste("The null hypothesis cannot be rejected at the ", (1-alpha)*100, 
                  "% significance level. The p-value is ", pvalue,". The F-value is ", f,".",sep=""))
    }

    
}
  
