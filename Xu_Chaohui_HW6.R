library("ggplot2") #import ggplot2
library("grid") #import grid package
data(diamonds)

#Question 2 Create a simple scatter plot of Weight (???Carat???) and Price using Color (the	
#???Color??? column in the	diamonds dataframe) as a facet. This might the precursor for	
#developing	a	model to predict price given some characteristic like weight. Notice that the	
#relationship is non-linear. If we wanted to build a linear model, this would violate one of the	
#assumptions.
layer1 = ggplot(diamonds, aes(carat, price, color = factor(color)))
dots = layer1 + geom_point() + xlab("Weight") + ylab("Price") + labs(title = "Diamonds - Weight to Price by Color") + theme(plot.title = element_text(color = "blue"))
#create a scatter plot of Weight ("Carat") and Price using Color (the "Color" column in the diamonds dataframe) as a facet. Moreover, set the title color as blue using theme()

#Question 3 All	Groups: Let???s remove the non-linearity and replot. Think about transforming both price	
#and weight. Given that we are working with dollars (Price); the first transformation we	
#might try is the natural log. Add the transformed columns to the dataframe and replot.
cbind(log(diamonds$carat), log(diamonds$price)) #add the transformed columns to the dataframe
layer2 = ggplot(diamonds, aes(log(carat), log(price), color = factor(color)))
dots2 = layer2 + geom_point() + xlab("Weight") + ylab("Price") + labs(title = "Diamonds - Weight to Price (Linear)") + theme(plot.title = element_text(colour = "blue"))
#remove the non-linearity by transforming both price and weight with natural log, and replot.

#Question 4 All Groups: Remove the linear trend (create	a linear model and use the transformed weight	
#on the x-axis and the residuals on the	y-axis). If there is a relationship that can be modeled	
#with	a linear regression model, then the residuals should be randomly distributed. The	
#resulting plot visually indicates there is possibly some heteroscedasticity to manage.
linear_model <- lm(log(price)~log(carat),diamonds) #create a linear model
layer3 = ggplot(diamonds, aes(log(carat),resid(linear_model), color = factor(color)))
dots3 = layer3 + geom_point() + xlab("Weight") + ylab("Price Residuals") + labs(title = "Diamonds - Weight to Price by Color") + theme(legend.position = 'top', plot.title = element_text(colour = "blue")) #make the tile blue

#Question 5 Use the grid package to create the following overlay of three plots. The	
#histogram on the bottom left is a density histogram of the price and the histogram on the	
#upper right is a density histogram of carat. One way to create this plot is using viewports.

#create the main plot
linear_model <- lm(log(price)~log(carat),diamonds)
layer3 = ggplot(diamonds, aes(log(carat),resid(linear_model), color = factor(color)))
dots3 = layer3 + geom_point() + xlab("Weight") + ylab("Price Residuals") + labs(title = "Diamonds - Weight to Price by Color") + theme(plot.title=element_text(colour='blue'),legend.position='top') + guides(col = guide_legend(nrow=1))
#guides() lines up legend into one row

#create the density histogram for price
hist_price <- ggplot(diamonds, aes(price, fill = color)) + geom_histogram(aes(y = ..density..), binwidth = 100) + xlab(NULL) + ylab(NULL) + theme(legend.position = "none")

#create the density histogram for weight
hist_carat <- ggplot(diamonds, aes(carat, colour = color)) + geom_histogram(aes(y = ..density..), binwidth = 0.05) + xlab(NULL) + ylab(NULL) + theme(legend.position = " none")

vp1 <- viewport(x = 0.25, y = 0.16, width = 0.4, height = 0.2) #find the postion for hist_price 
vp2 <- viewport(x = 0.80, y = 0.77, width = 0.4, height = 0.2) #the position for hist_carat
#plot hist_price and hist_carat over the main plot dots3
print(dots3)
print(hist_price, vp = vp1)
print(hist_carat, vp = vp2)




