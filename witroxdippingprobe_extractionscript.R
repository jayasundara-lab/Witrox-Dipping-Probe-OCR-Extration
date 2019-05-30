## Witrox Dipping Probe Data Extraction

#packages require: ggplot2, here

library(ggplot2)
library(here)

#Create an Rproject file on your desktop (via RStudio's file/newproject), and place all files for analysis in that folder
#This allows the here package to get all your files without having to deal with pathlengths

#Remove all objects from your environment via Rstudio's session/restart R

##Extracting Data 

#importing table as a dataframe, skipping the first 400 rows of witrox info and habituation
#remember to put your file into the Rproj, and change the file name to either "mytable" 
#or change "mytable" in the script to the name of your data file

salmon <- read.table(here("mytable.txt"), skip = 400)

#extracting our columns of concern

salmonplot <- salmon[, c(2,8)]
names(salmonplot) <- c("Time", "OCR")

#adding a number label column

salmonplot$second <- 1:nrow(salmonplot) 

#now for the scatterplot and linear regression

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "blue") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

fit1 <- lm(OCR ~ second, data = salmonplot)
ggplotRegression(fit1)

#saving  the plot as an image in your rproject (remember to rename according to your organization scheme!)

ggsave(here("salmonplot.png"))

#now swipe that slope and pop it into an excel sheet
#Remember: this is oxygen consumption rate in mg/L/sec, and you want it to be in mg/hr/embryo
#once you have that slope in excel, multiply it by 3600 (convert to hours) and divide by 0.086 L 
#(amount of water in respirometer chamber)
#also you can get rid of the negative sign

#end script
