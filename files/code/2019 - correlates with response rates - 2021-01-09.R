#This file is best used within R Studio
# rbadgett@kumc.edu
### Start =======================================
version
citation(package = "base", lib.loc = NULL, auto = NULL)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#setwd("../plots")
##* Graphics --------------------------
#windows(600, 600, pointsize = 12) # Size 600x600
getwd()
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
old.par <- par(no.readonly=T)
plot.new()
xmin <- par("usr")[1] + strwidth("A")
xmax <- par("usr")[2] - strwidth("A")
ymin <- par("usr")[3] + 1.2*strheight("A")
ymax <- par("usr")[4] - strheight("A")
##* Libraries------------------------------------
library(openxlsx) # read.xlsx

##* Constants declaration -------------------
`%notin%` <- Negate(`%in%`)
LF <- "\n"
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
KUCrimson = "#e8000d"
KUYellow = "#ffc82d"
current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE))
(current.date.pretty <- as.character(strftime (Sys.time(), format="%m/%d/%Y", tz="", usetz=FALSE)))
p.value <- sprintf(p.value, fmt='%#.3f')
res <- NULL
res$I2 <- 30.123
I2.value <- formatC(res$I2, digits=1, format="f")
I2.label <- bquote(I^2  ~ "%")
I2.label <- bquote(I^2 ~ "=" ~ .(I2.value) ~ "%")
I2.label
text(0.25,0.25,I2.label)
I2.summary.label <- bquote("RE Model ("~ I^2 ~ "= " ~ .(I2.value) ~ "%)")
text(0.5,0.5,I2.summary.label)
R2 = 50
R2.value <- formatC(R2, digits=1, format="f")
R2.label <- bquote(R^2  ~ "%")
R2.label <- bquote(R^2 ~ "=" ~ .(R2.value) ~ "%")
R2.label
##* Encoding characters---------------
# https://www.unicodepedia.com/groups/
# http://www.unicode.org/charts/
##* Footnotes
# https://www.unicodepedia.com/groups/general-punctuation/
# Dagger  \u2020
# Double dagger  \u2021
# Section symbol \A7
# Double Vertical Line \u2016
# Para    \B6 or \u0086
##*Greek
# https://unicode.org/charts/nameslist/n_0370.html
##* Troubleshooting grab -----------------------
options(error=NULL)
library(tcltk) # For troubleshooting
# msgbox for troubleshooting: 
# tk_messageBox(type = "ok", paste(current.state,': ', nrow(data.state.current),sepo=''), caption = "Troubleshooting")
# browser()
# Finish, quit is c or Q
# enter f press enter and then enter Q

### Data grab ===================================
# co <- read.table("https://data.princeton.edu/eco572/datasets/cohhpop.dat", col.names=c("age","pop"), header=FALSE)
##* Staff survey data------------------
file.filter   <- matrix(c("Spreadsheets","*.csv;*.xls;*.xlsx","All","*.*"),byrow=TRUE,ncol=2)
# Use "ST19-Organisation-Data-2019-Only-v1f.csv"
filename      <- choose.files(filters = file.filter,caption = "Select data file",index = 1,multi=FALSE)
file.extension<- substr(filename, nchar(filename) - 2, nchar(filename))
if (file.extension == 'csv'){
  data.import   <- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", stringsAsFactors=FALSE, strip.white=TRUE)
}else{
  data.import   <- read.xlsx(filename)}
data.StaffSurvey2019 <- data.import
head(data.StaffSurvey2019)
nrow(data.StaffSurvey2019)

##* ReName columns?------------------------------
#rename(data.temp, c("q2a_2019"="Engagement (vigor)", "q2b_2019"="Engagement (dedication)","q2c_2019"="Engagement (absorption)")) # plyer
names(data.StaffSurvey2019)[1] <- "Code"
names(data.StaffSurvey2019)[22] <- "Engagement (vigor)"
names(data.StaffSurvey2019)[23] <- "Engagement (dedication)"
names(data.StaffSurvey2019)[24] <- "Engagement (absorption)"
names(data.StaffSurvey2019)[68] <- "Workstress"
data.StaffSurvey2019 <- data.StaffSurvey2019[, c("Trust","rr_2019","responses","Engagement (vigor)","Engagement (dedication)","Engagement (absorption)","Workstress")]

summary(data.StaffSurvey2019$rr_2019)
summary(data.StaffSurvey2019$`Engagement (dedication)`)

## Regression -----------------------------------
##* Response rate----------------------
factor.name <- "Engagement (dedication)"
factor.name <- "Workstress"
#factor <- data.temp$`Engagement (dedication)`
data.StaffSurvey2019$factor <- data.StaffSurvey2019[factor.name]
data.StaffSurvey2019$factor <- as.vector(unlist(data.StaffSurvey2019$factor ))
lm.out <- lm(factor ~ rr_2019, data=data.StaffSurvey2019)
lm.out <- summary(lm.out)
Title <- paste('Plot of response rate vs ',factor.name, "\n(NHS Staff Surveys, 2019)")
plot(data.StaffSurvey2019$rr_2019, data.StaffSurvey2019$factor, xlab="Response rate", ylab = paste(factor.name,' (rate)') , main=Title)
abline(lm.out$coefficients[1,1], lm.out$coefficients[2,1], col='red')

R2 <- sprintf("%1.1f%%", 100*lm.out$adj.r.squared)
text(xmax,par("usr")[3]+2.2* strheight("A"),adj=c(1,0),paste("R2 (adjusted) = ",R2,sep=))

pvalue <- paste('P-value: ',round(lm.out$coefficients[2,4],3),sep=)
text(xmax,par("usr")[3]+strheight("A"),adj=c(1,0),pvalue)

### Plot printing ===============================
Title = paste("Plot of response rate vs ", factor.name, " (2019)")
Title <- gsub("\\?|\\!|\\'", "", Title) 
plotname = Title
rstudioapi::savePlotAsImage(
  paste(plotname," - ",current.date,".png",sep=""),
  format = "png", width = 600, height = 600)

