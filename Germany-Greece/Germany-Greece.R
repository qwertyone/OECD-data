#load files
av_wg<-read.csv("av_an_wage.csv", header=TRUE)
gdp<-read.csv("gdp.csv", header=TRUE)
hrs_total<-read.csv("hrs_total.csv", header=TRUE)
hrs_week<-read.csv("hrs_weekly.csv", header=TRUE)

#Add log 10 Transform for visualization in new column
av_wg[,"log_Average_Annual_Salary"]<- log10(av_wg$Average_Annual_Salary)
gdp[,"log_GDP"]<- log10(gdp$GDP)
hrs_total[,"log_Annual_Work_Hours"]<-log10(hrs_total$Annual_Work_Hours)
hrs_week[,"log_Hours_Per_Week"]<-log10(hrs_week$Hours_Per_Week)

df<-merge(av_wg,hrs_total, by = c("Country","TIME","Time"))
df<-merge(df,hrs_week, by = c("Country","TIME","Time","Unit"))

#subset between group vectors
GreekDF<-df[which(df$Country=="Greece"),]
GermanDF<-df[which(df$Country=="Germany"),]

Greek.Average.Annual.Salary<-GreekDF$log_Average_Annual_Salary
Greek.Average.Annual.Work.Hours<-GreekDF$log_Annual_Work_Hours
Greek.Average.Weekly.Hours<-GreekDF$log_Hours_Per_Week
German.Average.Annual.Salary<-GermanDF$log_Average_Annual_Salary
German.Average.Annual.Work.Hours<-GermanDF$log_Annual_Work_Hours
German.Average.Weekly.Hours<-GermanDF$log_Hours_Per_Week

#set y range
y2<-df$log_Annual_Work_Hours
y3<-df$log_Average_Annual_Salary
y<-df$log_Hours_Per_Week
yrng<-c(0,max(df$log_Average_Annual_Salary))
ylim<-extendrange(yrng,f=.1)
wkdens<-density(df$log_Hours_Per_Week)
yrdens<-density(df$log_Annual_Work_Hours)
saldens<-density(df$log_Average_Annual_Salary)
wkdens.max<-max(wkdens$y)
yrdens.max<-max(yrdens$y)
saldens.max<-max(saldens$y)

#Greek data
wkdens_Greek<-density(GreekDF$log_Hours_Per_Week)
yrdens_Greek<-density(GreekDF$log_Annual_Work_Hours)
saldens_Greek<-density(GreekDF$log_Average_Annual_Salary)
wkdens_Greek.max<-max(wkdens_Greek$y)
yrdens_Greek.max<-max(yrdens_Greek$y)
saldens_Greek.max<-max(saldens_Greek$y)

#German data
wkdens_German<-density(GermanDF$log_Hours_Per_Week)
yrdens_German<-density(GermanDF$log_Annual_Work_Hours)
saldens_German<-density(GermanDF$log_Average_Annual_Salary)
wkdens_German.max<-max(wkdens_German$y)
yrdens_German.max<-max(yrdens_German$y)
saldens_German.max<-max(saldens_German$y)

wkdens_t<-t.test(wkdens_German$y,wkdens_Greek$y)
yrdens_t<-t.test(yrdens_German$y,yrdens_Greek$y)
saldens_t<-t.test(saldens_German$y,saldens_Greek$y)

#set x range
x<-GermanDF$Time
#xrng<-range(x)
xrng<-c(2000,2015)
xlim <- extendrange(xrng, f = 0.1)

#png(file = "german-greek.png")

#build the plot
library(grid)
pushViewport(viewport(x=.45,y=.45, width=.7, height=.7, xscale=xlim,yscale=ylim))

grid.points(x,GermanDF$log_Annual_Work_Hours,default.units="native", pch=1,gp=gpar(col="orange"))
grid.points(x,GermanDF$log_Hours_Per_Week,default.units="native", pch=2,gp=gpar(col="orange"))
grid.points(x,GermanDF$log_Average_Annual_Salary,default.units="native", pch=3,gp=gpar(col="orange"))

x<-GreekDF$Time
grid.points(x,GreekDF$log_Annual_Work_Hours,default.units="native", pch=1,gp=gpar(col="blue"))
grid.points(x,GreekDF$log_Hours_Per_Week,default.units="native", pch=2,gp=gpar(col="blue"))
grid.points(x,GreekDF$log_Average_Annual_Salary,default.units="native", pch=3, gp=gpar(col="blue"))

grid.xaxis()
#grid.yaxis()

grid.segments(x0=unit(x,"native"), y0=unit(0,"npc"), x1 = unit(x,"native"), y1=unit(.03,"npc"))
#grid.segments(x0=unit(0,"npc"), y0=unit(.03,"native"), x1 = unit(.03,"npc"), y1=unit(y,"native"))

upViewport(1)

grid.text("Weekly Hours Worked",x=.1,y=.33,rot=90,just=c("centre","top"), gp=gpar(fontsize=6))
grid.text("Yearly Hours Worked",x=.1,y=.55,rot=90,just=c("centre","top"), gp=gpar(fontsize=6))
grid.text("Annual Salary",x=.1,y=.72,rot=90,just=c("centre","top"), gp=gpar(fontsize=6))


upViewport(1)
pushViewport(viewport(x=.85,y=.45, width=.1,height= .7,xscale=c(0,saldens.max),yscale=ylim,clip="on"))
grid.polygon(x=yrdens_Greek$y,y=yrdens_Greek$x,default.units="native",gp=gpar(fill="blue",col="transparent"))
grid.polygon(x=wkdens_Greek$y,y=wkdens_Greek$x,default.units="native",gp=gpar(fill="blue",col="transparent"))
grid.polygon(x=saldens_Greek$y,y=saldens_Greek$x,default.units="native",gp=gpar(fill="blue",col="transparent"))

grid.polygon(x=yrdens_German$y,y=yrdens_German$x,default.units="native",gp=gpar(fill="orange",col="transparent"))
grid.polygon(x=wkdens_German$y,y=wkdens_German$x,default.units="native",gp=gpar(fill="orange",col="transparent"))
grid.polygon(x=saldens_German$y,y=saldens_German$x,default.units="native",gp=gpar(fill="orange",col="transparent"))

#pvalues
#y4<-((mean(wkdens_Greek$y)+mean(wkdens_German$y))/2)
grid.text(paste("p= ", wkdens_t$p.value),x=0.1, y=.35,gp=gpar(fontsize=5),just=c("left","bottom"))

upViewport(1)
pushViewport(viewport(x=.5,y=.87,width=1,height=.1))
grid.text("Average Work Lifestyles of Germans and Greeks", x=.5,y=.5)
grid.text("Germans", x=.40,y=.15,gp=gpar(col="orange"))
grid.text("Greeks", x=.60,y=.15,gp=gpar(col="blue"))

dev.off