library(readxl)
library(forcats)
library(ggforce)

whsT <- read_excel("~/MakeoverMonday/whiteHouseSalary/WhiteHouseSalaries.xlsx", sheet=1)
whsO <- read_excel("~/MakeoverMonday/whiteHouseSalary/WhiteHouseSalaries.xlsx", sheet=2)

whs <- rbind(whsT, whsO)

whsQuartiles <- whs %>% group_by(ADMINISTRATION) %>% summarise(
  q25 = quantile(SALARY, 0.25),
  q50 = quantile(SALARY, 0.5),
  q75 = quantile(SALARY, 0.75),
  q100 = quantile(SALARY, 1)
  )
whsQuartileMelt <- reshape2::melt(whsQuartiles)

whsQ75Avg <- whs %>% group_by(ADMINISTRATION) %>% mutate(q75=quantile(SALARY, 0.75)) %>% filter(SALARY > q75) %>% summarise(sum=sum(SALARY), avg=round(mean(SALARY)))
whsQ25Avg <- whs %>% group_by(ADMINISTRATION) %>% mutate(q25=quantile(SALARY, 0.25)) %>% filter(SALARY < q25) %>% summarise(sum=sum(SALARY), avg=round(mean(SALARY)))


whsPlot <- whs
whsPlot$ADMINISTRATION <- factor(whsPlot$ADMINISTRATION, levels=c("Obama", "blank", "Trump"))

p <- ggplot(whsPlot, aes(y=SALARY, x=ADMINISTRATION)) + 
  geom_violin(scale="count", aes(fill=ADMINISTRATION), color="#00000033", adjust=1, width=1) + 
  ggforce::geom_sina(binwidth=10000, size=2, method="counts", color="#00000066", maxwidth=0.8) + 
 # geom_boxplot(fill=NA) +
  theme_minimal() + 
  scale_fill_manual(values=c("Trump"="#cc3d3d33", "blank"="Quartiles", "Obama"="#1a80c433"), guide="none") +
  scale_color_manual(values=c("Trump"="#cc3d3d", "Obama"="#1a80c4"), guide="none") + 
  scale_x_discrete(labels=c("Obama"="Obama 2016", "blank"="", "Trump"="Trump 2017"), breaks=c("Obama", "Obama", "Trump"), name=NULL, drop=F) + 
  scale_y_continuous(labels = scales::dollar, minor_breaks = seq(0,200000,10000)) +
  ggtitle("Pay Distribution of the Obama and Trump Administrations", subtitle="Lean Trump or Balanced Obama") + 
  expand_limits(y=c(0,200000)) + 
  geom_line(data=whsQuartileMelt, aes(x=2.1, y=value, group=variable)) +
  geom_point(data=whsQuartileMelt, aes(x=2.1, y=value, color=ADMINISTRATION)) +  
  geom_text(data=whsQuartiles, x=2.1, y=whsQuartiles[[1,"q25"]], label=paste0("25% of Obama's staff earned less than: ", scales::dollar(whsQuartiles[[1,"q25"]]), "\n", scales::dollar(whsQuartiles[[2,"q25"]]-whsQuartiles[[1,"q25"]]), " a year less than Trump's\nBut Obama's office spent ", scales::dollar(whsQ25Avg[[1,"sum"]]) ," on these employees\nwhilst Trump spends ", scales::dollar(whsQ25Avg[[2,"sum"]])), vjust=1.5) +
  geom_text(data=whsQuartiles, x=2.1, y=whsQuartiles[[2,"q100"]], label=paste0("Trumps highest pay is: ", scales::dollar(whsQuartiles[[2,"q100"]]), "\n", scales::dollar(whsQuartiles[[2,"q100"]]-whsQuartiles[[1,"q100"]]), " higher than the Obama administration"), vjust=-0.5) +
  geom_text(data=whsQuartiles, x=2.1, y=whsQuartiles[[2,"q75"]], label=paste0("Of those with the top 25% of salaries, the Trump mean is ", scales::dollar(whsQ75Avg[[2,"avg"]]), "\n", scales::dollar(whsQ75Avg[[2,"avg"]]-whsQ75Avg[[1,"avg"]]), " more than the Obama administration"), vjust=-0.5) +
  geom_text(data=whsQuartileMelt, x=2.1, aes(y=value, label=scales::dollar(value), color=ADMINISTRATION, vjust=0.5, hjust=((as.numeric(as.factor(ADMINISTRATION))-1.5)*-1.2)+0.5)) + ylab("Yearly Salary")

#Save output at 1100x1100 px
library(grid)
library(gridExtra)
grid.newpage()
footnote <- "#MakeoverMonday @MrYeti1\nDatasource: NPR - How Trump And Obama's Staff Salaries Spread Across The Board"
g <- arrangeGrob(p, bottom = textGrob(footnote, x = 0, hjust = -0.1, vjust=0.2, gp = gpar(fontface = "italic", fontsize = 9)))
grid.draw(g)
#whsQuartileMelt


#ggplot() + expand_limits(y=c(0,200000)) + 
#  scale_color_manual(values=c("Trump"="#cc3d3d", "Obama"="#1a80c4")) + 
#  geom_line(data=whsQuartileMelt, aes(x=2.1, y=value, group=variable)) +
#  geom_point(data=whsQuartileMelt, aes(x=2.1, y=value, color=ADMINISTRATION)) + 
#  geom_text(data=whsQuartiles, x=2.1, y=whsQuartiles[[1,"q25"]], label=paste0("25% of Obama's staff earned less than: ", scales::dollar(whsQuartiles[[1,"q25"]]), ".\n", scales::dollar(whsQuartiles[[2,"q25"]]-whsQuartiles[[1,"q25"]]), " a year less than Trumps.\nBut Obama's office spent ", scales::dollar(whsQ25Avg[[1,"sum"]]) ," on these employees\nwhilst Trump spends ", scales::dollar(whsQ25Avg[[2,"sum"]])), vjust=1.5) +
#  geom_text(data=whsQuartiles, x=2.1, y=whsQuartiles[[2,"q100"]], label=paste0("Trumps highest pay is: ", scales::dollar(whsQuartiles[[2,"q100"]]), ".\n", scales::dollar(whsQuartiles[[2,"q100"]]-whsQuartiles[[1,"q100"]]), " higher than the Obama administration"), vjust=-0.5) +
#  geom_text(data=whsQuartiles, x=2.1, y=whsQuartiles[[2,"q75"]], label=paste0("Of those in the top 25% of salaries, the Trump mean is ", scales::dollar(whsQ75Avg[[2,"avg"]]), ".\n", scales::dollar(whsQ75Avg[[2,"avg"]]-whsQ75Avg[[1,"avg"]]), " more than the Obama administration"), vjust=-0.5) +
#  geom_text(data=whsQuartileMelt, x=2.1, aes(y=value, label=scales::dollar(value), vjust=0.5, hjust=((as.numeric(as.factor(ADMINISTRATION))-1.5)*-1.2)+0.5)) +
#  geom_text(data=whsQuartiles, x=2.1, aes(y=c(mean(whsQuartiles$q50), mean(whsQuartiles$q100)), label=c("Median Salary", "Maximum Salary")), vjust=0.5, hjust=-0.1, color="#666666")

