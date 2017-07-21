library(readxl)
library(forcats)
whsT <- read_excel("~/MakeoverMonday/whiteHouseSalary/WhiteHouseSalaries.xlsx", sheet=1)
whsO <- read_excel("~/MakeoverMonday/whiteHouseSalary/WhiteHouseSalaries.xlsx", sheet=2)

whs <- rbind(whsT, whsO)

table(whs$STATUS)
whs

ggplot(whs, aes(x=SALARY)) + geom_histogram() + facet_grid(~ADMINISTRATION)

ggplot(whs, aes(x=STATUS)) + geom_histogram(stat="count") + facet_grid(~ADMINISTRATION)

ggplot(whs, aes(y=SALARY, x=ADMINISTRATION)) + geom_boxplot() + geom_jitter()




ggplot(whs %>% arrange(SALARY) , aes(x=SALARY)) + stat_ecdf() + facet_grid(~ADMINISTRATION)

ggplot(whs %>% filter(startsWith(`POSITION TITLE`, "SPECIAL ASSISTANT")), aes(y=SALARY, x=ADMINISTRATION)) + geom_boxplot() + geom_jitter()


ggplot(whs %>% filter(startsWith(`POSITION TITLE`, "SPECIAL ASSISTANT TO THE PRESIDENT")), aes(y=SALARY, x=ADMINISTRATION)) + geom_boxplot() + geom_jitter()
ggplot(whs %>% filter(startsWith(`POSITION TITLE`, "ASSISTANT TO THE PRESIDENT")), aes(y=SALARY, x=ADMINISTRATION)) + geom_boxplot() + geom_jitter()

ggplot(whs %>% filter(startsWith(`POSITION TITLE`, "DIRECTOR OF")), aes(y=SALARY, x=ADMINISTRATION)) + geom_boxplot() + geom_jitter()
ggplot(whs %>% filter(startsWith(`POSITION TITLE`, "DEPUTY DIRECTOR")), aes(y=SALARY, x=ADMINISTRATION)) + geom_boxplot() + geom_jitter()

whs$`POSITION` <- whs$`POSITION TITLE`

whs$`POSITION` <- whs$`POSITION` %>% gsub("SENIOR (.*)", "\\1", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("DEPUTY (.*)", "\\1", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("ASSOCIATE (.*)", "\\1", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("EXECUTIVE (.*)", "\\1", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("SPECIAL (.*)", "\\1", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("PRESIDENTIAL (.*)", "\\1", .)





whs$`POSITION` <- whs$`POSITION` %>% gsub("ASSISTANT TO THE PRESIDENT.*", "ASSISTANT TOTHE PRESIDENT", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("ASSISTANT AND .*", "ASSISTANT", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("ASSISTANT TO .*", "ASSISTANT (TO OTHER)", .)

whs$`POSITION` <- whs$`POSITION` %>% gsub("SUPERVISOR.*", "SUPERVISOR", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("PRESS ASSISTANT.*", "PRESS ASSISTANT", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("POLICY ADVISOR.*", "POLICY ADVISOR", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("WEST WING RECEPTIONIST.*", "WEST WING RECEPTIONIST", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("TRIP COORDINATOR.*", "TRAVEL", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("TRAVEL.*", "TRAVEL", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("STAFF ASSISTANT.*", "STAFF ASSISTANT", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("WRITER.*", "WRITER", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("SCHEDULER.*", "SCHEDULER", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("DIRECTOR.*", "DIRECTOR", .)
whs$`POSITION` <- whs$`POSITION` %>% gsub("CHIEF OF STAFF.*", "CHIEF OF STAFF", .)












whs$POSITION <- as.factor(whs$POSITION)
levels(whs$POSITION)


a <- ggplot(whs, aes(y=SALARY, x=fct_infreq(POSITION))) + geom_boxplot() #+ geom_jitter()
a



is_outlier <- function(x) {
  #return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
  lq <- quantile(x, 0.25) - 1.5 * IQR(x);
  uq <- quantile(x, 0.75) + 1.5 * IQR(x)
  return (ifelse(x < lq, -(lq-x), ifelse(x>uq, x-uq, NA)) )
  #if (x < lq) { return(lq-x) }
  #if (x > uq) { return(x-uq) }
  #return (NA)
}

whsOutliers <- whs %>% group_by(POSITION) %>% mutate(outlier = is_outlier(SALARY)) %>% filter(!is.na(outlier)) %>% select(ADMINISTRATION, NAME, SALARY, `POSITION TITLE`, POSITION, outlier)
View(whsOutliers)

ggplot(whsOutliers, aes(x=ADMINISTRATION, y=SALARY, color=as.logical(outlier>0))) + ggforce::geom_sina(method="counts", maxwidth=2)# + facet_grid(~ADMINISTRATION) + theme(axis.text.x = element_text(angle=60, hjust=1))

whs %>% filter(POSITION=="DIRECTOR") %>% mutate(outlier = is_outlier(SALARY), IQR=IQR(SALARY), lq = quantile(SALARY, 0.25) - 1.5 * IQR(SALARY), uq = quantile(SALARY, 0.75) + 1.5 * IQR(SALARY) ) %>% select(SALARY, `POSITION`, outlier, IQR, lq, uq) %>% View()



levelCut <- cut_width(whs$SALARY, 10000, boundary=0)
levelCut  <- fct_rev(levelCut)
whsCut <- whs
whsCut$levelCut <- levelCut

  ggplot(whsCut, aes(y=SALARY, x=ADMINISTRATION)) + geom_violin(scale = "count") + facet_grid(levelCut~., scales="free_y")

  ggplot(whsCut, aes(y=SALARY, x=ADMINISTRATION)) + geom_violin(scale = "count", draw_quantiles = c(0.25,0.5,0.75))
  
  
  
    ggplot(whs, aes(y=SALARY, x=ADMINISTRATION)) + 
      geom_violin(scale="count", aes(fill=ADMINISTRATION ), color="#00000033", draw_quantiles = c(0.25,0.5,0.75)) + 
      ggforce::geom_sina(binwidth=10000, size=2, method="counts", color="#00000066", maxwidth=0.8) + 
      theme_minimal() + 
      scale_fill_manual(values=c("Trump"="#cc3d3d33", "Obama"="#1a80c433"), guide="none") + 
      scale_x_discrete(labels=c("Obama"="Obama 2016", "Trump"="Trump 2017"), name=NULL) + 
      scale_y_continuous(labels = scales::dollar) +
      ggtitle("Pay Distribution of the Obama and Trump Administrations", subtitle="Lean Trump or Balanced Obama") + 
      expand_limits(y=c(0,200000))
    