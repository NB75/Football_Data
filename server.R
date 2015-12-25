library(shiny)
library(dplyr)
library(arulesViz)
library(ggplot2)
library(reshape2)

#set of leagues to be loaded
league_code <- c("E0","I1","D1","SP1")
league_desc <- c("Premier League","Serie A","Bundesliga","La Liga")
league_no   <- length(league_code)

#set of years to be loaded
years <- c(2011:2015)
minYear <- min(years)
maxYear <- max(years)

#set of columns to be loaded
sel_columns <- c("Div",
                 "Date",
                 "HomeTeam",
                 "AwayTeam",
                 "FTHG",
                 "FTAG",
                 "FTR",
                 "HTHG",
                 "HTAG",
                 "HTR",
                 "HS",
                 "AS",
                 "HST",
                 "AST",
                 "HF",
                 "AF",
                 "HC",
                 "AC",
                 "HY",
                 "AY",
                 "HR",
                 "AR")

#load data
for (j in 1:league_no) {

  cur_league_code <- league_code[j]
  cur_league_desc <- league_desc[j]

  for (i in minYear:maxYear) {
      fn <- paste("./data/",cur_league_code,"_",i,".csv",sep="") 
      df_temp <- read.csv(fn)
  
      #subset of columns with results
      df_temp <- df_temp[,sel_columns]
  
      #addition of League Descr
      df_temp$League <- rep(cur_league_desc,nrow(df_temp))

      #addition of Year
      df_temp$Year <- rep(i,nrow(df_temp))
  
      #identification of Teams and Number of Matches
      Teams <- unique(df_temp$HomeTeam)
      no_Teams <- length(Teams)
      no_Matches <- no_Teams/2
  
      #addition of matchday number for historical
      no_day <- nrow(df_temp)/(no_Matches)
      Day <- rep(1:no_day,each=no_Matches)
      df_temp  <- data.frame(df_temp,Day)
  
      #addition of a counter
      count <- rep(1,nrow(df_temp))
      df_temp  <- data.frame(df_temp,count)
  
      #addition of columns with points
      df_temp <- mutate(df_temp,HP=ifelse(FTR=="H",3,ifelse(FTR=="D",1,0)),AP=ifelse(FTR=="A",3,ifelse(FTR=="D",1,0)))
  
      if ((i == minYear) & (j == 1)) {
      df <- df_temp
      } else {
      df <- rbind(df,df_temp)       
      }
      }  
}

#filter on selected Year and League
df2 <- df

#Creation of hist_data table
hist_data1 <- data.frame(aggregate(df2[,c("FTHG","HTHG","HS","HST","HF","HC","HY","HR","HP")],by=list(df2$HomeTeam,df2$League,df2$Year),FUN=sum,na.rm=TRUE))
hist_data2 <- data.frame(aggregate(df2[,c("FTAG","HTAG","AS","AST","AF","AC","AY","AR","AP")],by=list(df2$AwayTeam,df2$League,df2$Year),FUN=sum,na.rm=TRUE))

names(hist_data1) <- c("Team","League","Year","FTG","HTG","S","ST","F","C","Y","R","P")
names(hist_data2) <- c("Team","League","Year","FTG","HTG","S","ST","F","C","Y","R","P")

hist_data1 <- mutate(hist_data1,HA="H")
hist_data2 <- mutate(hist_data2,HA="A")

hist_data <- rbind(hist_data1,hist_data2)
hist_data_tot <- data.frame(aggregate(hist_data[,c("P","FTG","ST","S","C","F","Y","R")],by=list(hist_data$Team,hist_data$League,hist_data$Year),FUN=sum,na.rm=TRUE))
names(hist_data_tot) <- c("Team","League","Year","Points","Goals","Shots.on.Target","Shots","Corners","Fouls","Yellow Cards","Red Cards")

avg_goals <- c()
avg_sot   <- c()
beta0     <- c()
beta1     <- c()

for (k in 1:league_no) {
  cur_league_desc <- league_desc[k]
  cur_league_tab <- hist_data_tot[hist_data_tot$League == cur_league_desc,]
  cur_avg_goals <- mean(cur_league_tab$Goals,na.rm=TRUE)
  cur_avg_sot   <- mean(cur_league_tab$Shots.on.Target,na.rm=TRUE)
  fit <- lm(Goals ~ Shots.on.Target,cur_league_tab)
  cur_beta0 <- coefficients(fit)[[1]]
  cur_beta1 <- coefficients(fit)[[2]]
  
  avg_goals <- c(avg_goals,cur_avg_goals)
  avg_sot   <- c(avg_sot,cur_avg_sot)
  beta0     <- c(beta0,cur_beta0)
  beta1     <- c(beta1,cur_beta1)
  
}
regr_table <- data.frame(league_desc,avg_goals,avg_sot,beta0,beta1)
names(regr_table) <- c("League","Average Goals","Average Shots on Target","Intercept","Slope")

# Shiny server
shinyServer(
  function(input, output) {
    output$chart1 <- renderChart2({
      #filter on selected Year and League
      df2 <- df[df$Year==input$Year_RB,]
      df2 <- df2[df2$League==input$League_RB,]
      df2 <- df2[(df2$Day >= input$matchdays[1])&(df2$Day <= input$matchdays[2]),]
      
      #Creation of hist_data table
      hist_data1 <- data.frame(aggregate(df2[,c("FTHG","HTHG","HS","HST","HF","HC","HY","HR","HP")],by=list(df2$HomeTeam),FUN=sum,na.rm=TRUE))
      hist_data2 <- data.frame(aggregate(df2[,c("FTAG","HTAG","AS","AST","AF","AC","AY","AR","AP")],by=list(df2$AwayTeam),FUN=sum,na.rm=TRUE))
      
      names(hist_data1) <- c("Team","FTG","HTG","S","ST","F","C","Y","R","P")
      names(hist_data2) <- c("Team","FTG","HTG","S","ST","F","C","Y","R","P")
      
      hist_data1 <- mutate(hist_data1,HA="H")
      hist_data2 <- mutate(hist_data2,HA="A")
      
      hist_data <- rbind(hist_data1,hist_data2)
      
      n1 <- nPlot(P ~ Team,
                  group="HA",
                  type='multiBarHorizontalChart',
                  data=hist_data)
      return(n1)
    })
    output$chart2 <- renderChart2({
      #filter on selected Year and League
      df2 <- df[df$Year==input$Year_RB,]
      df2 <- df2[df2$League==input$League_RB,]
      df2 <- df2[(df2$Day >= input$matchdays[1])&(df2$Day <= input$matchdays[2]),]
      
      #Creation of hist_data table
      hist_data1 <- data.frame(aggregate(df2[,c("FTHG","HTHG","HS","HST","HF","HC","HY","HR","HP")],by=list(df2$HomeTeam),FUN=sum,na.rm=TRUE))
      hist_data2 <- data.frame(aggregate(df2[,c("FTAG","HTAG","AS","AST","AF","AC","AY","AR","AP")],by=list(df2$AwayTeam),FUN=sum,na.rm=TRUE))
      
      names(hist_data1) <- c("Team","FTG","HTG","S","ST","F","C","Y","R","P")
      names(hist_data2) <- c("Team","FTG","HTG","S","ST","F","C","Y","R","P")
      
      hist_data1 <- mutate(hist_data1,HA="H")
      hist_data2 <- mutate(hist_data2,HA="A")
      
      hist_data <- rbind(hist_data1,hist_data2)
      
      shots <- data.frame(aggregate(hist_data[,c("S","ST","FTG")],by=list(hist_data$Team),FUN=sum,na.rm=TRUE))
      names(shots) <- c("Team","Shots","Shots.on.Target","Goals")
      shots <- melt(shots,id.vars="Team")
      n2 <- nPlot(value ~ Team,
                  group="variable",
                  type='multiBarHorizontalChart',
                  data=shots)
      n2$chart(showControls = F)
      return(n2)
    })
    output$chart3 <- renderChart2({
      #filter on selected Year and League
      df2 <- df[df$Year==input$Year_RB,]
      df2 <- df2[df2$League==input$League_RB,]
      df2 <- df2[(df2$Day >= input$matchdays[1])&(df2$Day <= input$matchdays[2]),]
      
      #Creation of hist_data table
      hist_data1 <- data.frame(aggregate(df2[,c("FTHG","HTHG","HS","HST","HF","HC","HY","HR","HP")],by=list(df2$HomeTeam),FUN=sum,na.rm=TRUE))
      hist_data2 <- data.frame(aggregate(df2[,c("FTAG","HTAG","AS","AST","AF","AC","AY","AR","AP")],by=list(df2$AwayTeam),FUN=sum,na.rm=TRUE))
      
      names(hist_data1) <- c("Team","FTG","HTG","S","ST","F","C","Y","R","P")
      names(hist_data2) <- c("Team","FTG","HTG","S","ST","F","C","Y","R","P")
      
      hist_data1 <- mutate(hist_data1,HA="H")
      hist_data2 <- mutate(hist_data2,HA="A")
      
      hist_data <- rbind(hist_data1,hist_data2)
      hist_data_tot <- data.frame(aggregate(hist_data[,c("FTG","ST")],by=list(hist_data$Team),FUN=sum,na.rm=TRUE))
      names(hist_data_tot) <- c("Team","ST","FTG")
      n3 <- nPlot(ST ~ FTG, 
                  group = 'Team', 
                  data = hist_data_tot, 
                  type = 'scatterChart')
      n3$xAxis(axisLabel = 'Shots on Target')
      n3$chart(showControls = T)
      return(n3)
    })
    output$chart4 <- renderChart2({
      #filter on selected Year and League
      df2 <- df[df$Year==input$Year_RB,]
      df2 <- df2[df2$League==input$League_RB,]
      df2 <- df2[(df2$Day >= input$matchdays[1])&(df2$Day <= input$matchdays[2]),]
      
      #Creation of hist_data table
      hist_data1 <- data.frame(aggregate(df2[,c("FTHG","HTHG","HS","HST","HF","HC","HY","HR","HP")],by=list(df2$HomeTeam),FUN=sum,na.rm=TRUE))
      hist_data2 <- data.frame(aggregate(df2[,c("FTAG","HTAG","AS","AST","AF","AC","AY","AR","AP")],by=list(df2$AwayTeam),FUN=sum,na.rm=TRUE))
      
      names(hist_data1) <- c("Team","FTG","HTG","S","ST","F","C","Y","R","P")
      names(hist_data2) <- c("Team","FTG","HTG","S","ST","F","C","Y","R","P")
      
      hist_data1 <- mutate(hist_data1,HA="H")
      hist_data2 <- mutate(hist_data2,HA="A")
      
      hist_data <- rbind(hist_data1,hist_data2)
      fouls <- data.frame(aggregate(hist_data[,c("F","Y","R")],by=list(hist_data$Team),FUN=sum,na.rm=TRUE))
      names(fouls) <- c("Team","Fouls","Yellow.Cards","Red.Cards")
      fouls <- melt(fouls,id.vars="Team")
      n4 <- nPlot(value ~ Team,
                  group="variable",
                  type='multiBarHorizontalChart',
                  data=fouls)
      n4$chart(showControls = F)
      return(n4)
    })
    output$dataTable <- renderDataTable(
      {#filter on selected Year and League
      df2 <- df[df$Year==input$Year_RB,]
      df2 <- df2[df2$League==input$League_RB,]
      df2 <- df2[(df2$Day >= input$matchdays[1])&(df2$Day <= input$matchdays[2]),]
      
      #Creation of hist_data table
      hist_data1 <- data.frame(aggregate(df2[,c("FTHG","HTHG","HS","HST","HF","HC","HY","HR","HP")],by=list(df2$HomeTeam),FUN=sum,na.rm=TRUE))
      hist_data2 <- data.frame(aggregate(df2[,c("FTAG","HTAG","AS","AST","AF","AC","AY","AR","AP")],by=list(df2$AwayTeam),FUN=sum,na.rm=TRUE))
      
      names(hist_data1) <- c("Team","FTG","HTG","S","ST","F","C","Y","R","P")
      names(hist_data2) <- c("Team","FTG","HTG","S","ST","F","C","Y","R","P")
      
      hist_data1 <- mutate(hist_data1,HA="H")
      hist_data2 <- mutate(hist_data2,HA="A")
      
      hist_data <- rbind(hist_data1,hist_data2)
      hist_data_tot <- data.frame(aggregate(hist_data[,c("P","FTG","ST","S","C","F","Y","R")],by=list(hist_data$Team),FUN=sum,na.rm=TRUE))
      names(hist_data_tot) <- c("Team","Points","Goals Scored","Shots on Target","Shots","Corners","Fouls","Yellow Cards","Red Cards")
      hist_data_tot <- hist_data_tot[order(hist_data_tot$Points,decreasing=TRUE),]
      hist_data_tot}, options = list(pageLength = 25,paging = FALSE,searching = FALSE))
    
    output$regressionTable <- renderDataTable(
      {ext_table <- regr_table
       ext_table},options = list(paging = FALSE,searching = FALSE))
    
    goal_predict <- eventReactive(input$predButton, {
      sel_beta0 <- regr_table[regr_table$League==input$League_Pred_RB,4]
      sel_beta1 <- regr_table[regr_table$League==input$League_Pred_RB,5]
      return(sel_beta0+sel_beta1*as.numeric(input$sot))
    })
    
    output$pred_goals <- renderText({
      goal_predict()
    })
    
  }
)

