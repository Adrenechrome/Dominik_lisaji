# Nalaganje knjiznic
library(shiny)
library(shinymanager)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(fmsb)
library(DT)
library(tidyverse)
library(shinythemes)
library(shinyBS) #vprasajcki
library(shinydashboard)
library(shinyWidgets)
library(openxlsx)

# Data import


surovi <- read.xlsx("vsi_podatki_3_changes.xlsx")

pod <- surovi

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# pod$Layer <- substrRight(pod$Profile, 1)
# pod <- subset(pod, pod$Layer != 1)
# pod$Layer <- ifelse(pod$Layer==0,10,pod$Layer)
# pod$Layer <- as.numeric(pod$Layer)



###

# dat <- subset(pod, select = c("Location.2", "Location.1", "Location.3", "Depth", "Layer", "Profile", "T-Hg", "Pb", "U"))
# colnames(dat) <- c("17_lokacij", "3_lokacije", "6_lokacij", "Depth", "Layer", "Profile", "T_Hg", "Pb", "U")

dat <- pod

for (i in 1:ncol(dat)) {
  if(is.character(dat[,i])){
    dat[,i] <- as.factor(dat[,i])
  }
}

rownames(dat) <- 1:nrow(dat)


GGTukey.2<-function(Tukey, sig=FALSE){
  A<-require("tidyverse")
  if(A==TRUE){
    library(tidyverse)
  } else {
    install.packages("tidyverse")
    library(tidyverse)
  }
  
  if(sig){
    barve<-"green"
  } else {barve <- c("red", "green")}
  
  B<-as.data.frame(Tukey[1])
  colnames(B)[2:4]<-c("min",
                      "max",
                      "p")
  C<-data.frame(id=row.names(B),
                min=B$min,
                max=B$max,
                idt=ifelse(B$p<0.05,
                           "significant",
                           "not significant")
  )
  
  if(sig){C <- filter(C, idt=="significant")}
  
  D<-C%>%
    ggplot(aes(id,color=idt))+
    geom_errorbar(aes(ymin=min,
                      ymax=max),
                  width = 0.5,
                  size=1.25)+
    labs(x=NULL,
         color=NULL)+
    scale_color_manual(values=barve
    )+
    geom_hline(yintercept=0,
               color="black", linetype="dashed")+
    coord_flip()+
    theme(text=element_text(family="TimesNewRoman"),
          title=element_text(color="black",size=15),
          axis.text = element_text(color="black",size=10),
          axis.title = element_text(color="black",size=10),
          panel.grid=element_line(color="grey75"),
          axis.line=element_blank(),
          plot.background=element_rect(fill="white",color="white"),
          panel.background=element_rect(fill="white"),
          panel.border = element_rect(colour = "black", fill = NA,size=0.59),
          legend.key= element_rect(color="white",fill="white")
    )
  return(D)
}

# User interface

ui <- fluidPage( tags$style(type="text/css", "body {padding-top: 70px;}"),
                 
                 navbarPage(title = "Dominik idrija",
                            
                            position = "fixed-top",
                            
                            
                            header = tagList(
                              useShinydashboard()
                            ),
  
  br(),
  br(),
  
  # sidebarLayout(



  #   sidebarPanel(
  #     title="Overview of variables",
  #     
  #     selectInput("var_desc_select", "Choose a variable to wiew:",
  #                 colnames(dat)),
  #     
  #     ##### Opisne statistike ######
  #     
  #     width = 4, collapsible = TRUE, solidHeader = TRUE
  #     
  #   ),
  #   
  #   mainPanel(
  #     
  # ),
  
  ##### SCATTER #####
  # br(),
  # br(),
  
  tabPanel("Linearna regresija",
  
  sidebarLayout(
    sidebarPanel(
      style = "position:fixed;width:30%;",
      h2("Linearna regresija"),
      
      hr(),
      # actionButton("df_select_1", "Vse meritve", selected=TRUE),
      # actionButton("df_select_2", "Prečiščene meritve"),
      uiOutput("scat_y"),
      uiOutput("scat_x"),
      uiOutput("scat_barva"),
      conditionalPanel(
        condition = 'input.barva != "Skupaj"',
        uiOutput("scat_lokacija")
      ),
      
      selectInput("scatter_smooth",
                  "Regresijska premica",
                  c("Ne", "Linearna", "Loessova"),
                  selected = "Ne"),
      conditionalPanel(
        condition = 'input.scatter_smooth == "Linearna" | input.scatter_smooth == "Loessova"',
        selectInput("scatter_IZ",
                    "Intervali zaupanja",
                    c("Da", "Ne"),
                    selected = "Ne"),
      ),
      conditionalPanel(
        condition = 'input.scatter_smooth == "Loessova"',
        sliderInput("scatter_glajenje",
                    "Glajenje Loessove RP",
                    min = 0,
                    max = 1,
                    value = 0.5,
                    step = 0.01)
      ),
      sliderInput(
        "scatter_velikost_tock",
        "Velikost točk:",
        min=0.5,
        max=4,
        value=2,
        step=0.5
      )
      
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("variable_descriptions_plot"),
      br(),
      br(),
      plotlyOutput("variable_descriptions_plot2"),
      br(),
      br(),
      plotlyOutput("distPlot", height = "750px"),
      br(),
      br(),
      plotlyOutput("pPlot", height = "750px")
    )
  ),
  ),
##### ANOVA #####
  tabPanel("Primerjava vzorcev",
  sidebarLayout(
    sidebarPanel(
      style = "position:fixed;width:30%;",
      h2("Primerjava vzorcev"),
      
      hr(),
      # actionButton("df_select_1", "Vse meritve", selected=TRUE),
      # actionButton("df_select_2", "Prečiščene meritve"),
      uiOutput("anova_odzivna"),
      uiOutput("anova_skupine"),
      uiOutput("anova_lokacija"),
      selectInput("anova_izbor", "Prikaz kombinacij", c("Vse", "Samo stat. značilne"), selected = "Vse")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("anova_boxplot"),
      plotlyOutput("tukey_plot", height = "800px"),
      br(),
      br()
    )
  ),
  )
))


########################## SERVER #########################################
###########################################################################

server <- function(input, output, session) {
  # output$variable_overview_reactive <- renderUI({
  #   selectInput("var_desc_select", "Choose a variable to wiew:",
  #               colnames(dat))
  # })
  
  # observeEvent(input$df_select_1, {
  #   dat <- pod_vse
  # })
  # 
  # observeEvent(input$df_select_2, {
  #   dat <- pod_preciscene
  # })
  
  output$variable_descriptions_plot <- renderPlotly({
    
    if(input$barva == "Skupaj"){
      temp <- dat
    } else {temp <- dat[dat[,input$barva] %in% input$lokacije,]}
    if(input$barva=="Skupaj"){lok<-"Skupaj"} else{lok <- temp[, input$barva]}
    temp <- data.frame("Spremenljivka"=temp[,input$y_os], "Lokacije"=lok)
    # temp <- na.omit(temp)
    if(is.numeric(temp$Spremenljivka)){
      if(length(unique(temp$Spremenljivka))<=15){
        p <- ggplot(temp, aes(Spremenljivka, color=Lokacije)) + geom_histogram(bins=100, col="white", fill="black") +
          ylab("Number of observations") + xlab(input$y_os) + 
          xlim(c(min(dat[,input$y_os], na.rm = TRUE), max(dat[,input$y_os], na.rm=TRUE)))}
      else {
        p <- ggplot(temp, aes(Spremenljivka, fill=Lokacije, color=Lokacije)) + geom_density(alpha=0.5) +
          ylab("Gostota") + xlab(input$y_os)
      }
    }
    else {
      p <- ggplot(temp, aes(Spremenljivka, color=Spremenljivka, fill=Spremenljivka)) + geom_bar() + ylab("Število") + xlab(input$y_os)  +
        theme(legend.position = "bottom", axis.text.x=element_blank())}
    ggplotly(p + labs(title=paste("Porazdelitev odzivne spremenljivke (", input$y_os, ")", sep = "")), tooltip = c("x", "color", "fill"))
  })
  
  output$variable_descriptions_plot2 <- renderPlotly({
    
    if(input$barva == "Skupaj"){
      temp <- dat
    } else {temp <- dat[dat[,input$barva] %in% input$lokacije,]}
    if(input$barva=="Skupaj"){lok<-"Skupaj"} else{lok <- temp[, input$barva]}
    temp <- data.frame("Spremenljivka"=temp[,input$x_os], "Lokacije"=lok)
    # temp <- na.omit(temp)
    if(is.numeric(temp$Spremenljivka)){
      if(length(unique(temp$Spremenljivka))<=15){
        p <- ggplot(temp, aes(Spremenljivka, color=Lokacije)) + geom_histogram(bins=100, col="white", fill="black") +
          ylab("Number of observations") + xlab(input$x_os) + 
          xlim(c(min(dat[,input$x_os], na.rm = TRUE), max(dat[,input$x_os], na.rm=TRUE)))}
      else {
        p <- ggplot(temp, aes(Spremenljivka, fill=Lokacije, color=Lokacije)) + geom_density(alpha=0.5) +
          ylab("Gostota") + xlab(input$x_os)
      }
    }
    else {
      p <- ggplot(temp, aes(Spremenljivka, color=Spremenljivka, fill=Spremenljivka)) + geom_bar() + ylab("Število") + xlab(input$x_os)  +
        theme(legend.position = "bottom", axis.text.x=element_blank())}
    ggplotly(p + labs(title=paste("Porazdelitev napovedne spremenljivke (", input$x_os, ")", sep = "")), tooltip = c("x", "color", "fill"))
  })
  
  ###### scatter #####
  
  stevilcni <- select_if(dat, is.numeric)
  faktorji <- select_if(dat, is.factor)
  vars_stev <- colnames(stevilcni)
  vars_fak <- colnames(faktorji)
  scat_sprem <- reactiveValues(stev = vars_stev, skup = vars_fak)

  output$scat_x <- renderUI({
    selectInput("x_os",
                "Napovedovalna (x os):",
                scat_sprem$stev,
                selected = "U")
  })

  output$scat_y <- renderUI({
    selectInput("y_os",
                "Odzivna (y os):",
                scat_sprem$stev,
                selected = "T-Hg")
  })

  output$scat_barva <- renderUI({
    selectInput("barva",
                "Tipi lokacij:",
                c("Skupaj", scat_sprem$skup),
                selected = "Location_2")
  })
  
  output$scat_lokacija <- renderUI({
    selectInput("lokacije",
                "Posamezne lokacije",
                unique(dat[,input$barva]),
                multiple = TRUE,
                selected = unique(dat[,input$barva]))
  })
  


  output$distPlot <- renderPlotly({
     if(input$barva != "Skupaj"){
       # zac <- filter(dat, dat[,input$barva] == input$lokacije)
       zac <- dat[dat[,input$barva] %in% input$lokacije, ]
       } else {zac <- dat}
    stevilcni1 <- select_if(zac, is.numeric)
    faktorji1 <- select_if(zac, is.factor)

    x    <- stevilcni1[, input$x_os]
    y <- stevilcni1[, input$y_os]
    if(input$barva=="Skupaj"){z<-"Skupaj"} else{z <- faktorji1[, input$barva]}
    sca <- data.frame(z,x,y, stringsAsFactors = TRUE)
    colnames(sca) <- c("z", "x", "y")
    sca$z <- as.factor(sca$z)
    sca <- na.omit(sca)

    # velikost tock

    v_t <- input$scatter_velikost_tock

    # draw the histogram with the specified number of bins
    p <- ggplot(sca, aes(x,y,color=z)) + geom_point(size=v_t) + ylab(input$y_os) +
      xlab(input$x_os) + labs(title=paste0("Razsevni diagram odzivne (", input$y_os, (") in napovedne ("), 
                                          input$x_os, (") spremenljivke")))  +
      guides(fill=FALSE)

    # if(input$y_os == "STROOP: z-score povprečje"){
    #     sre <- mean(sca$y)
    #     stdd <- sd(sca$y)
    #     p <- p + geom_hline(yintercept = sre, color="red") +
    #         geom_hline(yintercept = sre+ 2*stdd, linetype="dashed", color="red") +
    #         geom_hline(yintercept = sre- 2*stdd, linetype="dashed", color="red")
    # }
    # else if(input$y_os == "STROOP - povprečje"){
    #     p <- p + geom_hline(yintercept = mean(pod1$"STROOP - povprečje"))
    # }

    if(input$scatter_smooth=="Ne"){
      ggplotly(p)
    } else {
      if(input$scatter_smooth=="Loessova"){metoda <- "loess"} else {metoda<-"lm"}
      if(input$scatter_IZ!="Ne"){temp=TRUE}else{temp=FALSE}
      r <- p + geom_smooth(aes(fill=z),size=1.5, span=input$scatter_glajenje, se=temp, alpha=0.3, method = metoda)
      ggplotly(r)}

  })
  
  ##### p-vrednosti
  
  output$pPlot <- renderPlotly({
    plot.p.cross <- function(odvisna, neodvisna){
      if(input$barva != "Skupaj"){
        # zac <- filter(dat, dat[,input$barva] == input$lokacije)
        zac <- dat[dat[,input$barva] %in% input$lokacije, ]
      } else {zac <- dat}
      
      hg_lm_lay <- data.frame(lokacija=unique(zac[,input$barva]), SE=NA, p=NA, f_p=NA, r2=NA, pow=NA)
      rownames(hg_lm_lay) <- unique(zac[,input$barva])
      
      for (i in unique(zac[,input$barva])) {
        temp <- filter(zac, zac[,input$barva] %in% c(i))
        # temp <- zac
        odv <- temp[,odvisna]
        neo <- temp[,neodvisna]
        if(is_empty(temp$`T-Hg`)|is_empty(temp$Pb)|is_empty(temp$U)){
          hg_lm_lay[i,"SE"] <- NA
          hg_lm_lay[i,"p"] <- NA
          hg_lm_lay[i,"f_p"] <- NA
        } else {
          temp2 <- lm(odv~neo, zaca = temp)
          temp2 <- summary(temp2)
          hg_lm_lay[i,"SE"] <- temp2$coefficients[2,2]
          hg_lm_lay[i,"p"] <- temp2$coefficients[2,4]
          hg_lm_lay[i,"f_p"] <- pf(temp2$fstatistic[1],
                                   temp2$fstatistic[2],
                                   temp2$fstatistic[3],
                                   lower.tail = FALSE)
          hg_lm_lay[i,"r2"] <- temp2$r.squared
          hg_lm_lay[i,"pow"] <- temp2$r.squared/(1-temp2$r.squared)
        }
      }
      
      # hg_lm_lay
      
      # plot(hg_lm_lay$p)
      # abline(h=0.05, col="red")
      
      temp <- hg_lm_lay
      temp$barva <- ifelse(temp$p<=0.05, "darkgreen", ifelse(temp$p<=0.10, "yellow", "red"))
      p <- ggplot(temp, aes(lokacija, r2)) + geom_bar(stat = "identity", fill=temp$barva) + 
        geom_hline(yintercept = c(1), color=c("green"), lwd=1.1, linetype="dashed") + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
        geom_text(aes(label = round(r2,2)), vjust = -0.5) + 
        labs(title = paste0("R^2 in p-vrednosti linearne regresije odzivne (", odvisna, ") in napovedne (", neodvisna, ")" ),
             caption = "Višina stolpca je r^2 (pojasnjena varianca), \nbarva stolpca pa pomeni p-vrednost (zelena pomeni p<=0.05, rumena p<=0.1, rdeča ostalo")
      ggplotly(p) %>%
        layout(title = list(text = paste0(paste("R^2 in p-vrednosti linearne regresije", odvisna, "(odzivna) in", neodvisna, "(napovedna)" ),
                                          '<br>',
                                          '<sup>',
                                          "Višina stolpca je r^2 (pojasnjena varianca), barva stolpca pa pomeni p-vrednost (zelena pomeni p<=0.05, rumena p<=0.1, rdeča ostalo)",'</sup>')))
      }
    
    plot.p.cross(input$y_os, input$x_os)
  })
  
  ############## ANOVA ################
  #####################################
  
  output$anova_odzivna <- renderUI({
    selectInput("aov_odzivna",
                "Številčna spremenljivka:",
                scat_sprem$stev,
                selected = "T-Hg")
  })
  
  output$anova_skupine <- renderUI({
    selectInput("aov_skupine",
                "Skupine:",
                scat_sprem$skup,
                selected = "Location_2")
  })
  
  output$anova_lokacija <- renderUI({
    selectInput("aov_lokacije",
                "Posamezne lokacije",
                unique(dat[,input$aov_skupine]),
                multiple = TRUE,
                selected = unique(dat[,input$aov_skupine]))
  })
  
  output$anova_boxplot <- renderPlotly({
    temp <- select(dat, c(input$aov_odzivna, input$aov_skupine))
    temp <- temp[temp[,input$aov_skupine] %in% input$aov_lokacije, ]
    colnames(temp) <- c("Odzivna", "Skupine")
    ggplot(temp, aes(Skupine, Odzivna, fill=Skupine)) + geom_boxplot() + labs(title = paste("Boxplot", input$aov_odzivna, "in", input$aov_skupine),
                                                                              x = input$aov_skupine, y = input$aov_odzivna) + 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  })
  
  output$tukey_plot <- renderPlotly({
    temp <- select(dat, c(input$aov_odzivna, input$aov_skupine))
    temp <- temp[temp[,input$aov_skupine] %in% input$aov_lokacije, ]
    stevilska <- temp[,input$aov_odzivna]
    skupine <- temp[,input$aov_skupine]
    tuk_aov <- aov(stevilska~skupine)
    # summary(tuk_aov)
    tuk_plot <- TukeyHSD(tuk_aov)
    
    # B<-as.data.frame(tuk_plot[1])
    # colnames(B)[2:4]<-c("min",
    #                     "max",
    #                     "p")
    # C<-data.frame(id=row.names(B),
    #               min=B$min,
    #               max=B$max,
    #               idt=ifelse(B$p<0.05,
    #                          "significant",
    #                          "not significant")
    # )
    sign <- TRUE
    if(input$anova_izbor=="Vse"){sign <- FALSE}
    p <- GGTukey.2(tuk_plot, sign)
    ggplotly(p)
  })
}

######################## ZAGON ########################################
#######################################################################

shinyApp(ui = ui, server = server)