#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(devtools)
library(gridExtra)
library(shiny)
library(tidyverse)
library(rsconnect)
library(ggplot2)
library(MASS)
library(GGally)
library(dendextend)
library(igraph)
library(ggraph)
library(threejs)
library(plotly)
library(leaflet)

source("https://raw.githubusercontent.com/mateyneykov/315_code_data/master/code/geom_mosaic.R")
studalc <- read.csv("https://raw.githubusercontent.com/norawai/StudentAlcoholConsumption/main/studentAlcoholConsumption.csv")
d1 <- read.csv("https://raw.githubusercontent.com/norawai/StudentAlcoholConsumption/main/studentAlcoholConsumption.csv")
data <- data.frame(read_csv("https://raw.githubusercontent.com/norawai/StudentAlcoholConsumption/main/studentAlcoholConsumption.csv"))

d1_new <- mutate(d1, 
                 age = cut(`age`, c(15, 16, 17, 18, Inf), 
                           labels = c("< 16 Years Old", 
                                      "16 to 17 Years Old", 
                                      "17 to 18 Years Old", 
                                      "> 18 Years Old")))
d1_new <- subset(d1_new, !is.na(age)) 

nwai <-  theme_bw() + 
    theme(text = element_text(size = 10, color = "#5B4E9A"))
# creates StatMosaic object
StatMosaic <- ggproto("StatMosaic", Stat,
                      default_aes = aes(fill=NA),
                      
                      setup_params = function(data, params) {
                          if (!is.null(data$fill) || !is.null(params$fill)) {
                              stop("stat_mosaic() must not be used with a fill aesthetic.", 
                                   call. = FALSE)
                          }
                          params
                      },
                      
                      setup_data = function(data, params) {
                          tab <- table(data$x, data$y)
                          # normalizes row sums
                          prop.tab <- sweep(tab, 1, rowSums(tab), FUN ="/")
                          # assignments colors to fields based on residuals
                          expected <- outer(rowSums(tab),colSums(tab))/sum(tab)
                          # this standardizes the residuals to have a N(0,1) dist
                          standardization <- outer((1 - rowSums(tab)/sum(tab)),
                                                   (1 - colSums(tab)/sum(tab)))
                          std_resid <- (tab-expected)/
                              sqrt(expected*standardization)
                          # unstrandardized residual
                          # std_resid <- (tab-expected)/sqrt(expected)
                          resid_cat <- ifelse(std_resid>4, "r > 4",
                                              ifelse(std_resid>2, "2 < r < 4",
                                                     ifelse(std_resid> -2, "-2 < r < 2",
                                                            ifelse(std_resid> -4, "-4 < r < -2", 
                                                                   "r < -4"))))
                          
                          # find placements for vertical bars
                          bar_edges <- c(0, cumsum(rowSums(tab))/sum(tab)) *
                              nrow(tab) + .5
                          # find breakpoints within bars
                          bar_heights <- cbind(rep(0, nrow(tab)),
                                               matrixStats::rowCumsums(prop.tab)) *
                              ncol(tab) + .5
                          
                          # group parameters for plotting rectangles
                          xmin <- rep(head(bar_edges,-1), each=ncol(tab))
                          xmax <- rep(bar_edges[-1], each=ncol(tab))
                          ymin <- as.vector(t(bar_heights[,-ncol(bar_heights)]))
                          ymax <- as.vector(t(bar_heights[,-1]))
                          col <- factor(t(resid_cat),
                                        levels=c("r > 4","2 < r < 4",
                                                 "-2 < r < 2","-4 < r < -2",
                                                 "r < -4"))
                          data.frame(xmin, xmax, ymin, ymax, col, 
                                     group = 1:length(xmin),
                                     PANEL = rep(1,length(xmin)))
                      },
                      
                      compute_group = function(self, data, scales, params) {
                          cols = colorRampPalette(c("#A99DE2", "#FFFFFF", "#9DC5D0"))(5)
                          transform(data, fill=factor(col,levels=levels(col),
                                                      labels=cols))
                      }
)



mosaic_legend <- function(color1 = "#A99DE2", color2 = "#9DC5D0") {
    palette = c(color1, "#FFFFFF", color2)
    scale_fill_manual(name="Standardized\nResiduals",
                      values=colorRampPalette(palette)(5),
                      labels=c("r > 4", "2 < r < 4",
                               "-2 < r < 2", "-4 < r < -2",
                               "r < -4"),
                      drop = FALSE)
    
}



# wrapper using StatMosaic
geom_mosaic <- function(mapping = NULL, data = NULL,
                        stat = "mosaic", position = "identity",
                        color = "black", ..., na.rm = FALSE,
                        show.legend = TRUE, inherit.aes = TRUE) {
    
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRect,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            color=color,
            ...
        )
    )
}

########################################################################################################################################################################

function(input, output) {
    map = renderLeaflet({
        m <- leaflet() %>%
            addTiles() %>% 
            addMarkers(lat=38.5737898, lng=-7.9460654, popup="Gabriel Pereira") %>% 
            addMarkers(lat=38.626664, lng=-8.9370019, popup="Mousinho da Silveira")
        print(m) 
    })
    hist = renderPlot({
                        urban <- subset(studalc, address == "U")
                        rural <- subset(studalc, address == "R")
        
                        urban_count <- urban %>%
                            group_by(Walc) %>%
                            summarize(count = n()) %>%
                            mutate(total = sum(count),  #  add total number of observations
                                   proportion = count / total,  #  add proportions
                                   percentage = proportion * 100,
                                   address = "Urban")  
        
                        rural_count <- rural %>%
                            group_by(Walc) %>%
                            summarize(count = n()) %>%
                            mutate(total = sum(count),  #  add total number of observations
                                   proportion = count / total,  #  add proportions
                                   percentage = proportion * 100,
                                   address = "Rural")  
                        
                        addressagg <- rbind(urban_count, rural_count)
        
                        stud_count <- studalc %>%
                            group_by(Walc) %>%
                            summarize(count = n()) %>%
                            mutate(total = sum(count),
                                   proportion = count / total,
                                   percentage = proportion * 100)
                        
                        hkbar <- ggplot(stud_count, aes(x = Walc, y = percentage)) +
                            geom_bar(stat = "identity", fill = "#A99DE2") + 
                            labs(title = "Percentage of Students for each Category for Weekend Alcohol Consumption",
                                 subtitle = "1 - Very Low, 5 - Very High",
                                 x = "Weekend Alcohol Consumption Level",
                                 y = "Percentage") + nwai
                        
                        if (input$address) {
                            hkbar <- ggplot(addressagg, aes(x = Walc, y = percentage, fill = address)) +
                                geom_bar(stat = "identity", position = "dodge") +
                                scale_fill_manual(values = c("#B3CED6","#BBB4A6")) + 
                                labs(title = "Percentage of Students for each Category for Weekend Alcohol Consumption by Home",
                                     subtitle = "1 - Very Low, 5 - Very High",
                                     x = "Weekend Alcohol Consumption Level",
                                     y = "Percentage",
                                     fill = "Home Environment") + nwai
                        }
                        print(hkbar)})
    
    dend = renderPlot({
                        # select variables and create corr matrix
                        stud_cont <- dplyr::select(studalc, age, Medu, Fedu, traveltime,
                                                   studytime, failures, famrel, freetime,
                                                   goout, Dalc, Walc, health, absences,
                                                   G1, G2, G3)
                        correlation_matrix <- cor(stud_cont)
                        # transform correlations and convert to distance matrix -----------------------
                        cor_mat <- 1 - abs(correlation_matrix)
                        dist_mat <- as.dist(cor_mat)
                        # submit to hierarchical clustering and create dendrogram ---------------------
                        stud_clust <- hclust(dist_mat)
                        stud_dend <- as.dendrogram(stud_clust)
                        hkdend <- stud_dend %>% set("branches_k_color", k = 4) %>% as.ggdend(.) %>%
                            ggplot(horiz = T, hjust = 2) + labs(title = "Dendrogram of Student Alcohol Variables",
                                                                y = "Pairwise Euclidean Distance",
                                                                x = element_blank()) + nwai
                            theme(axis.text.y = element_blank(),
                                  axis.ticks = element_blank()) 
                        
                        if (input$orientation == "Vertical") {
                            hkdend <- stud_dend %>% set("branches_k_color", k = 4) %>% as.ggdend(.) %>%
                                ggplot(horiz = F, hjust = 2) + labs(title = "Dendrogram of Student Alcohol Variables",
                                                                    y = "Pairwise Euclidean Distance",
                                                                    x = element_blank()) +
                                nwai + theme(axis.text.x = element_blank(),
                                      axis.ticks = element_blank()) 
                        }
                        print(hkdend)
                        })
    
    scatter = renderPlotly({
            if (input$color_code == "School") {
                d1$school[which(d1$school == "GP")] <- 'Gabriel Pereira'
                d1$school[which(d1$school == "MS")] <- 'Mousinho da Silveira'
                d1$school <- as.factor(d1$school)
                fig <- plot_ly(d1, x = ~G1, y = ~G2, z = ~G3, color = ~school, colors = c('#A99DE2', '#7DB0BF'), size = 0.5)
                fig <- fig %>% add_markers()
                fig <- fig %>% layout(scene = list(xaxis = list(title = 'First Semester Grade',color = "#5B4E9A"),
                                                   yaxis = list(title = 'Second Semester Grade',color = "#5B4E9A"),
                                                   zaxis = list(title = 'Final Grade',color = "#5B4E9A")), 
                                                   title = "Scatterplot of Grades Based on School")
                print(fig)
            }
            else if (input$color_code == "Address") {
                d1$address[which(d1$address == "U")] <- 'Urban'
                d1$address[which(d1$address == "R")] <- 'Rural'
                d1$address <- as.factor(d1$address)
                fig2 <- plot_ly(d1, x = ~G1, y = ~G2, z = ~G3, color = ~address, colors = c('#A99DE2', '#7DB0BF'), size = 0.5)
                fig2 <- fig2 %>% add_markers()
                fig2 <- fig2 %>% layout(scene = list(xaxis = list(title = 'First Semester Grade', color = "#5B4E9A"),
                                                     yaxis = list(title = 'Second Semester Grade', color = "#5B4E9A"),
                                                     zaxis = list(title = 'Final Grade', color = "#5B4E9A")), 
                                                     title = "Scatterplot of Grades Based on Address")
                print(fig2)
            }
            else if (input$color_code == "Gender") {
                d1$sex[which(d1$sex == "F")] <- 'Female'
                d1$sex[which(d1$sex == "M")] <- 'Male'
                d1$sex <- as.factor(d1$sex)
                fig3 <- plot_ly(d1, x = ~G1, y = ~G2, z = ~G3, color = ~sex, colors = c('#A99DE2', '#7DB0BF'), size = 0.5)
                fig3 <- fig3 %>% add_markers()
                fig3 <- fig3 %>% layout(scene = list(xaxis = list(title = 'First Semester Grade',color = "#5B4E9A"),
                                                     yaxis = list(title = 'Second Semester Grade',color = "#5B4E9A"),
                                                     zaxis = list(title = 'Final Grade',color = "#5B4E9A")), 
                                                     title = "Scatterplot of Grades Based on Gender")
                print(fig3)
            }
            else if (input$color_code == "Activity Participation") {
                d1$activities[which(d1$activities == "yes")] <- 'Participate'
                d1$activities[which(d1$activities == "no")] <- 'Do Not Participate'
                d1$activities <- as.factor(d1$activities)
                fig4 <- plot_ly(d1, x = ~G1, y = ~G2, z = ~G3, color = ~activities, colors = c('#A99DE2', '#7DB0BF'), size = 0.5)
                fig4 <- fig4 %>% add_markers()
                fig4 <- fig4 %>% layout(scene = list(xaxis = list(title = 'First Semester Grade',color = "#5B4E9A"),
                                                     yaxis = list(title = 'Second Semester Grade',color = "#5B4E9A"),
                                                     zaxis = list(title = 'Final Grade',color = "#5B4E9A")),
                                                     title = "Scatterplot of Grades Based on Activity Particiaption ")
                print(fig4)
            }
            else if (input$color_code == "Relationship Status") {
                d1$romantic[which(d1$romantic == "yes")] <- 'In Relationship'
                d1$romantic[which(d1$romantic == "no")] <- 'Not In Relationship'
                d1$romantic <- as.factor(d1$romantic)
                fig5 <- plot_ly(d1, x = ~G1, y = ~G2, z = ~G3, color = ~romantic, colors = c('#A99DE2', '#7DB0BF'), size = 0.5)
                fig5 <- fig5 %>% add_markers()
                fig5 <- fig5 %>% layout(scene = list(xaxis = list(title = 'First Semester Grade',color = "#5B4E9A"),
                                                     yaxis = list(title = 'Second Semester Grade',color = "#5B4E9A"),
                                                     zaxis = list(title = 'Final Grade',color = "#5B4E9A")), 
                                                     title = "Scatterplot of Grades Based on Relationship Status")
                print(fig5)
            }
            else if (input$color_code == "Day Alcohol") {
                d1$Dalc[which(d1$Dalc == "1")] <- '1 - very low'
                d1$Dalc[which(d1$Dalc == "2")] <- '2'
                d1$Dalc[which(d1$Dalc == "3")] <- '3'
                d1$Dalc[which(d1$Dalc == "4")] <- '4'
                d1$Dalc[which(d1$Dalc == "5")] <- '5 - very high'
                d1$Dalc <- as.factor(d1$Dalc)
                fig6 <- plot_ly(d1, x = ~G1, y = ~G2, z = ~G3, color = ~Dalc, colors = c("#D09C62", "#A99DE2", "#E0AEC7", "#B3CED6", 
                                                                                         "#BBB4A6"), size = 0.5)
                fig6 <- fig6 %>% add_markers()
                fig6 <- fig6 %>% layout(scene = list(xaxis = list(title = 'First Semester Grade',color = "#5B4E9A"),
                                                     yaxis = list(title = 'Second Semester Grade',color = "#5B4E9A"),
                                                     zaxis = list(title = 'Final Grade',color = "#5B4E9A")), 
                                                     title = "Scatterplot of Grades Based on Day Alcohol")
                print(fig6)
            }
            else if (input$color_code == "Weekend Alcohol") {
                d1$Walc[which(d1$Walc == "1")] <- '1 - very low'
                d1$Walc[which(d1$Walc == "2")] <- '2'
                d1$Walc[which(d1$Walc == "3")] <- '3'
                d1$Walc[which(d1$Walc == "4")] <- '4'
                d1$Walc[which(d1$Walc == "5")] <- '5 - very high'
                d1$Walc <- as.factor(d1$Walc)
                fig7 <- plot_ly(d1, x = ~G1, y = ~G2, z = ~G3, color = ~Walc, colors = c("#D09C62", "#A99DE2", "#E0AEC7", "#B3CED6", 
                                                                                         "#BBB4A6"), size = 0.5)
                fig7 <- fig7 %>% add_markers()
                fig7 <- fig7 %>% layout(scene = list(xaxis = list(title = 'First Semester Grade',color = "#5B4E9A"),
                                                     yaxis = list(title = 'Second Semester Grade',color = "#5B4E9A"),
                                                     zaxis = list(title = 'Final Grade',color = "#5B4E9A")), 
                                                     title = "Scatterplot of Grades Based on Weekend Alcohol")
                print(fig7)
            }
        })
        
    mosaic1 = renderPlot({
            if (input$breakdown == "Family Relationships") {
                plot <- ggplot(d1, aes(x = famrel, y = Dalc)) +
                    geom_mosaic() + 
                    mosaic_legend() + labs(title = "Mosaic Plot of Family Relationships Vs. Rating of Weekday Drinking", 
                                           x = input$breakdown, 
                                           y = "Day Alcohol") + nwai 
                    theme(axis.text.x = element_text(angle = 15, hjust = 1))
                print(plot)}
            else if (input$breakdown =="Romantic Relationships") {
                plot <- ggplot(d1, aes(x = romantic, y = Dalc)) +
                    geom_mosaic() + 
                    mosaic_legend() + labs(title = "Mosaic Plot of Romantic Relationships Vs. Rating of Weekday Drinking", 
                                             x = input$breakdown, 
                                             y = "Day Alcohol") + nwai + 
                    theme(axis.text.x = element_text(angle = 15, hjust = 1))
                print(plot)}
            else if (input$breakdown == "Family Educational Support") {
                plot <- ggplot(d1, aes(x = famsup, y = Dalc)) +
                    geom_mosaic() + 
                    mosaic_legend() + labs(title = "Mosaic Plot of Family Educational Support Vs. Rating of Weekday Drinking", 
                                             x = input$breakdown, 
                                             y = "Day Alcohol") + nwai + 
                    theme(axis.text.x = element_text(angle = 15, hjust = 1))
                print(plot)}
            else if (input$breakdown == "Age") {
                plot <- ggplot(d1_new, aes(x = age, y = Dalc)) +
                    geom_mosaic() + 
                    mosaic_legend() + labs(title = "Mosaic Plot of Age Vs. Rating of Weekday Drinking", 
                                           x = input$breakdown, 
                                           y = "Day Alcohol") +nwai + 
                    theme(axis.text.x = element_text(angle = 15, hjust = 1))
                print(plot)}
        })
    mosaic2 = renderPlot({
    if (input$breakdown2 == "Family Relationships") {
        plot2 <- ggplot(d1, aes(x = famrel, y = Walc)) +
            geom_mosaic() + 
            mosaic_legend() + labs(title = "Mosaic Plot of Family Relationships Vs. Rating of Weekday Drinking", 
                                   x = input$breakdown2, 
                                   y = "Weekend Alcohol") +nwai + 
            theme(axis.text.x = element_text(angle = 15, hjust = 1))
        print(plot2)}
    else if (input$breakdown2 == "Romantic Relationships") {
        plot2 <- ggplot(d1, aes(x = romantic, y = Walc)) +
            geom_mosaic() + 
            mosaic_legend() + labs(title = "Mosaic Plot of Romantic Relationships Vs. Rating of Weekday Drinking", 
                                   x = input$breakdown2, 
                                   y = "Weekend Alcohol") +nwai + 
            theme(axis.text.x = element_text(angle = 15, hjust = 1))
        print(plot2)}
    else if (input$breakdown2 == "Family Educational Support") {
        plot2 <- ggplot(d1, aes(x = famsup, y = Walc)) +
            geom_mosaic() + 
            mosaic_legend() +  labs(title = "Mosaic Plot of Family Educational Support Vs. Rating of Weekday Drinking", 
                                    x = input$breakdown2, 
                                    y = "Weekend Alcohol") +nwai + 
            theme(axis.text.x = element_text(angle = 15, hjust = 1))
        print(plot2)}
    else if (input$breakdown2 == "Age") {
        plot2 <- ggplot(d1_new, aes(x = age, y = Walc)) +
            geom_mosaic() + 
            mosaic_legend() + labs(title = "Mosaic Plot of Age Vs. Rating of Weekday Drinking", 
                                   x = input$breakdown2, 
                                   y = "Weekend Alcohol") + nwai + 
            theme(axis.text.x = element_text(angle = 15, hjust = 1)) 
        print(plot2)}
    })

    boxplot1 = renderPlotly({
        if (input$var1 == "G1") {
            
            fig2 <- ggplot(data = d1,aes(x = Dalc,y = G1)) + 
                geom_boxplot(position = "dodge") + 
                xlim(0,6) + 
                stat_summary(fun.y = median, geom = 'line', color = "#A99DE2") + 
                labs(x = "Workday Alcohol Consumption", 
                     y = "First Grade Period", 
                     title = "First Grades vs Workday Alcohol Consumption") +nwai 
            ggplotly(fig2)
            
        }
        else if (input$var1 == "G2") {
            
            fig2 <- ggplot(data = d1,aes(x = Dalc,y = G2)) + 
                geom_boxplot(position = "dodge") + 
                xlim(0,6) + 
                stat_summary(fun.y = median, geom = 'line', color = "#A99DE2") + 
                labs(x = "Workday Alcohol Consumption", 
                     y = "Second Grade Period", 
                     title = "Second Grades vs Workday Alcohol Consumption") +nwai 
            ggplotly(fig2)
            
            
        }
        else if (input$var1 == "G3" ) {
            
            fig2 <- ggplot(data = d1,aes(x = Dalc,y = G3)) + 
                geom_boxplot(position = "dodge") + 
                xlim(0,6)+ 
                stat_summary(fun.y = median, geom = 'line', color = "#A99DE2") + 
                labs(x = "Workday Alcohol Consumption", 
                     y = "Final Grade Period", 
                     title = "Final Grades vs Workday Alcohol Consumption") + nwai 
            ggplotly(fig2)
            
        }})
    boxplot2 = renderPlotly({
        if (input$var2 == "G1" ) {
        
        fig2 <- ggplot(data = d1,aes(x = Walc,y = G1)) + 
            geom_boxplot(position = "dodge") + 
            xlim(0,6)+ 
            stat_summary(fun.y = median, geom = 'line', color = "#A99DE2") + 
            labs(x = "Weekend Alcohol Consumption", 
                 y = "First Grade Period", 
                 title = "First Grades vs Weekend Alcohol Consumption") + nwai
        ggplotly(fig2)
        
    }
        else if (input$var2 == "G2" ) {
            
            fig2 <- ggplot(data = d1,aes(x = Walc,y = G2)) + 
                geom_boxplot(position = "dodge") + 
                xlim(0,6)+ 
                stat_summary(fun.y = median, geom = 'line', color = "#A99DE2") + 
                labs(x = "Weekend Alcohol Consumption", 
                     y = "Second Grade Period", 
                     title = "Second Grades vs Weekend Alcohol Consumption") + nwai
            ggplotly(fig2)
        }
        else if (input$var2 == "G3" ) {
            
            fig2 <- ggplot(data = d1,aes(x = Walc,y = G3)) + 
                geom_boxplot(position = "dodge") + 
                xlim(0,6)+ 
                stat_summary(fun.y = median, geom = 'line', color = "#A99DE2") + 
                labs(x = "Weekend Alcohol Consumption", 
                     y = "Final Grade Period", 
                     title = "Final Grades vs Weekend Alcohol Consumption") + nwai
            ggplotly(fig2)
        }})
    
    
    heatmap1 =  renderPlot({
        
        data_male <- subset(data, sex == "M")
        
        if(input$grade == "First Period")
        {male <- ggplot(data_male, aes(x = absences, y = G1)) + 
            stat_density2d(aes(fill = ..density..), geom = "tile", contour = F, h=c(input$bw_adjustx,input$bw_adjusty)) +
            scale_fill_gradient2(low = "#40B0A6", mid = "#E1BE6A") +
            labs(title = "Heat Map of School Absences vs First Period Grade for Males",
                 x = "Number of School Absences",
                 y = "First Period Grade") + nwai}
        
        if(input$grade == "Second Period")
        {male <- ggplot(data_male, aes(x = absences, y = G2)) + 
            stat_density2d(aes(fill = ..density..), geom = "tile", contour = F, h=c(input$bw_adjustx,input$bw_adjusty)) +
            scale_fill_gradient2(low = "#40B0A6", mid = "#E1BE6A") +
            labs(title = "Heat Map of School Absences vs Second Period Grade for Males",
                 x = "Number of School Absences",
                 y = "Second Period Grade") + nwai}
        
        if(input$grade == "Final")
        {male <- ggplot(data_male, aes(x = absences, y = G3)) + 
            stat_density2d(aes(fill = ..density..), geom = "tile", contour = F, h=c(input$bw_adjustx,input$bw_adjusty)) +
            scale_fill_gradient2(low = "#40B0A6", mid = "#E1BE6A") +
            labs(title = "Heat Map of School Absences vs Final Grade for Males",
                 x = "Number of School Absences",
                 y = "Final Grade") + nwai}
        
        data_female <- subset(data, sex == "F")
        
        if(input$grade == "First Period")
        {female <- ggplot(data_female, aes(x = absences, y = G1)) + 
            stat_density2d(aes(fill = ..density..), geom = "tile", contour = F, h=c(input$bw_adjustx,input$bw_adjusty)) +
            scale_fill_gradient2(low = "#40B0A6", mid = "#E1BE6A") +
            labs(title = "Heat Map of School Absences vs First Period Grade for Females",
                 x = "Number of School Absences",
                 y = "First Period Grade") + nwai}
        
        if(input$grade == "Second Period")
        {female <- ggplot(data_female, aes(x = absences, y = G2)) + 
            stat_density2d(aes(fill = ..density..), geom = "tile", contour = F, h=c(input$bw_adjustx,input$bw_adjusty)) +
            scale_fill_gradient2(low = "#40B0A6", mid = "#E1BE6A") +
            labs(title = "Heat Map of School Absences vs Second Period Grade for Females",
                 x = "Number of School Absences",
                 y = "Second Period Grade") + nwai}
        
        if(input$grade == "Final")
        {female <- ggplot(data_female, aes(x = absences, y = G3)) + 
            stat_density2d(aes(fill = ..density..), geom = "tile", contour = F, h=c(input$bw_adjustx,input$bw_adjusty)) +
            scale_fill_gradient2(low = "#40B0A6", mid = "#E1BE6A") +
            labs(title = "Heat Map of School Absences vs Final Grade for Females",
                 x = "Number of School Absences",
                 y = "Final Grade") + nwai}
        
        print(grid.arrange(male,female))
    })
    
    heatmap2 = renderPlot({
        
        data_urban <- subset(data, address == "U")
        
        if(input$grade2 == "First Period")
        {urban <- ggplot(data_urban, aes(x = absences, y = G1)) + 
            stat_density2d(aes(fill = ..density..), geom = "tile", contour = F, h=c(input$bw_adjustx2,input$bw_adjusty2)) +
            scale_fill_gradient2(low = "purple4", mid = "pink") +
            labs(title = "Heat Map of School Absences vs First Period Grade for Urban Students",
                 x = "Number of School Absences",
                 y = "First Period Grade") + nwai}
        
        if(input$grade2 == "Second Period")
        {urban <- ggplot(data_urban, aes(x = absences, y = G2)) + 
            stat_density2d(aes(fill = ..density..), geom = "tile", contour = F, h=c(input$bw_adjustx2,input$bw_adjusty2)) +
            scale_fill_gradient2(low = "purple4", mid = "pink") +
            labs(title = "Heat Map of School Absences vs Second Period Grade for Urban Students",
                 x = "Number of School Absences",
                 y = "Second Period Grade") + nwai}
        
        if(input$grade2 == "Final")
        {urban <- ggplot(data_urban, aes(x = absences, y = G3)) + 
            stat_density2d(aes(fill = ..density..), geom = "tile", contour = F, h=c(input$bw_adjustx2,input$bw_adjusty2)) +
            scale_fill_gradient2(low = "purple4", mid = "pink") +
            labs(title = "Heat Map of School Absences vs Final Grade for Urban Students",
                 x = "Number of School Absences",
                 y = "Final Grade") + nwai}
        
        data_rural <- subset(data, address == "R")
        
        if(input$grade2 == "First Period")
        {rural <- ggplot(data_rural, aes(x = absences, y = G1)) + 
            stat_density2d(aes(fill = ..density..), geom = "tile", contour = F, h=c(input$bw_adjustx2,input$bw_adjusty2)) +
            scale_fill_gradient2(low = "purple4", mid = "pink") +
            labs(title = "Heat Map of School Absences vs First Period Grade for Rural Students",
                 x = "Number of School Absences",
                 y = "First Period Grade") + nwai}
        
        if(input$grade2 == "Second Period")
        {rural <- ggplot(data_rural, aes(x = absences, y = G2)) + 
            stat_density2d(aes(fill = ..density..), geom = "tile", contour = F, h=c(input$bw_adjustx2,input$bw_adjusty2)) +
            scale_fill_gradient2(low = "purple4", mid = "pink") +
            labs(title = "Heat Map of School Absences vs Second Period Grade for Rural Students",
                 x = "Number of School Absences",
                 y = "Second Period Grade") + nwai}
        
        if(input$grade2 == "Final")
        {rural <- ggplot(data_rural, aes(x = absences, y = G3)) + 
            stat_density2d(aes(fill = ..density..), geom = "tile", contour = F, h=c(input$bw_adjustx2,input$bw_adjusty2)) +
            scale_fill_gradient2(low = "purple4", mid = "pink") +
            labs(title = "Heat Map of School Absences vs Final Grade for Rural Students",
                 x = "Number of School Absences",
                 y = "Final Grade") + nwai}
        
        print(grid.arrange(urban,rural))
    })
################################################################################################################################################################
    output$map <- map
    
    # hk's barplot
    output$hk_barplot <- hist
    
    # hk's dendrogram    
    output$hk_dend <- dend

    #nora's scatterplot
    output$nora_scatter <- scatter
    
    #nora's mosaic plot
    output$nora_mosaic1 <- mosaic1
    output$nora_mosaic2 <- mosaic2
    
    # austin box plot
    output$weekday <- boxplot1
    output$weekend <- boxplot2
    
    # michelle heat maps 
    output$m_plot <- heatmap1
    
    # michelle heat maps 
    output$m2_plot <- heatmap2

}
