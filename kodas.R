#1 grafikas, 4 skaidre  

install.packages("palmerpenguins") 

install.packages("ggplot2") 

library(palmerpenguins) 

data(penguins) 

library(ggplot2) 

library(palmerpenguins) 

ggplot(data = penguins) + 
  
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) + 
  
  scale_color_manual(values = c('#057314', '#ff0021', '#bf5ccb'),na.translate = FALSE,  
                     
                     name = "Pingvino rūšis", 
                     
                     labels = c("Adelės", "Papuasinis", "Antarktinis")) + 
  
  labs(title = " ", x = "Snapo ilgis (mm)", 
       
       y = "Kūno masė (g)",color = "Pingvino rūšis") + 
  
  labs(title = "Pingvino snapo ilgio palyginimas su kūno mase")+ 
  
  annotate("text", x = 220, y = 3500, label = " ", fontface = "italic", size = 4, angle = 30) 



#2 grafikas, 5 skaidre  

data(penguins) 

pairs(penguins[,3:5], pch = 19) 

df <- penguins[3:5] 

species <- penguins[, 5] 

pCol <- c('#057314', '#ff0021', '#bf5ccb')  

colnames(df) <- c("Snapo_ilgis_mm", "Snapo_plotis_mm", "Peleko_ilgis_mm")  

pairs(df, col = pCol[penguins$species]) 

legend(8,5, fill = c('#057314', '#ff0021', '#bf5ccb') ,  
       
       legend = c("Adelie" = "Adelės",  
                  
                  "Gentoo" = "Papuasinis",  
                  
                  "Chinstrap" = "Antarktinis")) 



#3 grafikas, 6 skaidre 

library(ggplot2) 

data(penguins) 

ggplot(data = penguins, aes(x = flipper_length_mm)) + 
  
  geom_histogram(aes(fill = species), alpha = 0.5, position = "identity") + 
  
  scale_fill_manual(values = c('#057314', '#ff0021', '#bf5ccb'),  name = "Pingvinų rūšis", 
                    
                    breaks = c("Adelie", "Gentoo", "Chinstrap"), 
                    
                    labels = c("Adelės", "Papuasinis", "Antarktinis")) + 
  
  labs(x = "Pelekų ilgis (mm)", y = "Skaičius", title = "Pingvinų skaičiaus palyginimas") 



#4 grafikas, 7 skaidre 

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  
  geom_point(aes(color = sex)) + 
  
  scale_color_manual(values = c("red","blue"), na.translate = FALSE, name = "Pingvino lytis", labels = c("Patelė", "Patinas")) + 
  
  labs(title = "Pingvino snapas ir kūno masė", x = "Snapo ilgis (mm)", y = "Kūno masė (g)",  
       
       color = "Pingviniuko lytis") + 
  
  theme(legend.position = "bottom", plot.title.position = "plot", plot.caption = element_text(hjust = 0, face= "italic"), plot.caption.position = "plot") + 
  
  geom_smooth(method = "lm", se = FALSE, color = "black")+ 
  
  facet_wrap(~species, labeller = labeller(species = c("Adelie" = "Adelės pingvinas", "Gentoo" = "Papuasinis pingvinas", "Chinstrap" = "Antarktinis pingvinas"))) 



#5 grafikas, 8 skaidre 

library(palmerpenguins)  

data(penguins) 

colnames(penguins)[3] <- "Snapo ilgis (mm)" 

colnames(penguins)[4] <- "Snapo plotis (mm)" 

colnames(penguins)[5] <- "Peleko ilgis (mm)" 

colnames(penguins)[6] <- "Kūno svoris (g)" 

install.packages("GGally") 

library(GGally) 

p <- ggparcoord(data = penguins, 
                
                groupColumn = 1, 
                
                scale = "std", 
                
                columns = c(3,4,5,6), 
                
                title = "Pingvinų išvaizdos savybės", alphaLines = 0.3)+ 
  
  xlab('Tiriamas požymis') + ylab('Reikšmė') 

p <- p + scale_color_manual(name = "Pingvinų rūšis", 
                            
                            values = c('#057314', '#ff0021', '#bf5ccb'),   
                            
                            labels = c("Adelės", "Papuasinis", "Antarktinis")) 

p <- p + theme_minimal() 

# Sumazinamos x ir y asiu parastes 

#p <- p + scale_y_continuous(expand = c(0.02, 0.02)) 

p <- p + scale_x_discrete(expand = c(0.02, 0.02)) 

# Isvalome asies zymes 

p <- p + theme(axis.ticks = element_blank()) 

#p <- p + theme(axis.title = element_blank()) 

#p <- p + theme(axis.text.y = element_blank()) 

# Isvalome asiu tinkleli 

p <- p + theme(panel.grid.minor = element_blank()) 

p <- p + theme(panel.grid.major.y = element_blank()) 

# Patamsinamos vertikalios linijos 

p <- p + theme(panel.grid.major.x = element_line(color = "#bbbbbb")) 

p <- p + theme(plot.title = element_text(size=18, hjust = 0.5, face="bold"), 
               
               legend.text = element_text(size = 12), 
               
               axis.text.x = element_text(size = 12, color = "black"), 
               
               axis.text.y = element_text(size = 12, color = "black"), 
               
               axis.title.x = element_text(size = 14), 
               
               axis.title.y = element_text(size = 14)) 

p <- p+guides(color = guide_legend(override.aes = list(size = 3))) 

print(p) 



#6 grafikas, 9 skaidre 

library(palmerpenguins) 



data(penguins) 

penguins_df <- as.data.frame(penguins) 

penguins_df <-na.omit(penguins_df) 

penguins_df <- as.data.frame(penguins) 

penguins_df <-na.omit(penguins_df) 

adelie <- head(penguins_df[penguins_df$species == "Adelie", ], 5) 

gentoo <- head(penguins_df[penguins_df$species == "Gentoo", ], 5) 

chinstrap <- head(penguins_df[penguins_df$species == "Chinstrap", ], 5) 

penguins2<- rbind(adelie, gentoo, chinstrap) 

penguins2 <- penguins2[, 3:5] 



colnames(penguins2)[1] <- "Snapo ilgis (mm)" 

colnames(penguins2)[2] <- "Snapo plotis (mm)" 

colnames(penguins2)[3] <- "Peleko ilgis (mm)" 



stars(penguins2, labels = row.names(penguins2), 
      
      len = 0.7, key.loc = c(7, 0.9), 
      
      main = "Pingvinų rūšių išvaizdos savybės", sub = "subtitle", draw.segments = TRUE, 
      
      frame.plot = F, nrow = 3, cex = .7, col.segments = c('#84291e', '#D67e2a', '#D6b52a')) 



text(x = 6, y = 7.7, labels = "Adelės pingvinai", adj = 0, cex = 0.8) 

text(x = 6, y = 5.4, labels = "Papuasiniai pingvinai", adj = 0, cex = 0.8) 

text(x = 6, y = 3.2, labels = "Antarktiniai pingvinai", adj = 0, cex = 0.8) 







#7 grafikas 10 skaidrė 

install.packages("corrplot") 

library(corrplot) 

peng <- na.omit(penguins) 

duomenys <-data.frame("Snapo_ilgis_mm" = peng$bill_length_mm, 
                      
                      "Amžius" = peng$year, "Snapo_plotis_mm" = peng$bill_depth_mm, 
                      
                      "Peleko_ilgis_mm" = peng$flipper_length_mm, 
                      
                      "Kūno_masė_g" = peng$body_mass_g) 



M <- cor(duomenys) 

corrplot(M, tl.col = 'black') 







#8 grafikas 11 skaidrė 

if(!require('corrplot')) { 
  
  install.packages('corrplot') 
  
  library('corrplot') 
  
} 

library(corrplot) 

peng<-na.omit(penguins) 

duomenys<-data.frame("Snapo_ilgis_mm" =peng$bill_length_mm,"Amžius" =peng$year, 
                     
                     "Snapo_plotis_mm" = peng$bill_depth_mm, 
                     
                     "Peleko_ilgis_mm"= peng$flipper_length_mm,"Kūno_masė_g" =peng$body_mass_g) 



M <-cor(duomenys) 

col <-colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF","#77AADD", "#4477AA")) 

corrplot(M, method = 'color', col = col(200), type ='upper',tl.col= 'black', 
         
         cl.cex= 0.8, order = 'hclust',addCoef.col= 'black', 
         
         number.cex= 0.7,number.digits= 2,diag=FALSE,tl.srt= 90, 
         
         addColorbar= TRUE,colorbar= 'top', 
         
         col.order= 'hclust') 





#Duomenys 

install.packages("WriteXLS") 

library(WriteXLS) 

WriteXLS(penguins, ExcelFileName = "penguins.xlsx") 



