#------------------------------
#Name:    Animesh Patel
#Date:    Feb-25, 2021
#-----------------------------
############################## 
# Step: 0 - Load libraries
##############################
library("tidyverse")
#install.packages("epitools", dependencies = TRUE)
library("epitools", warn.conflicts = FALSE, quietly = TRUE)

##############################
# Step: 1 - Get Data into R
##############################
#Set working Directory
#setwd("F:/Animesh/Learning/R/R_Working_Directory")
adae <- read.csv(file = "D:/Animesh/SAS Paper Writing/PhuseConnect 2022/VolcanoPlotSample.csv", 
                 fileEncoding="UTF-8-BOM", 
                 blank.lines.skip = TRUE,
                 header = TRUE)

adae <- read.csv(file = "D:/Animesh/SAS Paper Writing/PhuseConnect 2022/adae2.csv", 
                 fileEncoding="UTF-8-BOM", 
                 blank.lines.skip = TRUE,
                 skipNul = TRUE,
                 header = TRUE)

adae1 <- adae %>%
  mutate(trt01p = ifelse(row_number()<300, "CMP-135", "Placebo"  ))
adae <- rbind(adae1,adae1,adae1)

#------------------------------------
trt_list <- c("CMP-135", "Placebo")

#Data Manipulation
names(adae) <- tolower(names(adae))
adae2 <- adae %>%
  select(usubjid,aeterm, aedecod, aebodsys, trt01p)%>% 
  filter( trt01p %in% c("CMP-135", "Placebo") ) %>%
  mutate(event="Yes") #to all events

adsl <- distinct(adae2 , usubjid, .keep_all = FALSE)

#Dummy ADAE with all combi.
ae1 <- distinct(adae , aedecod, .keep_all = FALSE)
ae2 <- merge(ae1, adsl) #Cartetian Product 
ae3 <- merge(ae2, adae2, by.y = c('usubjid', "aedecod"),all.x = TRUE ) #Cartetian Product 

#------------------------------------
#ggplot2
# Works with dataframe only.
# keep enhancing/adding layer on existing plot created using ggplot() function


frq1 <- data.frame(xtabs(  ~ aedecod + trt01p, data = adae2 ))
frq2 <-data.frame (xtabs(  ~ trt01p, data = adae2 ))

freq3 <- merge(frq1, frq2, by="trt01p")
freq4 <- freq3 %>%
  mutate( no = Freq.y - Freq.x,
          yes = Freq.x,
          odds = yes/no,
          trt01p=if_else(trt01p =="CMP-135","cmp_135","placebo")) %>%
  arrange(aedecod, trt01p) %>%
  select(c(trt01p, aedecod, yes, no)) %>%
  gather(ae_yn, value, -(trt01p:aedecod ) ) %>%
  spread( trt01p, value ) %>%
  arrange(aedecod, desc(ae_yn) )

#----------Ready Dataset --------------
#----------Get Odds Ratio and P Value --------------
dfx_all <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("aedecod", "pval", "ci_1","ci_2","or"))    

for (p in unique( freq4$aedecod) ) {
  df1<-  subset(freq4, aedecod == p, select = ( c(-aedecod, -ae_yn) ) )
  stat1<- fisher.test(df1)
  
  dfx <- data.frame(aedecod=p,
                    pval=stat1[[1]],
                    ci_1=stat1[[2]][1],
                    ci_2=stat1[[2]][2],
                    or = stat1[[3]][1],
                    row.names = NULL)
  #print(p)
  dfx_all <- rbind (dfx_all,dfx)
}


#Volcano PLot
# Y > -log10(pvalue)
# x=log2FoldChange

#Getting dataset ready for Volcano Plot

vplot1 <- dfx_all %>%
  mutate( logpval = -log10(pval),
          logfold = log2(or) ,
          aelable = ifelse(
            (logfold > 0.6 | logfold < -0.6) & (logpval > 1.5),
            aedecod, ""
          )) %>%
  filter( is.finite (logfold) & is.finite (logpval)  )


#Ploting
#-Complete-
ggplot(vplot1,
       aes(x=logfold, y=logpval, label = aedecod )) + 
  geom_point(aes(colour =aelable)) +  
  labs(title = "Volcano Plot", subtitle = "Safety Pop",
       x="Odds Ratio",
       y="P Value") + 
  theme_minimal() +
  geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red") +
  coord_cartesian(xlim = c(-4,10) , 
                  ylim = c(0,5)) +
  geom_text(aes(label = aelable), size = 3)

  scale_color_manual(values=c("blue", "black", "red")) 


ggplot(data=vplot1, aes(x=logfold, y=logpval )) + 
  geom_point() + 
  theme_minimal() 

