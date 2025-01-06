---
title: 'Assignemnt3'
output:
date: "2024-11-11"


---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

remove(list=ls())
cat('\014')

# Load packages
packs <- c("tidyverse", "sf", "spdep", "sfdep", "spData", "gridExtra", "viridis")
lapply(packs, require, character.only = TRUE)
```

###Loading the data 
```{r read data}
penn.sf<-st_read('/cloud/project/PennLC/PennLungCancer.shp')

#penn.sf<-st_read(file.choose())

penn.sf <- st_as_sf(penn.sf, coords=c('X','Y'), crs=2835)
# Create boundary region
penn <- st_geometry(penn.sf)

# Calculate (Rook contiguity-based) neighbors
contiguity.nb <- poly2nb(penn.sf, queen = FALSE) # Queen contiguity

# Compute a spatial weights matrix (row standardized neighbor list)
penn.rsw <- nb2listw(contiguity.nb, style = 'W')

```

## A brief summary of the attribute
```{r summary, echo=FALSE}

#Brief summary of the smoking attribute
summary(penn.sf$smoking)

```
###Plots of the distribution of a) the attribute and b) the lagged attribute
```{r, echo=FALSE}

#histogram of the distribution of smoking 
Figure1<-penn.sf %>% 
  ggplot(aes(x = smoking)) +
  geom_histogram(bins= 10, fill = "orange", color = "black") +
  labs(
    title = "Figure1:County-specific smoking rates in Pennsylvania",
    x = "smoking rates",
    y = "Frequency"
  ) +
  theme_minimal()

#histrogram of the lagged smoking 
penn.sf <- penn.sf %>%
  mutate(
    nb = st_contiguity(geometry, queen = FALSE),
    wt = st_weights(nb, style = "W"),
    smoking_lag = st_lag(smoking, nb, wt)
  )


Figure2<- penn.sf %>% 
  ggplot(aes(x = smoking_lag)) + 
  geom_histogram(bins = 10, color = "black", fill = "orange") + 
  theme_bw() + 
  labs(
    x = "smoking rates lagged", 
    y = "Frequency", 
    title = "Figure2:Distribution of lagged county-specific smoking rates in Pennsylvania"
  )
grid.arrange(Figure1, Figure2)
```

### Maps of a) the attribute and b) the lagged attribute
```{r Maps, echo=FALSE}

# Map of the observed smoking rates 
Figure3 <- penn.sf %>% 
  ggplot(aes(fill = smoking)) +  
  geom_sf(color = 'black', lwd = 0.15) +
  scale_fill_gradient(low = "blue", high = "red", limits = range(penn.sf$smoking)) +
  theme_minimal() +
  labs(
    title = 'Figure3:County-Specific Smoking Rates',  
    x = "Longitude", 
    y = "Latitude"
  )

# Map of the lagged smoking rates with a custom gradient
Figure4 <- penn.sf %>% 
  ggplot(aes(fill = smoking_lag)) +  
  geom_sf(color = 'black', lwd = 0.15) +
  scale_fill_gradient(low = "blue", high = "red", limits = range(penn.sf$smoking_lag)) +
  theme_minimal() +
  labs(
    title = 'Figure4:County-Specific Smoking Rates Lagged',  
    x = "Longitude", 
    y = "Latitude"
  )

grid.arrange(Figure3, Figure4)


```
### Global Moran's I test: Annalytical approach: p-value 
```{r, echo=FALSE}
# global Moran's 
gmoran <- moran.test(penn.sf$smoking, penn.rsw, 
                     alternative = 'greater') #  Test for clustering


# Plot of the G Moran's
penn.sf <- penn.sf %>%
  mutate(sSmoke = scale(smoking),
         sLag = scale(smoking_lag),
         quadrant =
           case_when(sSmoke >= 0 & sLag >= 0 ~ "High-High", # top right quadrant
                     sSmoke> 0 & sLag <= 0 ~ "High-Low", # bottom right quadrant
                     sSmoke <= 0 & sLag >= 0 ~ "Low-High",
                     sSmoke <= 0 & sLag <= 0 ~ "Low-Low"))

pal <- c('red', 'lightpink', 'lightblue','blue','lightgrey')

Figure5 <- penn.sf %>% ggplot(aes(y = sLag, x = sSmoke)) + # we scaled the values so the mean is 0 (changed into z-score)
  geom_point(size = 0.75, aes(color = quadrant)) +
  scale_color_manual(values = pal) +
  geom_smooth(method = "lm", se = FALSE, lwd = 0.25, col = "orchid") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", linewidth = 0.25) +
  theme_classic() +
  labs(x = "Observed Smoking rate",
       y = "Spatially Lagged Smoking rate",
       title = "Figure5:Spatially Lagged Crime vs Crime (Scaled)")

 
# 
#Map of the quadrants based on the lagged and smoke 
pal <- c('red', 'lightpink', 'lightblue', 'blue')
Figure6 <- penn.sf %>% ggplot(aes(fill = quadrant)) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_manual(values = pal) +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       title = " Figure6:Map of the quadrants") 


grid.arrange(Figure5, Figure6)

```



### Local Moran's I test: simulation-based approach
```{r Local Moran }
# Local Moran's  simulation-based approach
set.seed(2024)
lmoranMC <-  localmoran_perm(penn.sf$smoking, penn.rsw, nsim = 999, alternative = "greater")
 
# Define hotspots in penn.sf object
penn.sf <- penn.sf %>%
  mutate(Ii = hotspot(lmoranMC, Prname = "Pr(folded) Sim", cutoff = 0.05, p.adjust = "none")) #correcting for multiple comparisons

# Replace NA with 'Non-significant'.
penn.sf$Ii<-factor(penn.sf$Ii, 
                       exclude=NULL,
                       levels=c(levels(penn.sf$Ii), NA),
                       labels=c(levels(penn.sf$Ii), 'Non-significant'))

# Map significant clusters based 

pal2<-c('blue', 'lightpink', 'lightblue','red', 'lightgray')

Figure7 <-penn.sf %>% ggplot(aes(fill=Ii))+
  geom_sf(color='black', lwd=0.15)+
  scale_fill_manual(values=pal2)+
  theme_bw()+
  labs(x='Longitude',
       y='Latitude',
       title='Figure7:Simulated P-value(alpha=0.05)')

Figure7

```

### Philadelphia and Cameron counties and their neighbors
```{r Philadelphia and Cameron, echo=FALSE }

# get the rows for Philadelphia and Cameron counties
philadelphia <- penn.sf %>% filter(county == "philadelphia")
cameron <- penn.sf %>% filter(county == "cameron")
# Identify neighbors based on spatial contiguity
# Combine Philadelphia and Cameron's neighbors using the contiguity.nb list
philadelphia_neighbors <- contiguity.nb[[which(penn.sf$county == 'philadelphia')]]
cameron_neighbors <- contiguity.nb[[which(penn.sf$county == "cameron")]]

# Assign labels to Philadelphia and Cameron counties and their neighbors
penn.sf$highlighted <- "Other"
penn.sf$highlighted[which(penn.sf$county == "philadelphia")] <- "philadelphia"
penn.sf$highlighted[which(penn.sf$county == "cameron")] <- "cameron"
penn.sf$highlighted[philadelphia_neighbors] <- "Neighbor"
penn.sf$highlighted[cameron_neighbors] <- "Neighbor"

# Map Philadelphia, Cameron counties and their neighbors
Figure8 <- penn.sf %>%
  ggplot(aes(fill = highlighted)) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_manual(values = c("philadelphia" = "red", 
                               "cameron" = "blue", 
                               "Neighbor" = "lightblue", 
                               "Other" = "gray")) +
  theme_minimal() +
  labs(title = "Figure8:Philadelphia and Cameron Counties and Neighbors",
       x = "Longitude", 
       y = "Latitude")


Figure8

```
### sensitivity analysis(0.1, 0.01)
```{r sensitivity, echo=FALSE }
#A sensitivity analysis where the significance level is changed to a) α = 0.1 then b) α = 0.01.

# sig level 0.1
# global Moran's 
gmoran <- moran.test(penn.sf$smoking, penn.rsw, 
                     alternative = 'greater') 
 
# Local Moran's  simulation-based approach
set.seed(2024)
lmoranMC_0.1 <-  localmoran_perm(penn.sf$smoking, penn.rsw, nsim = 999, alternative = "greater")
 
# Define hotspots in penn.sf object
penn.sf <- penn.sf %>%
  mutate(Ii_0.1 = hotspot(lmoranMC_0.1, Prname = "Pr(folded) Sim", cutoff = 0.1, p.adjust = "none")) #correcting for multiple comparisons

# Replace NA with 'Non-significant'.
penn.sf$Ii_0.1<-factor(penn.sf$Ii_0.1, 
                       exclude=NULL,
                       levels=c(levels(penn.sf$Ii_0.1), NA),
                       labels=c(levels(penn.sf$Ii_0.1), 'Non-significant'))

# Map significant clusters based 

pal3<-c('blue', 'lightpink', 'lightblue','red', 'lightgray')

Figure9 <-penn.sf %>% ggplot(aes(fill=Ii_0.1))+
  geom_sf(color='black', lwd=0.15)+
  scale_fill_manual(values=pal3)+
  theme_bw()+
  labs(x='Longitude',
       y='Latitude',
       title='Figure9:Simulated P-value(alpha=0.1)')

# Sig level 0.01

set.seed(2024)
lmoranMC_0.01 <-  localmoran_perm(penn.sf$smoking, penn.rsw, nsim = 999, alternative = "greater")
 
# Define hotspots in penn.sf object
penn.sf <- penn.sf %>%
  mutate(Ii_0.01 = hotspot(lmoranMC_0.01, Prname = "Pr(folded) Sim", cutoff = 0.01, p.adjust = "none")) #correcting for multiple comparisons

# Replace NA with 'Non-significant'.
penn.sf$Ii_0.01<-factor(penn.sf$Ii_0.01, 
                       exclude=NULL,
                       levels=c(levels(penn.sf$Ii_0.01), NA),
                       labels=c(levels(penn.sf$Ii_0.01), 'Non-significant'))

# Map significant clusters based 

pal4<-c('blue', 'lightpink', 'lightblue','red', 'lightgray')

Figure10 <-penn.sf %>% ggplot(aes(fill=Ii_0.01))+
  geom_sf(color='black', lwd=0.15)+
  scale_fill_manual(values=pal4)+
  theme_bw()+
  labs(x='Longitude',
       y='Latitude',
       title='Figure10:Simulated P-value(alpha=0.01)')

grid.arrange(Figure9,Figure10)
```
Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

### Corecting for multiple comparinons
```{r , echo=FALSE }

# Local Moran's  simulation-based approach
set.seed(2024)
lmoranMC_0.1 <-  localmoran_perm(penn.sf$smoking, penn.rsw, nsim = 999, alternative = "greater")
 
# Define hotspots in penn.sf object
penn.sf <- penn.sf %>%
  mutate(Ii_0.1FDR = hotspot(lmoranMC_0.1, Prname = "Pr(folded) Sim", cutoff = 0.1, p.adjust = "fdr")) #correcting for multiple comparisons

# Replace NA with 'Non-significant'.
penn.sf$Ii_0.1FDR<-factor(penn.sf$Ii_0.1FDR, 
                       exclude=NULL,
                       levels=c(levels(penn.sf$Ii_0.1FDR), NA),
                       labels=c(levels(penn.sf$Ii_0.1FDR), 'Non-significant'))

# Map significant clusters based 

pal3<-c('blue', 'lightpink', 'lightblue','red', 'lightgray')

Figure11 <-penn.sf %>% ggplot(aes(fill=Ii_0.1FDR))+
  geom_sf(color='black', lwd=0.15)+
  scale_fill_manual(values=pal3)+
  theme_bw()+
  labs(x='Longitude',
       y='Latitude',
       title='Figure11:Simulated P-value(0.1), fdr adjusted')

# Sig level 0.01

set.seed(2024)
lmoranMC_0.01 <-  localmoran_perm(penn.sf$smoking, penn.rsw, nsim = 999, alternative = "greater")
 
# Define hotspots in penn.sf object
penn.sf <- penn.sf %>%
  mutate(Ii_0.01FDR = hotspot(lmoranMC_0.01, Prname = "Pr(folded) Sim", cutoff = 0.01, p.adjust = "fdr")) #correcting for multiple comparisons

# Replace NA with 'Non-significant'.
penn.sf$Ii_0.01FDR<-factor(penn.sf$Ii_0.01FDR, 
                       exclude=NULL,
                       levels=c(levels(penn.sf$Ii_0.01FDR), NA),
                       labels=c(levels(penn.sf$Ii_0.01FDR), 'Non-significant'))

# Map significant clusters based 

pal4<-c('blue', 'lightpink', 'lightblue','red', 'lightgray')

Figure12 <-penn.sf %>% ggplot(aes(fill=Ii_0.01FDR))+
  geom_sf(color='black', lwd=0.15)+
  scale_fill_manual(values=pal4)+
  theme_bw()+
  labs(x='Longitude',
       y='Latitude',
       title='Figure12:Simulated P-value(alpha=0.01), fdr Adjusted')

#WITH 0.05 SIG LEVEL
set.seed(2024)
lmoranMC_0.05 <-  localmoran_perm(penn.sf$smoking, penn.rsw, nsim = 999, alternative = "greater")
 
# Define hotspots in penn.sf object
penn.sf <- penn.sf %>%
  mutate(Ii_FDR = hotspot(lmoranMC_0.05, Prname = "Pr(folded) Sim", cutoff = 0.05, p.adjust = "fdr")) #correcting for multiple comparisons

# Replace NA with 'Non-significant'.
penn.sf$Ii_FDR<-factor(penn.sf$Ii_FDR, 
                       exclude=NULL,
                       levels=c(levels(penn.sf$Ii_FDR), NA),
                       labels=c(levels(penn.sf$Ii_FDR), 'Non-significant'))

Figure13 <-penn.sf %>% ggplot(aes(fill=Ii_FDR))+
  geom_sf(color='black', lwd=0.15)+
  scale_fill_manual(values=pal2)+
  theme_bw()+
  labs(x='Longitude',
       y='Latitude',
       title='Figure13:Simulated P-value(alpha=0.05), fdr Adjusted')

grid.arrange(Figure11, Figure12, Figure13)

```
Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.











