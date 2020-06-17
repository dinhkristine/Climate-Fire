#### Global Moran's test ----

#1: perfect positive spatial autocorrelation(clustered data)
#0: data is randomly distributed
#-1:negative spatial autocorrelation (dissimilar values next to each other)

####  weighted Moran's/Global Moran test ---- 

CA_tracts <- tracts(state = "CA")

neighbors <- poly2nb(CA_tracts)

neighbors_weighted <- nb2listw(neighbors,zero.policy=T)

moran.test(CA_tracts$fire_freq,neighbors_weighted,zero.policy=T)

globalMoran[["estimate"]][["Moran I statistic"]]

#the Moran I statistic is 0.059 mildly positively autocorrelated/ randomly distributed
globalMoran[["p.value"]] # p-value: model not statistically significant


#### Moran ScatterPlot ---- 

moran <- moran.plot(CA_tracts$fire_freq, listw = nb2listw(neighbors, style = "W"))

#local moran
local <- localmoran(x = CA_tracts$fire_freq, listw = nb2listw(neighbors, style = "W"))
#local is matrix 58 x 5 
head(local)

#### Map of the local moran statistic (Ii) ----

# A positive value for Ii indicates that the unit is surrounded by units w/similar values

moran.map <- cbind(CA, local)
tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "local moran statistic")+
  tm_layout(scale = .5,
            legend.position = c(.6,.6), 
            legend.outside.size = 0.1,
            legend.title.size = 6,
            legend.height = 0.9,
            legend.text.size = 5, 
            legend.hist.size = 0.6)+tm_compass()+tm_scale_bar()



#### Plot LISA Clusters ---- 

quadrant <- vector(mode="numeric",length=nrow(local))

# centers the variable of interest around its mean
m.qualification <- CA_tracts$fire_freq - mean(CA_tracts$fire_freq)     

# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])    

# significance threshold
signif <- 0.1 

#### builds a data quadrant

quadrant[m.qualification >0 & m.local>0] <- 4  
quadrant[m.qualification <0 & m.local<0] <- 1      
quadrant[m.qualification <0 & m.local>0] <- 2
quadrant[m.qualification >0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0   


## plot in r

brks <- c(0,1,2,3,4)

colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")

plot(CA_tracts,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])

box()

legend("topright", legend = c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n",cex=4)


#### Getis_ord Approach ----
#looks at neighbours within a defined proximity to identify where either high or low values cluster spatially

nb<-dnearneigh(coordinates(CA_tracts),0,1000)
nb_lw <- nb2listw(nb, style = 'B')

# Plot Data and Neighbours

plot(CA_tracts, border = 'lightgrey')

plot(nb, coordinates(CA_tracts), add=TRUE, col = 'red')

#### Getis-Ord Gi Statistic ---- 
## local_g is spatial polygons data frame

wn <- poly2nb(CA_tracts,row.names=CA_tracts$fire_freq,queen=FALSE)

lstw <- nb2listw(wn,style='B')

Gi <- localG(CA_tracts$fire_freq,lstw)

local_g <- cbind(CA_tracts,as.matrix(Gi))

names(local_g)[20]<-"gstat"


#### Clustermap ---- 

tm_shape(local_g) + 
  tm_fill("gstat", 
          palette = "RdBu",
          style = "pretty") +
  tm_borders(alpha=.4) +
  tm_layout(scale = .5,
            legend.position = c(.6,.6), 
            legend.outside.size = 0.1,
            legend.title.size = 6,
            legend.height = 0.9,
            legend.text.size = 5, 
            legend.hist.size = 0.6)+tm_compass()+tm_scale_bar()

## or

ws <- include.self(wn)

lstws <- nb2listw(wn, style='B')

Gis <- localG(CA_tracts$fire_freq, lstws)

Gscuts <- cut(Gis, 5)

Gscutsi <- as.integer(Gscuts)

cols <- rev(gray(seq(0,1,.2)))

plot(CA_tracts, col=cols[Gscutsi])

legend('bottomleft', levels(Gscuts), fill=cols)

#Gi Statistic is represented as a Z-score
#Greater values represent a greater intensity of clustering 
#the direction (positive or negative) indicates high or low clusters


#### Data Exploratory Quantile ---- 

tm_shape(CA_tracts) + 
  tm_fill("fire_freq",
          palette = "Blues", 
          style="quantile",
          title = "% with a Qualification") +
  tm_borders(alpha=.4)  


#LISA significance map
#locations with significant local statistics
#multiple comparison problem
#sensitivy analysis to p-value

#shading by significance
#non-significant locations not highlighted


#### Chloropleth Map ----

spplot(CA_tracts, zcol='fire_freq', col="transparent")

# zero.policy=T : locations with no neighbors (e.g. islands) will be skipped rather than generating an error
# nb2listw() takes the nb list and creates a weighted list. 




