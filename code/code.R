install.packages("vegan")
library("vegan")
library('ggplot2')
invert<-read.csv("braidburn_biota.csv")
#use the diversity function to calculate shannon diversity index of the invertebrates data
invert.shan<-diversity(invert[,-1],index="shannon")
invert.shan

#plot the average diversity
invert.sites <- invert[, 1]
species <-invert[-1]
species.name<-colnames(invert[-1])
species.sum<-colSums(species)
species.ave<-species.sum/7
new.invert<- data.frame(species = species.name, count = species.ave)
ggplot(new.invert, aes(x = "", y = count, fill = species)) +
  geom_col(width = 1, color = "black") +
  coord_polar("y", start = 0) + 
  labs(title = "Mean composition of macro-invertebrates of the Braid Burn") + 
  scale_fill_manual(values = rainbow(length(unique(species.name)))) + 
  theme_void() + 
  theme(legend.position = "bottom" ,plot.title = element_text(hjust = 0.5))

#plot the shannon diversity index of 7 sites
barplot(invert.shan, 
        xlab="Site", ylab="Shannon Diversity Index",
        main="Shannon Diversity Index across Sites",
        names.arg=1:7,
        col="orange",
        border="orange")

#plot the species accumulation curve
invert.accum<-specaccum(invert, method="random")
plot(invert.accum, ci.type="polygon",
     col="red",
     lwd=2,
     ci.lty=0,
     ci.col="pink",
     ylab="Number of Species")

#gain the stress output of NMDS
invert.nmds<-metaMDS(invert,
                     k=2,     
                     distance="bray",
                     maxit=999,
                     trymax=500,
                     wascores=RUE,
)

invert.nmds

#run a stress plot to evaluate the model
stressplot(invert.nmds)
autoplot(invert.nmds, geom="text")+theme_bw()

