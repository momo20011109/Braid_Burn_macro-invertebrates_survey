install.packages("vegan")
install.packages("adespatial")
library("vegan")
library('ggplot2')
invert<-read.csv("data/braidburn_biota.csv")
#use the diversity function to calculate shannon diversity index of the invertebrates data
invert.shan<-diversity(invert[,-1],index="shannon")
invert.shan
mean(invert.shan)
sd(invert.shan)
#plot the average diversity
invert.sites <- invert[, 1]
species <-invert[-1]
species.name<-colnames(invert[-1])
species.sum<-colSums(species)
species.ave<-species.sum/7
new.invert<- data.frame(Taxa = species.name, count = species.ave)
ggplot(new.invert, aes(x = "", y = count, fill = Taxa)) +
  geom_col(width = 1, color = "black") +
  coord_polar("y", start = 0) + 
  labs(title = "") + 
  scale_fill_manual(values = rainbow(length(unique(species.name)))) + 
  theme_void() + 
  theme(legend.position = "bottom" ,
        plot.title = element_text(hjust = 0.5,size=28),
        legend.key.size = unit(1.5, "lines"),
        legend.text = element_text(size = 12),
        legend.margin = margin(t = 0.5, unit = "cm")
        )

#plot the shannon diversity index of 7 sites
barplot(invert.shan, 
        xlab="Site", ylab="Shannon's Diversity Index",
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
     ylab="Number of Taxon",
     xlab="Site")

#gain the stress output of NMDS
invert.nmds<-metaMDS(invert,
                     k=2,     
                     distance="bray",
                     maxit=999,
                     trymax=500,
                     wascores=TRUE,
)

invert.nmds

#run a stress plot to evaluate the model
stressplot(invert.nmds)
autoplot(invert.nmds, geom="text")+theme_bw()

#beta diversity
library(adespatial)
## Jaccard differences
invert.div.jac <- beta.div.comp(invert[-1], coef = "J", quant = T)
invert.div.jac$part


#Site Contribution to beta-diversity
local.invert.rich <- LCBD.comp(invert.div.jac$rich, 
                             sqrt.D = T)
local.invert.rich
par(mar=c(10, 4, 4,2) + 0.1)
plot(local.invert.rich$LCBD, 
     type = "p", 
     ylab = "Site Contribution to beta-diversity",
     xlab = "Site Number",
     col = "orange",
     pch = 20,
     cex = 1.5)
# Species contribution to Beta-diversity
invert.scBD <- beta.div(invert[-1], method = "hellinger")
invert.scBD

par(mar=c(10, 4, 4,2) + 0.1)


barplot(invert.scBD$SCBD, 
        ylab="Taxa Contribution to beta-diversity",
        names.arg = colnames(invert.scBD$SCBD), 
        las = 2,
        col="orange",
        border="orange",
        cex.names = 1.4)

