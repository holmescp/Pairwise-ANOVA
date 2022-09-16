# Adapted from: https://rcompanion.org/rcompanion/d_08.html
# Citation: Mangiafico, S.S. 2015. An R companion for the handbook of biological statistics. New Brunswick, NJ: Rutgers Cooperative Extension, 161-167.
# Change file name, mesurevar, and groupvars
Data = factor(mothurANOVA)
as.factor(Group)
as.factor(Data$Taxon)
is.factor(Data$Group)
Data
sum = summarySE(Data,
                measurevar="Relabund",
                groupvars=c("Taxon","Group"))
sum

# If you'd like the ggplot ordered by decreasing y-values
data2=data[order(data$Relabund),]
data2$Group <- factor(data2$Group, levels=c("Female","Male","Tritonymph")) # MATCH WITH THEMES BELOW!!!
data2$Taxon <- factor(data2$Taxon, levels=c("Actinobacteria","Flavobacteriia","Sphingobacteriia","Gammaproteobacteria","Betaproteobacteria","Cytophagia","Deinococci","Bacteroidetes_unclassified","Alphaproteobacteria","Deltaproteobacteria","Other"))
data2

# Change x, y, fill, labs, and save file
Plot1<-ggplot(data2,aes(x=Taxon,y=Relabund,fill=Group))+
  geom_boxplot() +
  xlab("\nTaxon") +
  ylab("Relative Abundance\n") +
  scale_y_continuous(expand=c(0,0),breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6)) +
  scale_x_discrete(expand=c(0,0)) +
  theme_classic() +
  theme(axis.text.y=element_text(size=14,color="black"), axis.title=element_text(size=18,color="black")) +
  theme(axis.text.x=element_text(size=14,color="black",angle=45,hjust=1), axis.title=element_text(size=15,color="black")) +
  #If you don't need to angle the x-axis, use the following line instead
  #theme(axis.text=element_text(size=12,color="black"), axis.title=element_text(size=18,color="black")) +
  guides(colour = guide_legend(title=NULL, override.aes = list(size=.2)), fill=guide_legend(title=NULL)) +
  theme(panel.background=element_rect(fill="white",color="white"))+
  theme(legend.text=(element_text(size=10)))+
  #guides(fill=FALSE) + #this line removes the legend completely
  theme(axis.text=(element_text(size=12,color="black")),axis.title=(element_text(size=14,color="black")))+
  theme(axis.ticks=(element_line(color="black")),axis.line=(element_line(color="black"))) +
  scale_fill_manual(values=c(palettegrey),name="Groups",labels=c("Male", "Female","Tritonymph")) + 
  scale_color_manual(values=c(palettegrey),name="Groups",labels=c("Male","Female","Tritonymph"))
Plot1

# linear modeling and ANOVA; change all components "lm(y~x+fill+x:fill,...)"
model=lm(Relabund~Taxon+Group+Taxon:Group,data=Data)
hist(residuals(model))               # Check for normality
plot(fitted(model),residuals(model)) # For Homoscedasticity: condor.depaul.edu/sjost/it223/documents/resid-plots.gif
anova(model)                         # Type I
Anova(model,type="II")               # Type II
summary(model)                       # Did not look like it provided what was desired
lsmeans=lsmeans::lsmeans             # Uses lsmeans package, not lmerTest package
leastsquare=lsmeans(model,pairwise~Group:Taxon,adjust="tukey")
final=cld(leastsquare,alpha=0.05,Letters=letters)

# For extracting the table of comparisons with significance letters to tab delimited text, and excel files
write.table(final,"TaxonComparisons.txt",sep="\t")
write.xlsx(final,"TaxonComparisons.xlsx")