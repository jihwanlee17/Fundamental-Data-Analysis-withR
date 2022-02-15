#p. 17
score = c(13.1, 15, 14, 14.4, 14, 11.6, 16.3, 15.7, 17.2, 14.9, 14.4, 17.2, 13.7, 13.9, 12.4, 13.8, 14.9, 13.3, 15.7, 13.7, 14.4, 16, 13.9, 14.7, 13.5, 13.4, 13.2, 12.7, 13.4, 12.3) 
group = c(rep("ke", 6), rep("kn", 6), rep("en", 6), rep("ee", 6), rep("ks", 6))
md = data.frame(score, group) 
md

## descriptive statistics
library(moments)

#ke
ke=subset(md$score, md$group=='ke')
summary(ke)
skewness(ke)
kurtosis(ke)

#kn
kn = subset(md$score, md$group=='kn')
summary(kn)
skewness(kn)
kurtosis(kn)

#en
en = subset(md$score, md$group=='en')
summary(en)
skewness(en)
kurtosis(en)

#ee
ee = subset(md$score, md$group == 'ee')
summary(ee)
skewness(ee)
kurtosis(ee)

#ks
ks = subset(md$score, md$group == 'ks')
summary(ks)
skewness(ks)
kurtosis(ks)

## boxplot of score of each group
boxplot(subset(md$score, md$group=='ke'),subset(md$score, md$group=='kn'),subset(md$score, md$group=='en'),subset(md$score, md$group=='ee'),subset(md$score, md$group=='ks'), names=c('ke','kn','en','ee','ks'))

## ANOVA Test of five groups
aov.group <- aov(score~group, data=md)
summary(aov.group)
#p = 0.000187이므로, 유의수준 0.05보다 작으므로 집단 간에는 평가하는 점수가 서로 다르다.

## Post Hoc test - bonferroni
pairwise.t.test(score, group, p.adjust = 'bonferroni')

# p.18
### ke, kn의 비교

##try t-test
ttest=t.test(ke, kn, data=md)
ttest
# p= 0.00769

##try ANOVA
d2=subset(md, md$group == 'ke' | md$group == 'kn')
aov.kekn = aov(score~group, data=d2)
summary(aov.kekn)
# p= 0.00769

##compare the p-values
# 두 집단 ke, kn의 score에 대해 t-test와 anova를 수행해서 나온 각각의 p-value값은 0.00769로 서로 동일하다.
