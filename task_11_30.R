library(languageR)
data(ratings)
ratings

complex_freq = subset(ratings$Frequency, ratings$Complex=='complex')
simplex_freq = subset(ratings$Frequency, ratings$Complex=='simplex')

#print out descriptive statistics
summary(complex_freq)
summary(simplex_freq)
par(mfrow=c(1,2))
library(lattice)
bwplot(Frequency~Complex, data=ratings)
mean(complex_freq)
mean(simplex_freq)

#perform an appropriate t-test
# complex word와 simplex word는 서로 독립적인 관계에 있다. 따라서 independent t-test를 수행한다.

#가설 설정
#H0: complex word와 simplex word 사이의 frequency 차이가 없다.
#H1: complex word와 simplex word 사이의 frequency 차이가 있다.

#independent two sample t-test
t.test(complex_freq, simplex_freq)
#t(12.62)=-3.1652, p=0.007698
#유의수준 0.05에서 p-value가 0.05보다 작으므로 H0가 기각된다
#즉, complex word와 simplex word 사이의 frequency는 차이가 있다고 볼 수 있다.
#따라서 complex word의 평균 frequency가 simplex word의 평균 frequency보다 작으므로
#complex word가 simplex word보다 덜 빈번하게 나타난다고 볼 수 있다.