setwd('./') #문제에 필요한 모든 파일이 현재 디렉토리에 있다고 가정한다.

#1번
#editorial은 사설 텍스트이고 fairytaile은 동화 텍스트이다.
editorial <- 'California is approaching an energy crossroads In three years its last nuclear plant will begin to power down and the state will lose its largest single source of emissions-free electricity A 2018 law requires state regulators to avoid any increase in greenhouse gases as a result of closing the Diablo Canyon nuclear power plant on the Central Coast But if they don’t move more quickly to replace its electricity with renewable energy from wind solar and geothermal the void will almost certainly be filled by burning more natural gas which increased last year to account for nearly half of California’s in-state electricity generation California can’t allow the retirement of Diablo Canyon’s nuclear reactors to prolong its reliance on gas plants or increase planet-warming and health-damaging emissions But the state’s preparations for shutdown of an around-the-clock power source that supplies more than 8% of California’s in-state electricity generation have not inspired confidence there have been no assurances that an uptick in carbon emissions will be avoided'
fairytale <- "A Fox one day spied a beautiful bunch of ripe grapes hanging from a vine trained along the branches of a tree The grapes seemed ready to burst with juice and the Fox's mouth watered as he gazed longingly at them The bunch hung from a high branch and the Fox had to jump for it The first time he jumped he missed it by a long way So he walked off a short distance and took a running leap at it only to fall short once more Again and again he tried but in vain Now he sat down and looked at the grapes in disgust What a fool I am he said Here I am wearing myself out to get a bunch of sour grapes that are not worth gaping for And off he walked very very scornfully"

#1-(1)
editorial <- tolower(editorial)
fairytale <- tolower(fairytale)

#1-(2)
# 각 단어목록은 unique한 단어(중복을 제거한 단어)를 사용한다.
# 왜냐하면 특정 단어의 길이를 구하는 것이 목적이므로
# 중복된 단어는 서로 같은 단어이므로 각각의 단어 길이를 따로 구분해줄 필요가 없기 때문이다.
# 예를 들어 'of'의 길이는 2인데 'of'가 문장에서 여러번 나왔어도 분석하고자 하는 것은 그 단어의 길이일 뿐이다.
# 그러므로 'of' 자체의 길이를 알기한 하면 되므로 'of'의 중복을 제거한다.
# 이러한 이유로 각 단어목록에서 중복을 제외한 단어를 사용한다.
word_edit <- strsplit(editorial, split=" ")
word_edit <- unlist(word_edit)
word_edit <- unique(word_edit) #editorial 단어 목록
len_word_edit <- nchar(word_edit) #editorial 단어의 길이


word_fairy <- strsplit(fairytale, split=" ")
word_fairy <- unlist(word_fairy)
word_fairy <- unique(word_fairy) #fairytale 단어 목록
len_word_fairy <- nchar(word_fairy) #fairytale 단어의 길이

#1-(3)
#editorial의 단어 및 단어 길이로 구성된 데이터프레임 만들기
edit_frame <- data.frame(word_edit, len_word_edit)
#fairytale의 단어 및 단어 길이로 구성된 데이터프레임 만들기
fairy_frame <- data.frame(word_fairy, len_word_fairy)

#1-(4)
#editorial의 단어 및 단어길이로 된 데이터프레임을 현재 경로에 csv로 저장하기
write.csv(edit_frame, file='./edit_frame.csv', row.names=FALSE) #row.names=FALSE는 인덱스 빼고 저장하도록 하는 문구.
#fairytale의 단어 및 단어길이로 된 데이터프레임을 현재 경로에 csv로 저장하기
write.csv(fairy_frame, file='./fairy_frame.csv', row.names=FALSE)


#2번
#2-(1)

#editorial의 descriptive statistics
attach(edit_frame)
#평균
mean(len_word_edit)
#중앙값
median(len_word_edit)
#최빈값
frq=table(len_word_edit)
names(frq)[frq == max(frq)]
#범위(최솟값, 최댓값)
min(len_word_edit)
max(len_word_edit)
range(len_word_edit)
#분산
var(len_word_edit)
#skewness
library(moments)
skewness(len_word_edit)
#kurtosis
kurtosis(len_word_edit)
#histogram
par(mfrow=c(1,2))
hist(len_word_edit, main='Histogram of word length frequency', xlab='word length', ylab= 'frequency')
#box plot
boxplot(len_word_edit, main='Boxplot of word length frequency', xlab='word length', ylab= 'frequency')
par(mfrow=c(1,1))
detach(edit_frame)

#fairytale의 descriptive statistics
attach(fairy_frame)
#평균
mean(len_word_fairy)
#중앙값
median(len_word_fairy)
#최빈값
frq=table(len_word_fairy)
names(frq)[frq == max(frq)]
#범위(최솟값, 최댓값)
min(len_word_fairy)
max(len_word_fairy)
range(len_word_fairy)
#분산
var(len_word_fairy)
#skewness
library(moments)
skewness(len_word_fairy)
#kurtosis
kurtosis(len_word_fairy)
#histogram
par(mfrow=c(1,2))
hist(len_word_fairy, main='Histogram of word length frequency', xlab='word length', ylab= 'frequency')
#box plot
boxplot(len_word_fairy, main='Boxplot of word length frequency', xlab='word length', ylab= 'frequency')
par(mfrow=c(1,1))
detach(fairy_frame)

#2-(2)
#editorial에서 가장 긴 단어의 길이와 해당하는 단어 구하기
max(edit_frame$len_word_edit) #가장 긴 단어의 길이는 16이다.
edit_frame[edit_frame$len_word_edit==16,] #가장 긴 단어는 around-the-clock 이다.

#fairytale에서 가장 긴 단어의 길이와 해당하는 단어 구하기
max(fairy_frame$len_word_fairy) #가장 긴 단어의 길이는 10이다.
fairy_frame[fairy_frame$len_word_fairy==10,] #가장 긴 단어는 scornfully 이다.

#2-(3)
#H0: editorial과 fairytale의 단어 길이는 서로 차이가 없다.
#H1: editorial과 fairytale의 단어 길이는 서로 유의미한 차이가 있다.
t.test(edit_frame$len_word_edit, fairy_frame$len_word_fairy)
#p-value가 0.0003875로, 유의수준 0.05보다 작으므로 H0는 기각된다.
#따라서 editorial과 fairytale의 단어 길이는 서로 유의미한 차이가 있다고 분석할 수 있다(t(249.25)=3.5974, p=0.0003875).

#3번
formal = c(4.51, 3.61, 4.11, 4.18, 3.80, 4.04, 4.69, 3.71, 2.73, 3.90, 3.79, 2.77, 3.57, 4.36, 4.26, 4.12, 4.33, 3.95, 4.01, 4.18)
informal = c(4.78, 5.36, 5.51, 4.45, 5.43, 4.51, 5.32, 5.59, 5.06, 5.82, 5.99, 4.73, 5.45, 5.32, 4.99, 5.20, 5.32, 3.88, 5.18, 4.33)
#3-(1)
#formal 상황의 데이터의 기술통계정보
mean(formal)
#중앙값
median(formal)
#최빈값
frq=table(formal)
names(frq)[frq == max(frq)]
#범위(최솟값, 최댓값)
min(formal)
max(formal)
range(formal)
#분산
var(formal)
#skewness
library(moments)
skewness(formal)
#kurtosis
kurtosis(formal)
#histogram
par(mfrow=c(1,2))
hist(formal, main='Histogram of speech speed in formal situation', xlab='speech speed', ylab= 'frequency', col='yellow')
#box plot
boxplot(formal, main='Boxplot of speech speed in formal situation', xlab='speech speed', ylab= 'frequency', col='green')
par(mfrow=c(1,1))

#informal 상황의 데이터의 기술통계
mean(informal)
#중앙값
median(informal)
#최빈값
frq=table(informal)
names(frq)[frq == max(frq)]
#범위(최솟값, 최댓값)
min(informal)
max(informal)
range(informal)
#분산
var(informal)
#skewness
library(moments)
skewness(informal)
#kurtosis
kurtosis(informal)
#histogram
par(mfrow=c(1,2))
hist(informal, main='Histogram of speech speed in informal situation', xlab='speech speed', ylab= 'frequency', col='yellow')
#box plot
boxplot(informal, main='Boxplot of speech speed in informal situation', xlab='speech speed', ylab= 'frequency', col='green')
par(mfrow=c(1,1))

#3-(2)
#normality test
shapiro.test(formal-informal) #p=0.3161이므로 유의수준 0.05보다 커서 귀무가설 '데이터 분포는 정규적이다'를 채택한다.
#parametric paired t-test
t.test(formal, informal, paired=TRUE) #p=9.169e-07이므로 유이수준 0.05보다 작으므로 귀무가설이 기각된다. 즉, 두 상황에서의 발화속도는 유의미한 차이가 있다고 분석할 수 있다.

#4번
concat <-c(formal, informal) #데이터 모두 합치기
#4-(1)
shapiro.test(concat) #p=0.3112이므로, 유의수준 0.05보다 크므로 귀무가설이 채택된다. 따라서 데이터 분포는 정규분포로 분석된다.
#4-(2)
mean(concat) #평균: 4.521
sd(concat) #표준편차: 0.7826543
num=pnorm(5, mean=4.521, sd=0.7826543, lower.tail = FALSE)
cat('발화 속도가 5 이상인 사람은 상위',num*100,'% 입니다.')
#4-(3)
#하위 10%의 발화속도 구하기
qnorm(0.1, mean=4.521, sd=0.7826543) #하위 10%의 발화속도는 3.517988이다.
#하위 10%에 해당하는 사람은 몇 명인지 구하기
n = pnorm(3.517988, mean=4.521, sd=0.7826543) #0.09999996 (전체의 약 9.9%. 즉, 올림하면 전체의 약 10%에 해당하는 사람이 이 구간에 해당함.) 
n*40 #3.999999 즉, 약 4명이 이 구간에 해당한다.

#5번
rating_writing <- read.csv("./rating_writing.csv", header=TRUE)
attach(rating_writing)

#5-(1)
#grammar점수의 기술통계 정보 구하기
summary(grammar)
#최빈값
frq=table(grammar)
names(frq)[frq == max(frq)]
par(mfrow=c(1,2))
hist(grammar, main='boxplot of grammar score', xlab='grammar score', ylab='frequency', col='skyblue')
boxplot(grammar, main='histogram of grammar score', xlab='grammar score', ylab='frequency', col='pink')
#fluency점수의 기술통계 정보 구하기
summary(fluency)
frq=table(fluency)
names(frq)[frq == max(frq)]
hist(fluency, main='histogram of fluency score', xlab='fluency score', ylab='frequency', col='skyblue')
boxplot(fluency, main='boxplot of fluency score', xlab='fluency score', ylab='frequency', col='pink')
par(mfrow=c(1,1))

#5-(2)
#grammar 점수와 fluency 점수의 상관관계 구하기
cor.test(grammar, fluency)

#5-(3)
#상관관계 plot 그리기(linear regression modeling 이용)
plot(grammar, fluency)
lm.score <- lm(fluency~grammar)
abline(lm.score, col='red')
summary(lm.score)

#5-(4)
#grammar 점수가 3인 경우 fluency 점수 예측하기
new.grammar <- data.frame(grammar=3)
new.fluency <- predict(lm.score, new.grammar)
new.fluency #grammar 점수가 3일 때 fluency 점수는 2.362643점으로 예측된다.

detach(rating_writing)

#6번
#6-(1)
formality <- read.csv('./formality.csv', header=TRUE)
View(formality)
summary(formality)

formality$subject <- as.factor(formality$subject)
formality$gender <- as.factor(formality$gender)
formality$scenario <- as.factor(formality$scenario)
formality$attitude <- as.factor(formality$attitude)

# 피험자(speaker)마다 14번의 시행을 거쳐 목소리 높이를 측정했는데,
# 여러번 시행한 각각의 값에는 피험자 개인의 특성이 반영될 수 있다.
# 실험 당시 개인의 건강상태, 기분 등의 변화 또는 얼마나 빨리 실험에 익숙해져 실험 노하우를 터득하는지 등, 실험에 피험자 개인의 특성 또는 상황이 각각의 목소리 높이 측정값에 영향을 줄 수 있다.
# 또한 성별이 남성인 사람들 중에도 어떤 사람은 평소 목소리 높이가 평균적으로 높은데 어떤 사람은 목소리 높이가 평균적으로 낮을 수 있다.
# 이처럼, 각각의 목소리 높이에는 개인의 특성이 반영된다. 즉, 목소리 높이는 서로 독립적이지 않고 피험자에 의존한다.
# 따라서 피험자를 random effect로 설정해야 한다.

# 또한 발화상황(scenario)에 대해서도, 각 상황별 특성이 개별 목소리 높이에 반영될 수 있다.
# 예를들어 이야기하는 상황이 '출근시간에 늦어 누군가의 자동차를 탔을 때'라고 가정한다면,
# 택시에 타서 택시기사님에게 공손한 태도로 이야기하는 상황이거나, 직장 친구의 차에 친구와 같이 타면서 친구에게 비격식적 태도로 이야기하는 상황일 수 있다.
# 그런데 현재 자신의 상황이 급박하므로 공손한 태도나 비격식적 태도에 상관 없이 이러한 상황에서는 전반적으로 목소리 높이가 높아질 수 있다.
# 이러한 상황을 겪으면 대부분 비슷한 반응을 보일 것이다. 자신이 현재 급한 상황이라면 당황해서 목소리 높이가 일반적으로 높을 것이다.
# 특정 상황에 대한 개별 피험자의 목소리 높이가 서로 차이가 있을지라도 개별 상황의 특성이 반영되어 특정 상황에 대해서는 피험자별 목소리 높이가 비슷할 것이다.
# 즉, 피험자의 목소리 높이에는 상황별 특성이 반영될 수 있으므로 목소리 높이는 이야기하는 상황에 의존한다.
# 따라서 발화상황도 random effect로 설정해야 한다.

# 이제 피험자(subject)와 발화상황(scenario)를 random effect로 설정하고,
# 성별(gender)과 태도(attitude)를 fixed effect, 즉, 독립변수로 설정하고,
# 목소리 높이(pitch)를 종속변수로 설정하여 실험결과를 분석한다.

#개별 목소리 높이 데이터에 대한 최소, 최대, 평균, 중앙값
summary(formality$pitch)

#각각의 성별에 대한 평균 목소리 높이
ref2 <- aggregate(pitch~gender, data=formality, FUN='mean')
ref2

#각각의 태도에 대한 평균 목소리 높이
ref3 <- aggregate(pitch~attitude, data=formality, FUN='mean')
ref3

#성별과 태도를 동시에 고려했을 때의 평균 목소리 높이
ref4 <- aggregate(pitch~gender+attitude, data=formality, FUN='mean')
ref4

#boxplot 그리기
par(mfrow=c(1,3))
#성별에 따른 목소리 높이 차이를 비교하기 위해 boxplot을 그린 결과
boxplot(pitch~gender, data=formality, col='white')

#태도에 따른 목소리 높이 차이를 비교하기 위해 boxplot을 그린 결과
boxplot(pitch~attitude, data=formality, col='skyblue')

#성별과 태도를 함께 고려하여 각각의 경우에 대한 box plot을 그린 결과.
boxplot(pitch~gender+attitude, data=formality, col=c('green', 'yellow')) #여성은 녹색, 남성은 노란색으로 표시. #성별간 목소리 높이 차이가 제일 두드러지며, 남성, 여성 모두 

par(mfrow=c(1,1))

#성별과 태도의 상호작용에 따른 목소리 높이 변화 양상을 interaction plot으로 시각화한 결과.
interaction.plot(formality$gender, formality$attitude, formality$pitch) #각각의 발화 상황에서의 목소리 높이 차이가 여성과 남성에 따라 별로 큰 차이는 없어보인다.


#6-(2)
#가설1: 남성과 여성의 목소리 높이 차이는 유의미할 것이다. 즉, 남성보다 여성의 목소리 높이가 더 클 것이다.
#가설2: 공손한 태도와 비격식적 태도의 목소리 차이는 유의미할 것이다. 즉, 공손한 태도보다 비격식적 태도의 목소리 높이가 더 클 것이다.
#가설3: 남성과 여성 모두 공손한 태도보다 비격식적 태도에서 목소리 높이가 더 높을 것이다. 즉, 목소리 높이에 대한 성별과 태도의 상호작용은 유의하지 않다.

library(lme4) #linear mixed effect model
install.packages('lmerTest') #lmerTest 패키지가 이미 설치되어 있다면 굳이 설치하지 않아도 무방함.
library(lmerTest) #모델 분석 결과를 더 자세히 알려주는 패키지.
l0 = lmer(pitch~(1|subject)+(1|scenario),data=formality, REML=FALSE)
l1 = lmer(pitch~gender+ (1|subject)+(1|scenario), data=formality, REML=FALSE)
l2 = lmer(pitch~gender+attitude+(1|subject)+(1|scenario), data=formality, REML=FALSE)
l3 = lmer(pitch~gender+attitude+gender*attitude+(1|subject)+(1|scenario), data=formality, REML=FALSE)
anova(l0,l1,l2,l3)

l4 = lmer(pitch~(1|subject)+(1|scenario),data=formality, REML=FALSE)
l5 = lmer(pitch~attitude+(1|subject)+(1|scenario), data=formality, REML=FALSE)
l6 = lmer(pitch~attitude+gender+(1|subject)+(1|scenario), data=formality, REML=FALSE)
l7 = lmer(pitch~attitude+gender+gender*attitude+(1|subject)+(1|scenario), data=formality, REML=FALSE )
anova(l4,l5,l6,l7)

l8 = lmer(pitch~gender*attitude+(1|subject)+(1|scenario),data=formality, REML=FALSE)
anova(l8)

#결론
#성별과 태도 모두 목소리 높이에 유의한 영향을 미치며, 성별과 태도의 상호작용은 목소리 높이에 유의한 영향을 미치지 못한다.
#성별과 태도 두 요인이 발화자의 목소리 높이를 달라지게 하는 요인이다.

