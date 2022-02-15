# the rainbow passage.txt 에 들어있는 데이터를 tx라는 변수에 저장.
tx <- "When the sunlight strikes raindrops in the air, they act as a prism and form a rainbow. The rainbow is a division of white light into many beautiful colors. These take the shape of a long round arch, with its path high above, and its two ends apparently beyond the horizon. There is, according to legend, a boiling pot of gold at one end. People look, but no one ever finds it. When a man looks for something beyond his reach, his friends say he is looking for the pot of gold at the end of the rainbow. Throughout the centuries people have explained the rainbow in various ways. Some have accepted it as a miracle without physical explanation. To the Hebrews it was a token that there would be no more universal floods. The Greeks used to imagine that it was a sign from the gods to foretell war or heavy rain. The Norsemen considered the rainbow as a bridge over which the gods passed from earth to their home in the sky. Others have tried to explain the phenomenon physically. Aristotle thought that the rainbow was caused by reflection of the sun's rays by the rain. Since then physicists have found that it is not reflection, but refraction by the raindrops which causes the rainbows. Many complicated ideas about the rainbow have been formed. The difference in the rainbow depends considerably upon the size of the drops, and the width of the colored band increases as the size of the drops increases. The actual primary rainbow observed is said to be the effect of super-imposition of a number of bows. If the red of the second bow falls upon the green of the first, the result is to give a bow with an abnormally wide yellow band, since red and green light when mixed form yellow. This is a very common type of bow, one showing mainly red and yellow, with little or no green or blue."
tx <- gsub("[.,]","",tx) #punctuation 제거
tx <- gsub("-"," ",tx) #super-imposition에서 접두사 super와 어근 imposition을 분리함.

words <- strsplit(tx, split=" ")
is(words) #"list"   "vector"
words <- unlist(words)
is(words) #"character"           "vector"              "data.frameRowLabels" "SuperClassMethod"
words <- tolower(words) #모든 단어를 소문자로 바꾼다.(ex: The, the를 모두 the, the로 바꾼다 -> 나중에 unique한 단어들로 바뀌도록 하기 위해서.)

#1. word and syllable analysis
#List up all the unique words
u_words <- unique(words) #unique한 단어들을 저장.
print(u_words) #unique한 단어들을 출력

#count each word frequency
words <- data.frame(words)
tw <- table(words) # table()은 값들을 빈도로 정리해 줌.
dtw <- data.frame(tw)
print(dtw) #각 단어와 해당 단어의 빈도를 출력. --> 단어들은 unique한 단어들로, 오름차순으로 제시됨.

# Count number of letters/syllables for each word
#각 단어의 글자 개수를 구하기
cat('단어',':','글자개수')
for (x in words$words){
  cat(x,':',nchar(x),'개',end='\n')
}

#각 단어의 음절 개수를 구하기
cat('단어',':','음절개수')
vowels = c('a','e','i','o','u')
for (word in words$words){
  word2 = strsplit(word,split="")
  word2 = unlist(word2)
  count=0
  for (w in word2){
      if (w %in% vowels){
      count = count + length(w)
      }
  }
  cat(word,':',count,'개',end='\n')
}

#2. Give a descriptive stats on the word frequency and word length
word_length=nchar(sort(u_words)) #unique한 단어들을 오름차순 정렬하여 각 단어의 길이(문자개수)를 담은 벡터
word_length=data.frame(word_length) #word_length라는 데이터프레임으로 변형.
rainbow = data.frame(dtw, word_length)#unique한 각 단어들의 빈도와 길이를 모두 담은 데이터프레임 만들기.
rainbow 

#word frequency
#평균
mean(rainbow$Freq)
#중앙값
median(rainbow$Freq)
#최빈값
myt = table(rainbow$Freq)
names(myt)[myt==max(myt)]
#범위
range(rainbow$Freq)
min(rainbow$Freq)
max(rainbow$Freq)
#분산
var(rainbow$Freq)
#표준편차
sd(rainbow$Freq)
#사분위수 범위
IQR(rainbow$Freq)
#boxplot
boxplot(rainbow$Freq,main='boxplot of word_frequency',col='green')
hist(rainbow$Freq, main='Frequency of word frequency', xlab='word frequency',ylab='frequency of word frequency',col='yellow')
#word length
#평균
mean(rainbow$word_length)
#중앙값
median(rainbow$word_length)
#최빈값
myt2 = table(rainbow$word_length)
names(myt2)[myt2 == max(myt2)]
#범위
range(rainbow$word_length)
min(rainbow$word_length)
max(rainbow$word_length)
#분산
var(rainbow$word_length)
#표준편차
sd(rainbow$word_length)
#사분위수 범위
IQR(rainbow$word_length)
#boxplot
boxplot(word_length, main='boxplot of word_length',col='green')
hist(rainbow$word_length,main='Frequency of word length', xlab='word length',ylab='frequency of word length',col='yellow')


#3. Give a comprehensive analysis on the word frequency and word length assuming that your data represents the general tendency
library(moments)

# word frequency의 skewness와 kurtosis
skewness(rainbow$Freq)
kurtosis(rainbow$Freq)

# word length의 skewness와 kurtosis
skewness(rainbow$word_length)
kurtosis(rainbow$word_length)
