#문제 1
#1) character
#2) numeric - continuous
#3) numeric - ordinal
#4) character
#5) numeric - nominal
#6) numeric - continuous

#문제 2
#1) matrix의 특징
#답: 데이터의 각 요소들(elements)이 행과 열로 구성된 2차원 구조로 배치된다. 단지 2차원 형태로 배치될 뿐, 체계적인 틀 속에 저장되는 건 아니다.
#또한 각각의 요소들을 행과 열에 따라 찾을 수 있다. 즉, 몇행 몇열에 어떤 요소가 있는지 알 수 있다.

#2) data frame의 특징
#답: 데이터의 각 요소들(elements)이 행과 열로 구성된 2차원의 '틀'에 저장된다. 
# data frame도 각각의 요소들을 행과 열에 따라 찾을 수 있다. 즉, 몇행 몇열에 어떤 요소가 들어 있다는 것을 알 수 있다.

#3) matrix와 data frame의 공통점
#답: matrix와 data frame 모두 데이터의 요소들을 2차원 형태로 배치한다는 점에서는 공통점이 있다.
# 따라서 각각의 요소들을, 2차원을 구성하는 행과 열에 따라 찾을 수 있다.

#4) matrix와 data frame의 차이점
#답: 
# 첫 번째로, matrix에 저장되는 요소들의 데이터 유형(data type)은 행과 열에 상관 없이 모두 같아야 하는 반면, 
# data frame에 저장되는 요소들의 데이터 유형은 어떤 데이터 유형이든 상관 없다.
# 즉, matrix에는 한 가지의 데이터 유형에 해당하는 요소들만 저장될 수 있지만, data frame에는 여러 종류의 데이터 유형에 해당하는 요소들이 저장될 수 있다.
# 대신 dataframe의 각 열에는 같은 유형의 데이터가 들어와야 한다. 반면, 각 행에는 서로 다른 유형의 데이터가 배치될 수 있다.

# 두 번째로, matrix에는 요소들이 특정 '틀'에 저장되어 있지 않지만, data frame에는 요소들이 표의 형태로 구성된 구체적인 '틀'에 저장된다는 차이점이 있다.


#문제 3
convert = function(inch){
cm = inch * 2.54
cat(inch,'inch가',cm,'cm로 변환되었습니다.')
}

convert(30)

#문제 4
install.packages('languageR')
library(languageR)

head(verbs)
tail(verbs)
summary(verbs)
str(verbs)
colnames(verbs)
dim(verbs)
length(verbs)
View(verbs)

send = verbs[verbs$Verb == 'send',]
write.csv(send, file = './send.csv')

#문제 5-1
words = c('mild', 'soccer', 'philosophy', 'element', 'tragedy', 'elf', 'empowerment', 'alternative', 'burden')

mean_charlen = mean(c(nchar(words))) #단어들의 길이의 평균

for (word in words){
  if (nchar(word) < mean_charlen){
    cat(word)  
    cat(': 초급 beginner\n')
  }
  else if (nchar(word) == mean_charlen){
    cat(word)  
    cat(': 중급 intermediate\n')
  }
  else{
    cat(word)  
    cat(': 고급 advanced\n')
  }
}


#문제 5-2
word_level = function(words){
  for (word in words){
    if (nchar(word) < mean_charlen){
      cat(word)  
      cat(': 초급 beginner\n')
      }
    else if (nchar(word) == mean_charlen){
      cat(word)  
      cat(': 중급 intermediate\n')
      }
    else{
      cat(word)  
      cat(': 고급 advanced\n')
      }
    }
}

word_level(words)

#문제 6
NAME <- c('Kim', 'Park', 'Choi', 'Yoon')
WEIGHT <- c(56, 45, 67, 84)
HEIGHT <- c(160, 165, 171, 180)
BLOOD_TYPE <- c('A', 'B', 'O', 'AB')
person_info <- data.frame(NAME, WEIGHT, HEIGHT, BLOOD_TYPE)
print(person_info)

#문제7
#1)
text <- "A Fox one day spied a beautiful bunch of ripe grapes hanging from a vine trained along the branches of a tree. The grapes seemed ready to burst with juice, and the Fox's mouth watered as he gazed longingly at them. The bunch hung from a high branch, and the Fox had to jump for it. The first time he jumped he missed it by a long way. So he walked off a short distance and took a running leap at it, only to fall short once more. Again and again he tried, but in vain. Now he sat down and looked at the grapes in disgust."
word_of_text <- strsplit(text, split=' ') #문장을 띄어쓰기를 기준으로 각 단어들로 분할한다.
word_of_text <- unlist(word_of_text) #단어들을 리스트 형태에서 벡터형태로 변환함.
lenword <- nchar(word_of_text) #각 단어의 길이를 저장
wordlen <- data.frame(word_of_text, lenword) #단어와 단어의 길이를 데이터프레임으로 저장.
write.csv(wordlen, file='./wordlength.csv') #만들어진 데이터프레임을 csv형태로 컴퓨터의 현재 디렉토리에 저장.

#2)
text <- "A Fox one day spied a beautiful bunch of ripe grapes hanging from a vine trained along the branches of a tree. The grapes seemed ready to burst with juice, and the Fox's mouth watered as he gazed longingly at them. The bunch hung from a high branch, and the Fox had to jump for it. The first time he jumped he missed it by a long way. So he walked off a short distance and took a running leap at it, only to fall short once more. Again and again he tried, but in vain. Now he sat down and looked at the grapes in disgust."
word_of_text <- strsplit(text, split=' ')
word_of_text <- unlist(word_of_text)
word <- data.frame(word_of_text)
vowel_a <- subset(word, grepl('a', word_of_text)) #모음 a만 들어있는 단어들 찾기
print(vowel_a) #모음 a만 들어있는 단어들을 출력하기

#3)
text <- "A Fox one day spied a beautiful bunch of ripe grapes hanging from a vine trained along the branches of a tree. The grapes seemed ready to burst with juice, and the Fox's mouth watered as he gazed longingly at them. The bunch hung from a high branch, and the Fox had to jump for it. The first time he jumped he missed it by a long way. So he walked off a short distance and took a running leap at it, only to fall short once more. Again and again he tried, but in vain. Now he sat down and looked at the grapes in disgust."
word_of_text <- strsplit(text, split=' ')
word_of_text <- unlist(word_of_text)
unique_words <- unique(word_of_text)

sort(unique_words) #중복을 제외한 전체 단어를 알파벳 순으로 정렬하여 출력
sort(unique_words, decreasing = TRUE) #중복을 제외한 전체 단어를 역순으로 정렬하여 출력

#4)
text <- "A Fox one day spied a beautiful bunch of ripe grapes hanging from a vine trained along the branches of a tree. The grapes seemed ready to burst with juice, and the Fox's mouth watered as he gazed longingly at them. The bunch hung from a high branch, and the Fox had to jump for it. The first time he jumped he missed it by a long way. So he walked off a short distance and took a running leap at it, only to fall short once more. Again and again he tried, but in vain. Now he sat down and looked at the grapes in disgust."
word_of_text <- strsplit(text, split=' ')
word_of_text <- unlist(word_of_text)
unique_words <- unique(word_of_text)

print(length(word_of_text)) #전체 단어의 개수 출력
print(length(unique_words)) #중복을 제외한 단어의 개수 출력


#문제 8
#1)
freqword <- read.csv('./w_frequency.csv') #파일 읽기

#2)
plant <- subset(freqword, freqword$Class == 'plant')
animal <- subset(freqword, freqword$Class == 'animal')

#3)
##### 2)에서 정의한 plant, animal 변수를 활용함.#####
sum(plant$Frequency) # 'plant'인 단어들의 출현빈도의 합
sum(animal$Frequency) # 'animal'인 단어들의 출현빈도의 합

#4)
# 우선, 전체 데이터에서 Class변수의 값의 종류를 확인한다.
unique(freqword$Class) #값의 종류에는 'plant'와 'animal' 두 개 뿐이다.

# 특정 단어의 출현빈도 비율 = 특정 단어의 출현빈도의 합 / 전체 단어의 출현빈도의 합
pl <- 100 * (sum(plant$Frequency) / sum(freqword$Frequency)) #plant 단어의 출현 빈도 비율
an <- 100 * (sum(animal$Frequency) / sum(freqword$Frequency)) #animal 단어의 출현 빈도 비율

cat('animal 단어의 출현 빈도 비율은',an,'% 이고 plant 단어의 출현빈도비율은',pl,'% 이다.')
cat('animal단어는 plant단어보다 ', an/pl, '배 더 많이 사용되었다.')

#결과에 대한 설명
#animal단어의 출현 빈도 비율은 61.22379% 이고,
#plant 단어의 출현 빈도 비율은 38.77621% 이므로,
#w_frequency.csv파일에 들어있는 데이터에는
#animal 단어가 plant 단어보다 약 1.6배 더 많이 사용되었다고 분석할 수 있다.
