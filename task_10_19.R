#useful functions: strsplit(), nchar(), toupper(), grepl()

#Q1. Show how many words the 'text' has

st <- strsplit(text, split=' ') #문자열을 띄어쓰기를 기준으로 각각의 토큰으로 분할.
is(st) #데이터 타입이 리스트 -> 벡터 형태로 변환 필요.
stv <- unlist(st) #unlist(리스트) : 리스트 형태의 타입을 벡터 형태의 타입으로 바꿔주는 함수
is(stv) #데이터 타입이 벡터 형태로 잘 변환되었다.
n_word = length(stv) #length()는 벡터의 길이를 알려줌. 여기서 벡터의 요소는 각 단어들이므로, 단어의 개수를 구하는 것임.
cat('text에서 단어의 개수는',n_word,'개 입니다.')

#Q2. Count words with an 'i' vowel

df = data.frame(stv)
df #변수명이 stv로 되어 있는 데이터프레임을 생성
subset(df, grepl('i', stv)) #grepl로 stv변수의 값들 중 모음 i를 포함하는 행을 뽑아낸다.
newdf <- subset(df, grepl('i', stv)) #뽑아낸 단어들을 새로운 데이터프레임 newdf로 만든다
vec <- c(newdf) #데이터 프레임의 값들을 벡터 형태로 변환한다. 
is(vec) #그러나 아직 리스트 형태로 저장되어 있다.
vec2 <- unlist(vec) #리스트 형태를 벡터 형태로 변환.
n_word_with_i <- length(vec2) #i 모음이 들어있는 단어의 개수를 n_word_i 라는 변수에 저장.
cat('text에서 모음 i가 들어있는 단어의 개수는', nword_with_i,'개 입니다.')

#Q3. Print out each word in upper case letters and its number of characters

for (w in stv){
  cat(toupper(w),'의 글자 개수는',nchar(w),'개 입니다.\n') 
}
#toupper(문자열) -> 대문자로 바꿔줌
#nchar(문자열) -> 문자열의 글자 개수를 셈(영어 단어의 알파벳 개수)
