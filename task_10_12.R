#1. install and load the package.
load(languageR)

#2. check the data 'verbs' with functions: str(), colnames(), head(), and summary()
str(Verbs)
colnames(verbs)
head(verbs)
tail(verbs)
View(verbs)
summary(verbs)

#3. select rows whose verb element is 'give'. Save this data as 'v_give.csv' for future use.
v_give = subset(verbs, verbs$Verb=='give')
write.csv(v_give, file='C:/RClass/v_give.csv')

#4. Select all rows where the verb is 'give' and the structure is 'PP'
subset(verbs, verbs$Verb=='give' & verbs$RealizationOfRec=='PP')

#5. Compute percentage of the 'PP' and 'NP' structure, respectively.

#A. 전체 데이터에 대한 분석 결과
summary(Verbs)
  # NP: 555개
  # PP: 348개
  # RealizationOfRec의 총 개수 = 555 + 348 = 903
cat('NP 개수:',555, 'PP 개수:', 348)
cat('전체 RealizationOfRec의 총 개수:', 903)

## 전체 데이터에 대한 PP구조의 퍼센트
pp = 100*(348/903)
cat('PP의 퍼센트:', pp,'%')

## 전체 데이터에 대한 NP구조의 퍼센트
np = 100*(555/903)
cat('NP의 퍼센트:', np,'%')

cat('전체 문장에서 동사들이 가지는 NP구조의 비율이 PP구조의 비율에 비해', np/pp,'배 더 많다.' )

#B. 동사 'give'에 대한 분석결과
give= subset(verbs, verbs$Verb=='give')
summary(give)
  #NP: 327
  #PP: 76
  #전체 개수: 327+76 = 403
  
cat('NP 개수:',327, 'PP 개수:', 76)
cat('전체 RealizationOfRec의 총 개수:', 403)

## 동사 'give'에 대한 PP구조의 퍼센트
pp2 = 100*(76/403)
cat('PP의 퍼센트:', pp,'%')

## 동사 'give'에 대한 NP구조의 퍼센트
np2 = 100*(327/403)
cat('NP의 퍼센트:', np,'%')

cat('동사 give가 가지는 NP구조의 비율이 PP구조의 비율에 비해', np2/pp2,'배 더 많다.' )