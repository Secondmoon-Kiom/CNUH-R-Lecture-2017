#----------------------------------------------------------------------------------------------------------------------------#
# 충남대학교병원 임상시험센터 R 강의 실습 1: 데이터 조작
#  사용자료: 
#  1) Chen, Ding-Geng Din, and Karl E. Peace. Clinical trial data analysis using R. CRC Press, 2010.
#     파일이름: ../data/datR4CTDA.xlsx
#  2) http://biostat.mc.vanderbilt.edu/wiki/Main/DataSets 의 diabetes.xls (txt 확장자로 저장)
#----------------------------------------------------------------------------------------------------------------------------#

## 들어가기 앞서 현재 R 상태를 확인해보자!!
rm(list = ls())
sessionInfo()

## 필요한 패키지 설치 및  불러오자!!
pkgName <- c("MASS", "tidyverse", "ggthemes", "readxl", "ProjectTemplate", "kableExtra", 
             "ztable", "car", "lsmeans")

## Package 설치
install.packages(pkgName, dependencies = T)

## 패키지 불러오기
lapply(pkgName, require, character.only = T)

## Project 불러오기
load.project()

## Workspace 확인
ls()

## RStudio menu 에서 자료 불러오기
### Preview 복사
library(readr)
diabetes <- read_delim("rawdata/diabetes.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
View(diabetes)

## `read.table`로 자료 불러오기 (Diabetes)
DIAB <- read.table("rawdata/diabetes.txt", sep = "\t", header = T)
str(DIAB); dim(DIAB); 

## Excel 파일 불러오기: datR4CTDA.xlsx
### readxl로 모든 시트 불러오기
path <- "data/datR4CTDA.xlsx"
DFL1 <- path %>% excel_sheets() %>% 
  set_names() %>% map(read_excel, path = path)

### XLConnect를 이용하여 자료 불러오기 
# install.packages(XLConnect)
.rs.restartR() # R Session 다시 시작
source("lib/ReadExcel.R")
DFL2 <- ReadExcel(path)
rm(DFL2)

## DFL1 리스트의 객체를 개별 데이터프레임에 저장
command <- paste(names(DFL1), "<- DFL1$", names(DFL1), sep="")
for(i in command) eval(parse(text=i))
ls()

## DBP 데이터프레임을 output/DBP.txt 로 저장
### write.table()
write.table(DBP, "output/DBP.txt", sep = "\t", row.names = F)
### write_excel_csv()
write_excel_csv(DBP, "output/DBP.csv")

## 데이터프레임 조작
head(DBP); dim(DBP); summary(DBP)
## 데이터 프레임 색인
DBP[c(1,3,5), ] # 1,3,5열 자료만 추출


## dplyr 기본 사용법
# filter(조건식에 맞는 데이터 추출)
# select(열 추출)
# mutate(열 추가 및 변경)	
# arrange(정렬)
# summarise(집계)
# %>% 연산자 RStudio 단축키는 Ctrl + Shift + M

DBP1 <- DBP %>% 
  mutate()
