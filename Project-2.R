library(tidyverse)
library(rvest)
library(stringr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)

write.csv(tidy_albums, "Paramore_Lyrics.csv", row.names = FALSE)

#Song 1 URL
url_2005_1 <- paste0("https://songmeanings.com/songs/view/3530822107858550867/") 
s2005_1<- read_html(url_2005_1)
#Scrapping Song 1 
paragraphs1 = html_node(s2005_1, '.lyric-box')
pText1 = html_text(paragraphs1)
lyrics_text <- paste(pText1)
print(head(pText1))
#Cleaning Song 1 Data
clean_2005_1 <- gsub("\\\t|\\\n", '', lyrics_text, perl = TRUE)
remove_phrase <- "Edit Lyrics"
clean_2005_11 <- gsub(remove_phrase, "", clean_2005_1)
clean_2005_12 <- gsub("'", "", clean_2005_11)
All_We_Know1 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2005_12)
All_We_Know <- str_replace_all(All_We_Know1, "\\?", "? ")
print(All_We_Know)

#Scrapping Song 2 
url_2005_2 <- paste0("https://songmeanings.com/songs/view/3530822107858545848/") 
s2005_2<- read_html(url_2005_2)
paragraphs2 = html_node(s2005_2, '.lyric-box')
pText2 = html_text(paragraphs2)
lyrics_text1 <- paste(pText2)
print(head(pText2))
clean_2005_2 <- gsub("\\\t|\\\n", '', lyrics_text1, perl = TRUE)
clean_2005_21 <- gsub(remove_phrase, "", clean_2005_2)
clean_2005_22 <- gsub("'", "", clean_2005_21)
Pressure <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2005_22)
print(Pressure)

#Scrapping Song 3 
url_2005_3 <- paste0("https://songmeanings.com/songs/view/3530822107858547400/")
s2005_3<- read_html(url_2005_3)
paragraphs3 = html_node(s2005_3, '.lyric-box')
pText3 = html_text(paragraphs3)
lyrics_text2 <- paste(pText3)
print(head(pText3))
clean_2005_3 <- gsub("\\\t|\\\n", '', lyrics_text2, perl = TRUE)
clean_2005_31 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2005_3)
clean_2005_32 <- gsub("'", "", clean_2005_31)
clean_2005_33 <- gsub("wrongCause", "wrong Cause", clean_2005_32)
Emergency1 <- gsub(remove_phrase, "", clean_2005_33)
Emergency <- str_replace_all(Emergency1, "\\?", "? ")
print(Emergency)

#Scrapping Song 4
url_2005_4 <- paste0("https://songmeanings.com/songs/view/3530822107858547405/")
s2005_4<- read_html(url_2005_4)
paragraphs4 = html_node(s2005_4, '.lyric-box')
pText4 = html_text(paragraphs4)
lyrics_text3 <- paste(pText4)
print(head(pText4))
clean_2005_4 <- gsub("\\\t|\\\n", '', lyrics_text3, perl = TRUE)
clean_2005_41 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2005_4)
Brighter1 <- gsub(remove_phrase, "", clean_2005_41)
Brighter <- str_replace_all(Brighter1, "\\)", ") ")
print(Brighter)

#Scrapping Song 5
url_2005_5 <- paste0("https://songmeanings.com/songs/view/3530822107858545859/")
s2005_5<- read_html(url_2005_5)
paragraphs5 = html_node(s2005_5, '.lyric-box')
pText5 = html_text(paragraphs5)
lyrics_text4 <- paste(pText5)
print(head(pText5))
clean_2005_5 <- gsub("\\\t|\\\n", '', lyrics_text4, perl = TRUE)
clean_2005_51 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2005_5)
Here_We_Go_Again1 <- gsub(remove_phrase, "", clean_2005_51)
Here_We_Go_Again <- str_replace_all(Here_We_Go_Again1, "\\)", ") ")
print(Here_We_Go_Again)

#Scrapping Song 6 
url_2005_6 <- paste0("https://songmeanings.com/songs/view/3530822107858547407/")
s2005_6<- read_html(url_2005_6)
paragraphs6 = html_node(s2005_6, '.lyric-box')
pText6 = html_text(paragraphs6)
lyrics_text5 <- paste(pText6)
print(head(pText6))
clean_2005_6 <- gsub("\\\t|\\\n", '', lyrics_text5, perl = TRUE)
clean_2005_61 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2005_6)
Never_Let_This_Go1 <- gsub(remove_phrase, "", clean_2005_61)
Never_Let_This_Go <- str_replace_all(Never_Let_This_Go1, "\\,", ", ")
print(Never_Let_This_Go)

#Scrapping Song 7 
url_2005_7 <- paste0("https://songmeanings.com/songs/view/3530822107858549647/")
s2005_7<- read_html(url_2005_7)
paragraphs7 = html_node(s2005_7, '.lyric-box')
pText7 = html_text(paragraphs7)
lyrics_text6 <- paste(pText7)
print(head(pText7))
clean_2005_7 <- gsub("\\\t|\\\n", '', lyrics_text6, perl = TRUE)
clean_2005_71 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2005_7)
clean_2005_72 <- gsub("'", "", clean_2005_71)
clean_2005_73 <- gsub("wrongCause", "wrong Cause", clean_2005_72)
Whoa1 <- gsub(remove_phrase, "", clean_2005_73)
Whoa <- str_replace_all(Whoa1, "\\-", "- ")
print(Whoa)

#Scrapping Song 8 
url_2005_8 <- paste0("https://songmeanings.com/songs/view/3530822107858547409/")
s2005_8<- read_html(url_2005_8)
paragraphs8 = html_node(s2005_8, '.lyric-box')
pText8 = html_text(paragraphs8)
lyrics_text7 <- paste(pText8)
print(head(pText8))
clean_2005_8 <- gsub("\\\t|\\\n", '', lyrics_text7, perl = TRUE)
clean_2005_81 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2005_8)
clean_2005_82 <- gsub("'", "", clean_2005_81)
Conspiracy <- gsub(remove_phrase, "", clean_2005_82)
print(Conspiracy)

#Scrapping Song 9
url_2005_9 <- paste0("https://songmeanings.com/songs/view/3530822107858547416/")
s2005_9<- read_html(url_2005_9)
paragraphs9 = html_node(s2005_9, '.lyric-box')
pText9 = html_text(paragraphs9)
lyrics_text8 <- paste(pText9)
print(head(pText9))
clean_2005_9 <- gsub("\\\t|\\\n", '', lyrics_text8, perl = TRUE)
clean_2005_91 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2005_9)
Franklin1 <- gsub(remove_phrase, "", clean_2005_91)
Franklin2 <- gsub("'", "", Franklin1)
Franklin <- str_replace_all(Franklin2, "\\?", "? ")
print(Franklin)

#Scrapping Song 10
url_2005_10 <- paste0("https://songmeanings.com/songs/view/3530822107858547417/")
s2005_10<- read_html(url_2005_10)
paragraphs10 = html_node(s2005_10, '.lyric-box')
pText10 = html_text(paragraphs10)
lyrics_text9 <- paste(pText10)
print(head(pText10))
clean_2005_10 <- gsub("\\\t|\\\n", '', lyrics_text9, perl = TRUE)
clean_2005_101 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2005_10)
My_Heart1 <- gsub(remove_phrase, "", clean_2005_101)
My_Heart2 <- str_replace_all(My_Heart1, "\\(", " (")
My_Heart <- str_replace_all(My_Heart2, "\\)", ") ")
print(My_Heart)

#Scrapping Album 2- Song 1
url_2007_1 <-paste0("https://songmeanings.com/songs/view/3530822107858660469/")
s2007_1<- read_html(url_2007_1)
paragraphs11 = html_node(s2007_1, '.lyric-box')
pText11 = html_text(paragraphs11)
lyrics_text10 <- paste(pText11)
print(head(pText11))
clean_2007_1 <- gsub("\\\t|\\\n", '', lyrics_text10, perl = TRUE)
clean_2007_11 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2007_1)
For_A_Pessimist_Im_Pretty_Optimistic1 <- gsub(remove_phrase, "", clean_2007_11)
For_A_Pessimist_Im_Pretty_Optimistic <- str_replace_all(For_A_Pessimist_Im_Pretty_Optimistic1, "\\?", "? ")
print(For_A_Pessimist_Im_Pretty_Optimistic)

#Scrapping Album 2- Song 2
url_2007_2 <-paste0("https://songmeanings.com/songs/view/3530822107858667746/")
s2007_2<- read_html(url_2007_2)
paragraphs12 = html_node(s2007_2, '.lyric-box')
pText12 = html_text(paragraphs12)
lyrics_text11 <- paste(pText12)
print(head(pText12))
clean_2007_2 <- gsub("\\\t|\\\n", '', lyrics_text11, perl = TRUE)
clean_2007_21 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2007_2)
Thats_What_You_Get1 <- gsub(remove_phrase, "", clean_2007_21)
Thats_What_You_Get <- str_replace_all(Thats_What_You_Get1, "\\?", "? ")
print(Thats_What_You_Get)

#Scrapping Album 2- Song 3
url_2007_3 <-paste0("https://songmeanings.com/songs/view/3530822107858577440/")
s2007_3<- read_html(url_2007_3)
paragraphs13 = html_node(s2007_3, '.lyric-box')
pText13 = html_text(paragraphs13)
lyrics_text12 <- paste(pText13)
print(head(pText13))
clean_2007_3 <- gsub("\\\t|\\\n", '', lyrics_text12, perl = TRUE)
clean_2007_31 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2007_3)
Hallelujah1 <- gsub(remove_phrase, "", clean_2007_31)
Hallelujah <- str_replace_all(Hallelujah1, "[[:punct:]]", " ")
print(Hallelujah)


#Scrapping Album 2 - Song 4
url_2007_4 <-paste0("https://songmeanings.com/songs/view/3530822107858657546/")
s2007_4<- read_html(url_2007_4)
paragraphs14 = html_node(s2007_4, '.lyric-box')
pText14 = html_text(paragraphs14)
lyrics_text13 <- paste(pText14)
print(head(pText14))
clean_2007_4 <- gsub("\\\t|\\\n", '', lyrics_text13, perl = TRUE)
clean_2007_41 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2007_4)
Misery_Business1 <- gsub(remove_phrase, "", clean_2007_41)
Misery_Business2 <- gsub("'", "", Misery_Business1)
Misery_Business3 <- gsub("goodCause", "good Cause", Misery_Business2)
Misery_Business <- str_replace_all(Misery_Business3, "\\!", "! ")
print(Misery_Business)

#Scrapping Album 2 - Song 5 
url_2007_5 <-paste0("https://songmeanings.com/songs/view/3530822107858667744/")
s2007_5 <- read_html(url_2007_5)
paragraphs15 = html_node(s2007_5, '.lyric-box')
pText15 = html_text(paragraphs15)
lyrics_text14 <- paste(pText15)
print(head(pText15))
clean_2007_5 <- gsub("\\\t|\\\n", '', lyrics_text14, perl = TRUE)
clean_2007_51 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2007_5)
clean_2007_52 <- str_replace_all(clean_2007_51, "\\?", "? ")
When_It_Rains1 <- str_replace_all(clean_2007_52, "\\)", ") ")
When_It_Rains <- gsub(remove_phrase, "", When_It_Rains1)
print(When_It_Rains)

#Scrapping Album 2 - Song 6
url_2007_6 <-paste0("https://songmeanings.com/songs/view/3530822107858667742/")
s2007_6 <- read_html(url_2007_6)
paragraphs16 = html_node(s2007_6, '.lyric-box')
pText16 = html_text(paragraphs16)
lyrics_text15 <- paste(pText16)
print(head(pText16))
clean_2007_6 <- gsub("\\\t|\\\n", '', lyrics_text15, perl = TRUE)
clean_2007_61 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2007_6)
clean_2007_62 <- str_replace_all(clean_2007_61, "\\)", ") ")
Let_The_Flames_Begin <- gsub(remove_phrase, "", clean_2007_62)
print(Let_The_Flames_Begin)

#Scrapping Album 2 - Song 7
url_2007_7 <-paste0("https://songmeanings.com/songs/view/3530822107858667740/")
s2007_7 <- read_html(url_2007_7)
paragraphs17 = html_node(s2007_7, '.lyric-box')
pText17 = html_text(paragraphs17)
lyrics_text16 <- paste(pText17)
print(head(pText17))
clean_2007_7 <- gsub("\\\t|\\\n", '', lyrics_text16, perl = TRUE)
clean_2007_71 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2007_7)
clean_2007_72 <- str_replace_all(clean_2007_71, "\\)", ") ")
clean_2007_73 <- gsub("'", "", clean_2007_72)
clean_2007_74 <- gsub("IWhen", "I When", clean_2007_73)
clean_2007_75 <- gsub("goingCause", "going Cause", clean_2007_74)
clean_2007_76 <- gsub("IIm", "I Im", clean_2007_75)
Miracle <- gsub(remove_phrase, "", clean_2007_76)
print(Miracle)

#Scrapping Album 2 - Song 8
url_2007_8 <-paste0("https://songmeanings.com/songs/view/3530822107858667646/")
s2007_8 <- read_html(url_2007_8)
paragraphs18 = html_node(s2007_8, '.lyric-box')
pText18 = html_text(paragraphs18)
lyrics_text17 <- paste(pText18)
print(head(pText18))
clean_2007_8 <- gsub("\\\t|\\\n", '', lyrics_text17, perl = TRUE)
clean_2007_81 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2007_8)
clean_2007_82 <- str_replace_all(clean_2007_81, "\\)", ") ")
clean_2007_83 <- gsub(remove_phrase, "", clean_2007_82)
crushcrushcrush <- gsub("'", "", clean_2007_83)
print(crushcrushcrush)

#Scrapping Album 2 - Song 9
url_2007_9 <-paste0("https://songmeanings.com/songs/view/3530822107858667739/")
s2007_9 <- read_html(url_2007_9)
paragraphs19 = html_node(s2007_9, '.lyric-box')
pText19 = html_text(paragraphs19)
lyrics_text18 <- paste(pText19)
print(head(pText19))
clean_2007_9 <- gsub("\\\t|\\\n", '', lyrics_text18, perl = TRUE)
clean_2007_91 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2007_9)
clean_2007_92 <- str_replace_all(clean_2007_91, "\\)", ") ")
clean_2007_93 <- str_replace_all(clean_2007_92, "\\(", " (")
clean_2007_94 <- str_replace_all(clean_2007_93, "\\?", "? ")
We_Are_Broken <- gsub(remove_phrase, "", clean_2007_94)
print(We_Are_Broken)

#Scrapping Album 2 Song 10
url_2007_10 <-paste0("https://songmeanings.com/songs/view/3530822107858667738/")
s2007_10 <- read_html(url_2007_10)
paragraphs20 = html_node(s2007_10, '.lyric-box')
pText20 = html_text(paragraphs20)
lyrics_text19 <- paste(pText20)
print(head(pText20))
clean_2007_10 <- gsub("\\\t|\\\n", '', lyrics_text19, perl = TRUE)
clean_2007_101 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2007_10)
clean_2007_102 <- gsub(remove_phrase, "", clean_2007_101)
Fences <- gsub("wide'Cause", "wide Cause", clean_2007_102)
print(Fences)

#Scrapping Album 2 Song 11
url_2007_11 <-paste0("https://songmeanings.com/songs/view/3530822107858656163/")
s2007_11 <- read_html(url_2007_11)
paragraphs21 = html_node(s2007_11, '.lyric-box')
pText21 = html_text(paragraphs21)
lyrics_text20 <- paste(pText21)
print(head(pText21))
clean_2007_11 <- gsub("\\\t|\\\n", '', lyrics_text20, perl = TRUE)
clean_2007_111 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2007_11)
clean_2007_112 <- gsub(remove_phrase, "", clean_2007_111)
clean_2007_113 <- str_replace_all(clean_2007_112, "\\)", ") ")
Born_For_This <- str_replace_all(clean_2007_113, "\\?", "? ")
print(Born_For_This)

#Scrapping Album 3 Song 1 
url_2009_1 <-paste0("https://songmeanings.com/songs/view/3530822107858797592/")
s2009_1 <- read_html(url_2009_1)
paragraphs22 = html_node(s2009_1, '.lyric-box')
pText22 = html_text(paragraphs22)
lyrics_text21 <- paste(pText22)
print(head(pText22))
clean_2009_1 <- gsub("\\\t|\\\n", '', lyrics_text21, perl = TRUE)
clean_2009_11 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2009_1)
Careful <- gsub(remove_phrase, "", clean_2009_11)
print(Careful)

#Scrapping Album 3 Song 2 
url_2009_2 <-paste0("https://songmeanings.com/songs/view/3530822107858776850/")
s2009_2 <- read_html(url_2009_2)
paragraphs23 = html_node(s2009_2, '.lyric-box')
pText23 = html_text(paragraphs23)
lyrics_text22 <- paste(pText23)
print(head(pText23))
clean_2009_2 <- gsub("\\\t|\\\n", '', lyrics_text22, perl = TRUE)
clean_2009_21 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2009_2)
clean_2009_22 <- str_replace_all(clean_2009_21, "\\?", "? ")
clean_2009_23 <- str_replace_all(clean_2009_22, "\\)", ") ")
clean_2009_24 <- gsub("fault'Cause", "fault Cause", clean_2009_23)
Ignorance <- gsub(remove_phrase, "", clean_2009_24)
print(Ignorance)

#Scrapping Album 3 Song 3 
url_2009_3 <-paste0("https://songmeanings.com/songs/view/3530822107858777218/")
s2009_3 <- read_html(url_2009_3)
paragraphs24 = html_node(s2009_3, '.lyric-box')
pText24 = html_text(paragraphs24)
lyrics_text23 <- paste(pText24)
print(head(pText24))
clean_2009_3 <- gsub("\\\t|\\\n", '', lyrics_text23, perl = TRUE)
clean_2009_31 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2009_3)
clean_2009_32 <- str_replace_all(clean_2009_31, "\\?", "? ")
clean_2009_33 <- str_replace_all(clean_2009_32, "\\)", ") ")
clean_2009_34 <- str_replace_all(clean_2009_33, "\\(", " (")
Playing_God <- sub(remove_phrase, "", clean_2009_34)
print(Playing_God)

#Scrapping Album 3 Song 4
url_2009_4 <-paste0("https://songmeanings.com/songs/view/3530822107858786482/")
s2009_4 <- read_html(url_2009_4)
paragraphs25 = html_node(s2009_4, '.lyric-box')
pText25 = html_text(paragraphs25)
lyrics_text24 <- paste(pText25)
print(head(pText25))
clean_2009_4 <- gsub("\\\t|\\\n", '', lyrics_text24, perl = TRUE)
clean_2009_41 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2009_4)
clean_2009_42 <- str_replace_all(clean_2009_41, "\\)", ") ")
clean_2009_43 <- gsub("behind\"It's all about", "behind Its all about", clean_2009_42)
clean_2009_44 <- gsub("lens,\"", "the lens, I told her", clean_2009_43)
Brick_By_Boring_Brick <- sub(remove_phrase, "", clean_2009_44)
print(Brick_By_Boring_Brick)

#Scrapping Album 3 Song 5
url_2009_5 <-paste0("https://songmeanings.com/songs/view/3530822107858791788/")
s2009_5 <- read_html(url_2009_5)
paragraphs26 = html_node(s2009_5, '.lyric-box')
pText26 = html_text(paragraphs26)
lyrics_text25 <- paste(pText26)
print(head(pText26))
clean_2009_5 <- gsub("\\\t|\\\n", '', lyrics_text25, perl = TRUE)
clean_2009_51 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2009_5)
Only_Exception <- gsub(remove_phrase, "", clean_2009_51)
print(Only_Exception)

#Scrapping Album 3 Song 6
url_2009_6 <-paste0("https://songmeanings.com/songs/view/3530822107858791787/")
s2009_6 <- read_html(url_2009_6)
paragraphs27 = html_node(s2009_6, '.lyric-box')
pText27 = html_text(paragraphs27)
lyrics_text26 <- paste(pText27)
print(head(pText27))
clean_2009_6 <- gsub("\\\t|\\\n", '', lyrics_text26, perl = TRUE)
clean_2009_61 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2009_6)
clean_2009_62 <- str_replace_all(clean_2009_61, "\\)", ") ")
Turn_it_Off <- gsub(remove_phrase, "", clean_2009_62)
print(Turn_it_Off)

#Scrapping Album 3 Song 7
url_2009_7 <-paste0("https://songmeanings.com/songs/view/3530822107858797394/")
s2009_7 <- read_html(url_2009_7)
paragraphs28 = html_node(s2009_7, '.lyric-box')
pText28 = html_text(paragraphs28)
lyrics_text27 <- paste(pText28)
print(head(pText28))
clean_2009_7 <- gsub("\\\t|\\\n", '', lyrics_text27, perl = TRUE)
clean_2009_71 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2009_7)
clean_2009_72 <- str_replace_all(clean_2009_71, "\\?", "? ")
clean_2009_73 <- gsub("IWon't", "I Won't", clean_2009_72)
Feeling_Sorry <- gsub(remove_phrase, "", clean_2009_73)
print(Feeling_Sorry)

#Scrapping Album 3 Song 8
url_2009_8 <-paste0("https://songmeanings.com/songs/view/3530822107858797593/")
s2009_8 <- read_html(url_2009_8)
paragraphs29 = html_node(s2009_8, '.lyric-box')
pText29 = html_text(paragraphs29)
lyrics_text28 <- paste(pText29)
print(head(pText29))
clean_2009_8 <- gsub("\\\t|\\\n", '', lyrics_text28, perl = TRUE)
clean_2009_81 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2009_8)
clean_2009_82 <- str_replace_all(clean_2009_81, "\\?", "? ")
Looking_Up <- gsub(remove_phrase, "", clean_2009_82)
print(Looking_Up)

#Scrapping Album 3 Song 9
url_2009_9 <-paste0("https://songmeanings.com/songs/view/3530822107858777087/")
s2009_9 <- read_html(url_2009_9)
paragraphs30 = html_node(s2009_9, '.lyric-box')
pText30 = html_text(paragraphs30)
lyrics_text29 <- paste(pText30)
print(head(pText30))
clean_2009_9 <- gsub("\\\t|\\\n", '', lyrics_text29, perl = TRUE)
clean_2009_91 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2009_9)
Where_the_Lines_Overlap <- gsub(remove_phrase, "", clean_2009_91)
print(Where_the_Lines_Overlap)

#Scrapping Album 3 Song 10 
url_2009_10 <-paste0("https://songmeanings.com/songs/view/3530822107858797335/")
s2009_10 <- read_html(url_2009_10)
paragraphs31 = html_node(s2009_10, '.lyric-box')
pText31 = html_text(paragraphs31)
lyrics_text30 <- paste(pText31)
print(head(pText31))
clean_2009_10 <- gsub("\\\t|\\\n", '', lyrics_text30, perl = TRUE)
clean_2009_101 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2009_10)
clean_2009_102 <- gsub("classify,Our", "classify, Our", clean_2009_101)
Misguided_Ghosts <- gsub(remove_phrase, "", clean_2009_102)
print(Misguided_Ghosts)

#Scrapping Album 3 Song 11
url_2009_11 <-paste0("https://songmeanings.com/songs/view/3530822107858797334/")
s2009_11 <- read_html(url_2009_11)
paragraphs32 = html_node(s2009_11, '.lyric-box')
pText32 = html_text(paragraphs32)
lyrics_text31 <- paste(pText32)
print(head(pText32))
clean_2009_11 <- gsub("\\\t|\\\n", '', lyrics_text31, perl = TRUE)
clean_2009_112 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2009_11)
clean_2009_113 <- str_replace_all(clean_2009_112, "\\)", ") ")
clean_2009_114 <- gsub("black-and-white", "black and white", clean_2009_113)
All_I_Wanted <- gsub(remove_phrase, "", clean_2009_114)
print(All_I_Wanted)

#Scrapping Album 3 Song 12
url_2009_12 <-paste0("https://songmeanings.com/songs/view/3530822107858743010/")
s2009_12 <- read_html(url_2009_12)
paragraphs33 = html_node(s2009_12, '.lyric-box')
pText33 = html_text(paragraphs33)
lyrics_text32 <- paste(pText33)
print(head(pText33))
clean_2009_12 <- gsub("\\\t|\\\n", '', lyrics_text32, perl = TRUE)
clean_2009_122 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2009_12)
clean_2009_123 <- str_replace_all(clean_2009_122, "\\)", ") ")
clean_2009_124 <- str_replace_all(clean_2009_123, "\\(", " (")
clean_2009_125 <- str_replace_all(clean_2009_124, "\\?", "? ")
Decode <- gsub(remove_phrase, "", clean_2009_125)
print(Decode)

#Scrapping Album 4 Song 1
url_2013_1 <-paste0("https://songmeanings.com/songs/view/3530822107859454040/")
s2013_1 <- read_html(url_2013_1)
paragraphs34 = html_node(s2013_1, '.lyric-box')
pText34 = html_text(paragraphs34)
lyrics_text33 <- paste(pText34)
print(head(pText34))
clean_2013_1 <- gsub("\\\t|\\\n", '', lyrics_text33, perl = TRUE)
clean_2013_12 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_1)
Fast_in_my_Car <- gsub(remove_phrase, "", clean_2013_12)
print(Fast_in_my_Car)

#Scrapping Album 4 Song 2
url_2013_2 <-paste0("https://songmeanings.com/songs/view/3530822107859448401/")
s2013_2 <- read_html(url_2013_2)
paragraphs35 = html_node(s2013_2, '.lyric-box')
pText35 = html_text(paragraphs35)
lyrics_text34 <- paste(pText35)
print(head(pText35))
clean_2013_2 <- gsub("\\\t|\\\n", '', lyrics_text34, perl = TRUE)
clean_2013_21 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_2)
Now <- gsub(remove_phrase, "", clean_2013_21)
print(Now)

#Scrapping Album 4 Song 3
url_2013_3 <-paste0("https://songmeanings.com/songs/view/3530822107859454041/")
s2013_3 <- read_html(url_2013_3)
paragraphs36 = html_node(s2013_3, '.lyric-box')
pText36 = html_text(paragraphs36)
lyrics_text35 <- paste(pText36)
print(head(pText36))
clean_2013_3 <- gsub("\\\t|\\\n", '', lyrics_text35, perl = TRUE)
clean_2013_31 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_3)
clean_2013_32 <- str_replace_all(clean_2013_31, "\\)", ") ")
clean_2013_33 <- str_replace_all(clean_2013_32, "\\(", " (")
Grow_Up <- gsub(remove_phrase, "", clean_2013_33)
print(Grow_Up)

#Scrapping Album 4 Song 4 
url_2013_4 <-paste0("https://songmeanings.com/songs/view/3530822107859454042/")
s2013_4 <- read_html(url_2013_4)
paragraphs37 = html_node(s2013_4, '.lyric-box')
pText37 = html_text(paragraphs37)
lyrics_text36 <- paste(pText37)
print(head(pText37))
clean_2013_4 <- gsub("\\\t|\\\n", '', lyrics_text36, perl = TRUE)
clean_2013_41 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_4)
clean_2013_42 <- str_replace_all(clean_2013_41, "\\)", ") ")
Daydreaming <- gsub(remove_phrase, "", clean_2013_42)
print(Daydreaming)

#Scrapping Album 4 Song 5
url_2013_5 <-paste0("https://songmeanings.com/songs/view/3530822107859454043/")
s2013_5 <- read_html(url_2013_5)
paragraphs38 = html_node(s2013_5, '.lyric-box')
pText38 = html_text(paragraphs38)
lyrics_text37 <- paste(pText38)
print(head(pText38))
clean_2013_5 <- gsub("\\\t|\\\n", '', lyrics_text37, perl = TRUE)
clean_2013_51 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_5)
clean_2013_52 <- gsub("'", "", clean_2013_51)
Interlude_Moving_On <- gsub(remove_phrase, "", clean_2013_52)
print(Interlude_Moving_On)

#Scrapping Album 4 Song 6
url_2013_6 <-paste0("https://songmeanings.com/songs/view/3530822107859454044/")
s2013_6 <- read_html(url_2013_6)
paragraphs39 = html_node(s2013_6, '.lyric-box')
pText39 = html_text(paragraphs39)
lyrics_text38 <- paste(pText39)
print(head(pText38))
clean_2013_6 <- gsub("\\\t|\\\n", '', lyrics_text38, perl = TRUE)
clean_2013_61 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_6)
clean_2013_62 <- str_replace_all(clean_2013_61, "\\)", ") ")
clean_2013_63 <- gsub("'", "", clean_2013_62)
clean_2013_64 <- str_replace_all(clean_2013_63, "\\?", "? ")
clean_2013_65 <- gsub("mamaCause", "mama Cause", clean_2013_64)
Aint_it_Fun <- gsub(remove_phrase, "", clean_2013_65)
print(Aint_it_Fun)

#Scrapping Album 4 Song 7 
url_2013_7 <-paste0("https://songmeanings.com/songs/view/3530822107859454045/")
s2013_7 <- read_html(url_2013_7)
paragraphs40 = html_node(s2013_7, '.lyric-box')
pText40 = html_text(paragraphs40)
lyrics_text39 <- paste(pText40)
print(head(pText40))
clean_2013_7 <- gsub("\\\t|\\\n|\\\r", '', lyrics_text39, perl = TRUE)
clean_2013_71 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_7)
clean_2013_72 <- str_replace_all(clean_2013_71, "\\)", ") ")
clean_2013_73 <- str_replace_all(clean_2013_72, "\\(", " (")
Part_II <- gsub(remove_phrase, "", clean_2013_73)
print(Part_II)

#Scrapping Album 4 Song 8 
url_2013_8 <-paste0("https://songmeanings.com/songs/view/3530822107859454046/")
s2013_8 <- read_html(url_2013_8)
paragraphs41 = html_node(s2013_8, '.lyric-box')
pText41 = html_text(paragraphs41)
lyrics_text40 <- paste(pText41)
print(head(pText41))
clean_2013_8 <- gsub("\\\t|\\\n", '', lyrics_text40, perl = TRUE)
clean_2013_81 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_8)
Last_Hope <- gsub(remove_phrase, "", clean_2013_81)
print(Last_Hope)

#Scrapping Album 4 Song 9
url_2013_9 <-paste0("https://songmeanings.com/songs/view/3530822107859452823/")
s2013_9 <- read_html(url_2013_9)
paragraphs42 = html_node(s2013_9, '.lyric-box')
pText42 = html_text(paragraphs42)
lyrics_text41 <- paste(pText42)
print(head(pText42))
clean_2013_9 <- gsub("\\\t|\\\n", '', lyrics_text41, perl = TRUE)
clean_2013_91 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_9)
clean_2013_92 <- str_replace_all(clean_2013_91, "\\)", ") ")
clean_2013_93 <-  gsub("'", "", clean_2013_92)
Still_Into_You <- gsub(remove_phrase, "", clean_2013_93)
print(Still_Into_You)

#Scrapping Album 4 Song 10
url_2013_10 <-paste0("https://songmeanings.com/songs/view/3530822107859454047/")
s2013_10 <- read_html(url_2013_10)
paragraphs43 = html_node(s2013_10, '.lyric-box')
pText43 = html_text(paragraphs43)
lyrics_text42 <- paste(pText43)
print(head(pText43))
clean_2013_10 <- gsub("\\\t|\\\n", '', lyrics_text42, perl = TRUE)
clean_2013_101 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_10)
clean_2013_102 <- str_replace_all(clean_2007_51, "\\?", "? ")
clean_2013_103 <- str_replace_all(clean_2007_52, "\\)", ") ")
Anklebiters <- gsub(remove_phrase, "", clean_2013_103)
print(Anklebiters)

#Scrapping Album 4 Song 11
url_2013_11 <-paste0("https://songmeanings.com/songs/view/3530822107859454048/")
s2013_11 <- read_html(url_2013_11)
paragraphs44 = html_node(s2013_11, '.lyric-box')
pText44 = html_text(paragraphs44)
lyrics_text43 <- paste(pText44)
print(head(pText44))
clean_2013_11 <- gsub("\\\t|\\\n", '', lyrics_text43, perl = TRUE)
clean_2013_112 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_11)
clean_2013_113 <- gsub("IRead", "I read", clean_2013_112)
Interlude_Holiday <- gsub(remove_phrase, "", clean_2013_113)
print(Interlude_Holiday)

#Scrapping Album 4 Song 12
url_2013_12 <-paste0("https://songmeanings.com/songs/view/3530822107859454049/")
s2013_12 <- read_html(url_2013_12)
paragraphs45 = html_node(s2013_12, '.lyric-box')
pText45 = html_text(paragraphs45)
lyrics_text44 <- paste(pText45)
print(head(pText45))
clean_2013_12 <- gsub("\\\t|\\\n|\\\r", '', lyrics_text44, perl = TRUE)
clean_2013_121 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_12)
clean_2013_122 <- str_replace_all(clean_2013_121, "\\?", "? ")
clean_2013_123 <- gsub("arms'Cause", "arms Cause", clean_2013_122)
Proof <- gsub(remove_phrase, "", clean_2013_123)
print(Proof)

#Scrapping Album 4 Song 13
url_2013_13 <-paste0("https://songmeanings.com/songs/view/3530822107859454050/")
s2013_13 <- read_html(url_2013_13)
paragraphs46 = html_node(s2013_13, '.lyric-box')
pText46 = html_text(paragraphs46)
lyrics_text45 <- paste(pText46)
print(head(pText46))
clean_2013_13 <-  gsub("\\\t|\\\n|\\\r", '', lyrics_text45, perl = TRUE)
clean_2013_131 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_13)
clean_2013_132 <- str_replace_all(clean_2013_131, "\\?", "? ")
clean_2013_133 <- gsub("blind,Expecting", "blind, Expecting", clean_2013_132)
Hate_to_See_Your_Heart_Break <- gsub(remove_phrase, "", clean_2013_133)
print(Hate_to_See_Your_Heart_Break)

#Scrapping Album 4 Song 14
url_2013_14 <-paste0("https://songmeanings.com/songs/view/3530822107859454051/")
s2013_14 <- read_html(url_2013_14)
paragraphs47 = html_node(s2013_14, '.lyric-box')
pText47 = html_text(paragraphs47)
lyrics_text46 <- paste(pText47)
print(head(pText47))
clean_2013_14 <- gsub("\\\t|\\\n|\\\r", '', lyrics_text46, perl = TRUE)
clean_2013_141 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_14)
clean_2013_142 <- str_replace_all(clean_2013_141, "\\?", "? ")
clean_2013_143 <- gsub("Ooh-ooh-ooh-ooh-ooh", "Ooh ooh ooh ooh ooh", clean_2013_142)
One_of_those_Crazy_Girls <- gsub(remove_phrase, "", clean_2013_143)
print(One_of_those_Crazy_Girls)

#Scrapping Album 4 Song 15
url_2013_15 <-paste0("https://songmeanings.com/songs/view/3530822107859454052/")
s2013_15 <- read_html(url_2013_15)
paragraphs48 = html_node(s2013_15, '.lyric-box')
pText48 = html_text(paragraphs48)
lyrics_text47 <- paste(pText48)
print(head(pText48))
clean_2013_15 <- gsub("\\\t|\\\n|\\\r", '', lyrics_text47, perl = TRUE)
clean_2013_151 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_15)
Interlude_Im_not_Angry_Anymore <- gsub(remove_phrase, "", clean_2013_151)
print(Interlude_Im_not_Angry_Anymore)

#Scrapping Album 4 Song 16
url_2013_16 <-paste0("https://songmeanings.com/songs/view/3530822107859454053/")
s2013_16 <- read_html(url_2013_16)
paragraphs49 = html_node(s2013_16, '.lyric-box')
pText49 = html_text(paragraphs49)
lyrics_text48 <- paste(pText49)
print(head(pText49))
clean_2013_16 <- gsub("\\\t|\\\n|\\\r", '', lyrics_text48, perl = TRUE)
clean_2013_161 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_16)
clean_2013_162 <- str_replace_all(clean_2013_161, "\\?", "? ")
Be_Alone <- gsub(remove_phrase, "", clean_2013_162)
print(Be_Alone)

#Scrapping Album 4 Song 17
url_2013_17 <-paste0("https://songmeanings.com/songs/view/3530822107859454054/")
s2013_17 <- read_html(url_2013_17)
paragraphs50 = html_node(s2013_17, '.lyric-box')
pText50 = html_text(paragraphs50)
lyrics_text49 <- paste(pText50)
print(head(pText50))
clean_2013_17 <- gsub("\\\t|\\\n|\\\r", '', lyrics_text49, perl = TRUE)
clean_2013_171 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2013_17)
clean_2013_172 <- str_replace_all(clean_2013_171, "\\)", ") ")
Future <- sub(remove_phrase, "", clean_2013_172)
print(Future)

#Scrapping Album 5 Song 1
url_2017_1 <-paste0("https://songmeanings.com/songs/view/3530822107859545200/")
s2017_1 <- read_html(url_2017_1)
paragraphs51 = html_node(s2017_1, '.lyric-box')
pText51 = html_text(paragraphs51)
lyrics_text50 <- paste(pText51)
print(head(pText51))
clean_2017_1 <- gsub("\\\t|\\\n", '', lyrics_text50, perl = TRUE)
clean_2017_11 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2017_1)
clean_2017_12 <- str_replace_all(clean_2017_11, "\\)", ") ")
clean_2017_13 <- str_replace_all(clean_2017_12, "\\(", " (")
Hard_Times <- sub(remove_phrase, "", clean_2017_13)
print(Hard_Times)

#Scrapping Album 5 Song 2
url_2017_2 <-paste0("https://songmeanings.com/songs/view/3530822107859546706/")
s2017_2 <- read_html(url_2017_2)
paragraphs52 = html_node(s2017_2, '.lyric-box')
pText52 = html_text(paragraphs52)
lyrics_text51 <- paste(pText52)
print(head(pText52))
clean_2017_2 <- gsub("\\\t|\\\n", '', lyrics_text51, perl = TRUE)
clean_2017_21 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2017_2)
clean_2017_22 <- str_replace_all(clean_2017_21, "[[:punct:]]", " ")
Rose_Colored_Boy <- gsub(remove_phrase, "", clean_2017_22)
print(Rose_Colored_Boy)

#Scrapping Album 5 Song 3
url_2017_3 <-paste0("https://songmeanings.com/songs/view/3530822107859546707/")
s2017_3 <- read_html(url_2017_3)
paragraphs53 = html_node(s2017_3, '.lyric-box')
pText53 = html_text(paragraphs53)
lyrics_text52 <- paste(pText53)
print(head(pText53))
clean_2017_3 <- gsub("\\\t|\\\n", '', lyrics_text52, perl = TRUE)
clean_2017_31 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2017_3)
clean_2017_32 <- str_replace_all(clean_2017_31, "\\?", "? ")
clean_2017_33 <- str_replace_all(clean_2017_32, "\\)", ") ")
clean_2017_34 <- str_replace_all(clean_2017_33, "\\(", " (")
Told_You_So <- gsub(remove_phrase, "", clean_2017_34)
print(Told_You_So)

#Scrapping Album 5 Song 4
url_2017_4 <-paste0("https://songmeanings.com/songs/view/3530822107859546708/")
s2017_4 <- read_html(url_2017_4)
paragraphs54 = html_node(s2017_4, '.lyric-box')
pText54 = html_text(paragraphs54)
lyrics_text53 <- paste(pText54)
print(head(pText54))
clean_2017_4 <-  gsub("\\\t|\\\n", '', lyrics_text53, perl = TRUE)
clean_2017_41 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2017_4)
clean_2017_42 <- str_replace_all(clean_2017_32, "\\)", ") ")
clean_2017_43 <- str_replace_all(clean_2017_33, "\\(", " (")
Forgiveness <- gsub(remove_phrase, "", clean_2017_43)
print(Forgiveness)

#Scrapping Album 5 Song 5
url_2017_5 <-paste0("https://songmeanings.com/songs/view/3530822107859546709/")
s2017_5 <- read_html(url_2017_5)
paragraphs55 = html_node(s2017_5, '.lyric-box')
pText55 = html_text(paragraphs55)
lyrics_text54 <- paste(pText55)
print(head(pText55))
clean_2017_5 <- gsub("\\\t|\\\n", '', lyrics_text54, perl = TRUE)
clean_2017_51 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2017_5)
clean_2017_52 <- str_replace_all(clean_2017_51, "\\?", "? ")
Fake_Happy <- gsub(remove_phrase, "", clean_2017_52)
print(Fake_Happy)

#Scrapping Album 5 Song 6
url_2017_6 <-paste0("https://songmeanings.com/songs/view/3530822107859546710/")
s2017_6 <- read_html(url_2017_6)
paragraphs56 = html_node(s2017_6, '.lyric-box')
pText56 = html_text(paragraphs56)
lyrics_text55 <- paste(pText56)
print(head(pText56))
clean_2017_6 <- gsub("\\\t|\\\n", '', lyrics_text55, perl = TRUE)
clean_2017_61 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2017_6)
clean_2017_62 <- str_replace_all(clean_2017_61, "\\?", "? ")
Twenty_Six <- gsub(remove_phrase, "", clean_2017_62)
print(Twenty_Six)

#Scrapping Album 5 Song 7
url_2017_7 <-paste0("https://songmeanings.com/songs/view/3530822107859546711/")
s2017_7 <- read_html(url_2017_7)
paragraphs57 = html_node(s2017_7, '.lyric-box')
pText57 = html_text(paragraphs57)
lyrics_text56 <- paste(pText57)
print(head(pText57))
clean_2017_7 <- gsub("\\\t|\\\n", '', lyrics_text55, perl = TRUE)
clean_2017_71 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2017_7)
clean_2017_72 <- str_replace_all(clean_2017_71, "\\?", "? ")
Pool <- gsub(remove_phrase, "", clean_2017_72)
print(Pool)

#Scrapping Album 5 Song 8
url_2017_8 <-paste0("https://songmeanings.com/songs/view/3530822107859546712/")
s2017_8 <- read_html(url_2017_8)
paragraphs58 = html_node(s2017_8, '.lyric-box')
pText58 = html_text(paragraphs58)
lyrics_text57 <- paste(pText58)
print(head(pText58))
clean_2017_8 <- gsub("\\\t|\\\n", '', lyrics_text57, perl = TRUE)
clean_2017_81 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2017_8)
clean_2017_82 <- str_replace_all(clean_2017_81, "\\?", "? ")
clean_2017_83 <-  gsub("'", "", clean_2017_82)
Grudges <- gsub(remove_phrase, "", clean_2017_83)
print(Grudges)

#Scrapping Album 5 Song 9
url_2017_9 <-paste0("https://songmeanings.com/songs/view/3530822107859546713/")
s2017_9 <- read_html(url_2017_9)
paragraphs59 = html_node(s2017_9, '.lyric-box')
pText59 = html_text(paragraphs59)
lyrics_text58 <- paste(pText59)
print(head(pText59))
clean_2017_9 <- gsub("\\\t|\\\n", '', lyrics_text58, perl = TRUE)
clean_2017_91 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2017_9)
Caught_in_the_Middle <- gsub(remove_phrase, "", clean_2017_91)
print(Caught_in_the_Middle)

#Scrapping Album 5 Song 10
url_2017_10 <-paste0("https://songmeanings.com/songs/view/3530822107859546714/")
s2017_10 <- read_html(url_2017_10)
paragraphs60 = html_node(s2017_10, '.lyric-box')
pText60 = html_text(paragraphs60)
lyrics_text59 <- paste(pText60)
print(head(pText60))
clean_2017_10 <- gsub("\\\t|\\\n", '', lyrics_text59, perl = TRUE)
clean_2017_101 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2017_10)
clean_2017_102 <- str_replace_all(clean_2017_101, "\\?", "? ")
Idle_Worship <- gsub(remove_phrase, "", clean_2017_102)
print(Idle_Worship)

#Scrapping Album 5 Song 11
url_2017_11 <-paste0("https://songmeanings.com/songs/view/3530822107859546715/")
s2017_11 <- read_html(url_2017_11)
paragraphs61 = html_node(s2017_11, '.lyric-box')
pText61 = html_text(paragraphs61)
lyrics_text60 <- paste(pText61)
print(head(pText61))
clean_2017_11 <- gsub("\\\t|\\\n", '', lyrics_text60, perl = TRUE)
clean_2017_111 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2017_11)
clean_2017_112 <- gsub("clear:You", "clear You", clean_2017_111)
No_Friend <- gsub(remove_phrase, "", clean_2017_112)
print(No_Friend)

#Scrapping Album 5 Song 12
url_2017_12 <-paste0("https://songmeanings.com/songs/view/3530822107859546716/")
s2017_12 <- read_html(url_2017_12)
paragraphs62 = html_node(s2017_12, '.lyric-box')
pText62 = html_text(paragraphs62)
lyrics_text61 <- paste(pText62)
print(head(pText62))
clean_2017_12 <- gsub("\\\t|\\\n", '', lyrics_text61, perl = TRUE)
clean_2017_121 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2017_12)
clean_2017_122 <- str_replace_all(clean_2017_121, "\\?", "? ")
clean_2017_123 <- gsub("'", "", clean_2017_122)
Tell_Me_How <- gsub(remove_phrase, "", clean_2017_123)
print(Tell_Me_How)

#Scrapping Album 6 Song 1
url_2023_1 <-paste0("https://genius.com/Paramore-this-is-why-lyrics")
s2023_1 <- read_html(url_2023_1)
paragraphs63 = html_node(s2023_1, '.kUgSbL')
pText63 = html_text(paragraphs63)
lyrics_text62 <- paste(pText63)
print(head(pText63))
remove_phrase1 <- c("\\[Verse 1\\]", "\\[Chorus\\]", "\\[Verse 2\\]", "\\[Pre-Chorus\\]")
clean_2023_1 <- lyrics_text62
for (phrase in remove_phrase1) {
  clean_2023_1 <- gsub(phrase, "", clean_2023_1)
}
clean_2023_11 <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2023_1)
This_is_Why <- str_replace_all(clean_2023_11, "\\)", ") ")
print(This_is_Why)

#Scrapping Album 6 Song 2
url_2023_2 <-paste0("https://genius.com/Paramore-the-news-lyrics")
s2023_2 <- read_html(url_2023_2)
paragraphs64 = html_node(s2023_2, '.kUgSbL')
pText64 = html_text(paragraphs64)
lyrics_text63 <- paste(pText64)
print(head(pText64))
clean_2023_2 <- lyrics_text63
for (phrase in remove_phrase1) {
  clean_2023_2 <- gsub(phrase, "", clean_2023_2)
}
The_News <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2023_2)
print(The_News)

#Scrapping Album 6 Song 3
url_2023_3 <-paste0("https://genius.com/Paramore-running-out-of-time-lyrics")
s2023_3 <- read_html(url_2023_3)
paragraphs65 = html_node(s2023_3, '.kUgSbL')
pText65 = html_text(paragraphs65)
lyrics_text64 <- paste(pText65)
print(head(pText65))
clean_2023_3 <- lyrics_text64
for (phrase in remove_phrase1) {
  clean_2023_3 <- gsub(phrase, "", clean_2023_3)
}
clean_2023_31 <- str_replace_all(clean_2023_3, "\\)", ") ")
Running_Out_of_Time <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2023_31)
print(Running_Out_of_Time)

#Scrapping Album 6 Song 4
url_2023_4 <-paste0("https://genius.com/Paramore-cest-comme-ca-lyrics")
s2023_4 <- read_html(url_2023_4)
paragraphs66 = html_node(s2023_4, '.kUgSbL')
pText66 = html_text(paragraphs66)
lyrics_text65 <- paste(pText66)
print(head(pText66))
clean_2023_4 <- lyrics_text65
for (phrase in remove_phrase1) {
  clean_2023_4 <- gsub(phrase, "", clean_2023_4)
}
Cest_Comme_ca <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2023_4)
print(Cest_Comme_ca)

#Scrapping Album 6 Song 5
url_2023_5 <-paste0("https://genius.com/Paramore-big-man-little-dignity-lyrics")
s2023_5 <- read_html(url_2023_5)
paragraphs67 = html_node(s2023_5, '.kUgSbL')
pText67 = html_text(paragraphs67)
lyrics_text66 <- paste(pText67)
print(head(pText67))
clean_2023_5 <- lyrics_text67
for (phrase in remove_phrase1) {
  clean_2023_5 <- gsub(phrase, "", clean_2023_5)
}
clean_2023_51 <- str_replace_all(clean_2023_5, "\\?", "? ")
Big_Man_Little_Dignity <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2023_51)
print(Big_Man_Little_Dignity)

#Scrapping Album 6 Song 6
url_2023_6 <-paste0("https://genius.com/Paramore-you-first-lyrics")
s2023_6 <- read_html(url_2023_6)
paragraphs68 = html_node(s2023_6, '.kUgSbL')
pText68 = html_text(paragraphs68)
lyrics_text67 <- paste(pText68)
print(head(pText68))
clean_2023_6 <- lyrics_text67
for (phrase in remove_phrase1) {
  clean_2023_6 <- gsub(phrase, "", clean_2023_6)
}
clean_2023_61 <- str_replace_all(clean_2023_6, "\\?", "? ")
You_First <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2023_61)
print(You_First)

#Scrapping Album 6 Song 7
url_2023_7 <-paste0("https://genius.com/Paramore-figure-8-lyrics")
s2023_7 <- read_html(url_2023_7)
paragraphs69 = html_node(s2023_7, '.kUgSbL')
pText69 = html_text(paragraphs69)
lyrics_text68 <- paste(pText69)
print(head(pText69))
clean_2023_7 <- lyrics_text68
for (phrase in remove_phrase1) {
  clean_2023_7 <- gsub(phrase, "", clean_2023_7)
}
clean_2023_71 <- str_replace_all(clean_2023_7, "\\)", ") ")
clean_2023_72 <- str_replace_all(clean_2023_71, "\\(", " (")
clean_2023_73 <- gsub("'", "", clean_2023_72)
Figure_Eight <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2023_73)
print(Figure_Eight)

#Scrapping Album 6 Song 8
url_2023_8 <-paste0("https://genius.com/Paramore-crave-lyrics")
s2023_8 <- read_html(url_2023_8)
paragraphs70 = html_node(s2023_8, '.kUgSbL')
pText70 = html_text(paragraphs70)
lyrics_text69 <- paste(pText70)
print(head(pText70))
clean_2023_8 <- lyrics_text69
for (phrase in remove_phrase1) {
  clean_2023_8 <- gsub(phrase, "", clean_2023_8)
}
clean_2023_81 <- gsub("IJust", "I just", clean_2023_8)
clean_2023_82 <- gsub("say, \"Live in the present\"I'm already", "say, Live in the present I'm already", clean_2023_81)
Crave <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2023_82)
print(Crave)

#Scrapping Album 6 Song 9
url_2023_9 <-paste0("https://genius.com/Paramore-liar-lyrics")
s2023_9 <- read_html(url_2023_9)
paragraphs71 = html_node(s2023_9, '.kUgSbL')
pText71 = html_text(paragraphs71)
lyrics_text70 <- paste(pText71)
print(head(pText71))
clean_2023_9 <- lyrics_text70
for (phrase in remove_phrase1) {
  clean_2023_9 <- gsub(phrase, "", clean_2023_9)
}
Liar <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2023_9)
print(Liar)

#Scrapping Album 6 Song 10
url_2023_10 <-paste0("https://genius.com/Paramore-thick-skull-lyrics")
s2023_10 <- read_html(url_2023_10)
paragraphs72 = html_node(s2023_10, '.kUgSbL')
pText72 = html_text(paragraphs72)
lyrics_text71 <- paste(pText72)
print(head(pText72))
clean_2023_10 <- lyrics_text71
for (phrase in remove_phrase1) {
  clean_2023_10 <- gsub(phrase, "", clean_2023_10)
}
clean_2023_101 <- gsub("\\[Intro\\]", "", clean_2023_10)
clean_2023_102 <- gsub("\\[Verse\\]", "", clean_2023_101)
Thick_Skull <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_2023_102)
print(Thick_Skull)

#Combining Albums into a Data Frame
Albums <- data.frame(
  Song = c("All_We_Know", "Pressure", "Emergency", "Brighter", "Here_We_Go_Again", "Never_Let_This_Go", 
                   "Whoa", "Conspiracy", "Franklin", "My_Heart","For_A_Pessimist_Im_Pretty_Optimistic", "Thats_What_You_Get", "Hallelujah", "Misery_Business", "When_It_Rains", "Let_The_Flames_Begin", "Miracle", "crushcrushcrush", "We_Are_Broken", "Fences", "Born_For_This", 
           "Careful", "Ignorance", "Playind_God", "Brick_by_Boring_Brick", "Turn_it_Off", "Only_Exception", "Feeling_Sorry", 
           "Looking_Up", "Where_the_Lines_Overlap", "Misguided_Ghosts", "All_I_Wanted", "Decode","Fast_in_my_Car", "Now", "Grow_Up", "Daydreaming", "Interlude_Moving_On", "Aint_it_Fun", "Part_II", 
            "Last_Hope", "Still_Into_You", "Anklebiters", "Interlude_Holiday", "Proof", "Hate_to_See_Your_Heart_Break", 
            "One_of_those_Crazy_Girls", "Interlude_Im_not_Angry_Anymore", "Be_Alone", "Future","Hard_Times", "Rose_Colored_Boy", "Told_You_So", "Forgiveness", "Fake_Happy", "Twenty_Six", "Pool", 
           "Grudges", "Caught_in_the_Middle", "Idle_Worship", "No_Friend", "Tell_Me_How","This_is_Why", "The_News", "Running_Out_of_Time", "Cest_Comme_ca", "Big_Man_Little_Dignity", "You_First", "Figure_Eight", "Liar", "Crave", "Thick_Skull"), 
  
  Album = c("All_We_Know_is_Falling","All_We_Know_is_Falling","All_We_Know_is_Falling","All_We_Know_is_Falling","All_We_Know_is_Falling","All_We_Know_is_Falling","All_We_Know_is_Falling","All_We_Know_is_Falling","All_We_Know_is_Falling","All_We_Know_is_Falling", "Riot", "Riot", "Riot", "Riot", "Riot", "Riot", "Riot", "Riot", "Riot", "Riot", "Riot",
            "Brand_New_Eyes", "Brand_New_Eyes", "Brand_New_Eyes", "Brand_New_Eyes", "Brand_New_Eyes", "Brand_New_Eyes", "Brand_New_Eyes", "Brand_New_Eyes", "Brand_New_Eyes", "Brand_New_Eyes", "Brand_New_Eyes", "Brand_New_Eyes",
            "Paramore", "Paramore", "Paramore", "Paramore", "Paramore", "Paramore", "Paramore", "Paramore", "Paramore", "Paramore", "Paramore", "Paramore", "Paramore", "Paramore", "Paramore", "Paramore", "Paramore", 
            "After_Laughter",  "After_Laughter",  "After_Laughter",  "After_Laughter",  "After_Laughter",  "After_Laughter",  "After_Laughter",  "After_Laughter",  "After_Laughter",  "After_Laughter",  "After_Laughter",  "After_Laughter",
            "This_is_Why", "This_is_Why", "This_is_Why", "This_is_Why", "This_is_Why", "This_is_Why", "This_is_Why", "This_is_Why", "This_is_Why", "This_is_Why"), 
  Lyrics = c(All_We_Know, Pressure, Emergency, Brighter, Here_We_Go_Again, Never_Let_This_Go, Whoa, Conspiracy, Franklin, My_Heart, For_A_Pessimist_Im_Pretty_Optimistic, Thats_What_You_Get, Hallelujah, Misery_Business, When_It_Rains, Let_The_Flames_Begin, Miracle, crushcrushcrush, We_Are_Broken, Fences, Born_For_This, 
             Careful, Ignorance, Playing_God, Brick_By_Boring_Brick, Turn_it_Off, Only_Exception, Feeling_Sorry, Looking_Up, Where_the_Lines_Overlap, 
             Misguided_Ghosts, All_I_Wanted, Decode, Fast_in_my_Car, Now, Grow_Up, Daydreaming, Interlude_Moving_On, Aint_it_Fun, Part_II, 
             Last_Hope, Still_Into_You, Anklebiters, Interlude_Holiday, Proof, Hate_to_See_Your_Heart_Break, One_of_those_Crazy_Girls, Interlude_Im_not_Angry_Anymore, 
             Be_Alone, Future, Hard_Times, Rose_Colored_Boy, Told_You_So, Forgiveness, Fake_Happy, Twenty_Six, Pool, Grudges, Caught_in_the_Middle, 
             Idle_Worship, No_Friend, Tell_Me_How, This_is_Why, The_News, Running_Out_of_Time, Cest_Comme_ca, Big_Man_Little_Dignity, You_First, Figure_Eight, Liar, Crave, Thick_Skull))

#Tokenize Lyrics
orginal_albums <- Albums %>%
  group_by(Album) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()

tidy_albums1 <- orginal_albums %>%
  unnest_tokens(word, Lyrics)
print(tidy_albums1)
#Remove stop words for analysis 
data(stop_words)
tidy_albums2 <- tidy_albums1 %>%
  anti_join(stop_words)

#Run Sentiment Analysis 
tidy_albums <- tidy_albums2 %>%
  inner_join(get_sentiments("bing"), by = "word")

#Wordcloud
tidy_albums %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 60))

#Piechart
sentiment_freq <- tidy_albums %>%
  count(sentiment)

pie_chart <- ggplot(sentiment_freq, aes(x = "", y = n, fill = sentiment)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Positive and Negative Words", fill = "Sentiment") +
  scale_fill_manual(values = c("positive" = "green", "negative" = "red")) +
  theme_void() +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))
print(pie_chart)

#Bargraph
album_sentiment_freq <- tidy_albums %>%
  count(Album, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(Album = factor(Album, levels = unique(Album)))
print(album_sentiment_freq)

Album_Sent_Freq <- data.frame(
  Album = c("After_Laughter", "All_We_Know_is_Falling", "Brand_New_Eyes", "Paramore", "Riot", "This_is_Why"),
  Total_Positive = c(71, 45, 81, 91, 78, 14), 
  Total_Negative = c(158, 62, 100, 148, 80, 46))
view(word_freq_totals1)

Album_Sent_Freq_Long <- tidyr::pivot_longer(Album_Sent_Freq, cols = c(Total_Positive, Total_Negative), names_to = "Sentiment", values_to = "Frequency")

Sentiment_Bar_Graph <- ggplot(Album_Sent_Freq_Long, aes(x = Album, y = Frequency, fill = Sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Positive and Negative Words by Album",
       x = "Album",
       y = "Word Frequency") +
  scale_fill_manual(values = c("Total_Positive" = "green", "Total_Negative" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
print(Sentiment_Bar_Graph)
