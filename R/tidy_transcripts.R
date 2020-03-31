###### Import .txt Debate files and create tidy dataframe ######
library(tidyverse)

#Two formats. Either the speaker is identified by "LASTNAME:" or by "first last:"
firstlast <- c("Dem_Debate_2020_WashingtonDC.txt","Dem_Debate_2020_NewHampshire.txt",
               "Dem_Debate_2020_Miami1.txt","Dem_Debate_2020_Miami2.txt",
               "Dem_Debate_2020_Iowa.txt", "Dem_Debate_2020_Houston.txt",
               "Dem_Debate_2020_Detroit1.txt","Dem_Debate_2020_Detroit2.txt")

files <- list.files("raw_data/", pattern = "\\.txt$")


transcripts <- list()
i <- 1
for(file in files){

    #read in a transcript
  transcript <- readLines(paste0("raw_data/",file))
    
    #Fix spacing
  transcript <- str_replace_all(transcript, "\\.\\.\\."," ")
  transcript <- str_replace_all(transcript, "\\.","\\. ")
  transcript <- str_replace_all(transcript, "\\?","\\? ")
  transcript <- str_replace_all(transcript, "\\!","\\! ")
  transcript <- str_replace_all(transcript, "\\-","\\- ")
  transcript <- str_replace_all(transcript, "\\]","\\] ")
  transcript <- str_replace_all(transcript, "\\:","\\: ")
  transcript <- str_replace_all(transcript, "\\,","\\, ")
  transcript <- str_replace_all(transcript, "O'MALLEY", "OMALLEY")
  transcript <- str_replace_all(transcript, "O'ROURKE", "OROURKE")
  transcript <- str_replace_all(transcript, "O'DONNELL", "ODONNELL")
  transcript <- str_replace_all(transcript, "\\(UNKNOWN\\)", "UNKNOWN")
  transcript <- str_replace_all(transcript, "OK", "ok")
  transcript <- str_replace_all(transcript, '"', '" ')
  transcript <- str_replace_all(transcript, '\\)', '\\) ')
  transcript <- str_replace_all(transcript, '\\(\\)', '')
  
  transcript <- str_replace_all(transcript, "\\.  ","\\. ")
  transcript <- str_replace_all(transcript, "\\?  ","\\? ")
  transcript <- str_replace_all(transcript, "\\!  ","\\! ")
  transcript <- str_replace_all(transcript, "\\-  ","\\- ")
  transcript <- str_replace_all(transcript, "\\]  ","\\] ")
  transcript <- str_replace_all(transcript, "\\:  ","\\: ")
  transcript <- str_replace_all(transcript, '"  ', '" ')
  transcript <- str_replace_all(transcript, "\\,  ","\\, ")
  transcript <- str_replace_all(transcript, "\\)  ","\\) ")
  transcript <- str_replace_all(transcript, "([0-9])(:)","\\1")
  transcript <- str_replace_all(transcript, "(:)([0-9])","\\2")
  transcript <- str_replace_all(transcript, "Kirsten G\\. \\:","Kirsten Gillibrand\\:")
  transcript <- str_replace_all(transcript, "de Blasio","deBlasio")
  
  
  if(file %in% firstlast){
    search_pattern <- "( (?=\\w+ \\w+:)|: )"
  }
  else{
    search_pattern <- "( (?=\\w+:)|: )"
    transcript <- str_replace_all(transcript, "([a-z0-9])([A-Z])([A-Z])([A-Z])", "\\1 \\2\\3\\4")
    transcript <- str_replace_all(transcript, "([a-z0-9])(:)", "\\1;")
  }
  
    #split interviewer from statements
  transcript <- strsplit(transcript, split=search_pattern,perl=TRUE)
  transcript <- data.frame(text=transcript[[1]])
    
    #Split the character vector into two vars - every other element in each column.
  transcript_clean <- transcript %>%
    mutate(ind = rep(c(1, 2),length.out = n())) %>%
    group_by(ind) %>%
    mutate(id = row_number()) %>%
    spread(ind, text) %>%
    select(-id) %>%
    mutate(type=substr(file, 1, 3)) %>% #now when you add general and vice presidential have that be firs three chars
    mutate(year=substr(file, 12, 15)) %>%
    mutate(location=substr(file, 17, (str_locate(file, pattern="\\.")[1] - 1)))
    
  transcripts[[i]] <- transcript_clean
  i <- i+1
  print(file)
  rm(transcript, transcript_clean)
}

candidates_2020 <- c("Joe Biden","Bernie Sanders", "Elizabeth Warren", "Amy Klobuchar", "Andrew Yang",
                "Kamala Harris", "Kirsten Gillibrand", "Tulsi Gabbard", "Michael Bloomberg", "Cory Booker",
                "Julian Castro", "Beto O'Rourke", "Jay Inslee", "Pete Buttigieg", "Marianne Williamson",
                "Tom Steyer", "John Hickenlooper", "Bill DeBlasio", "Eric Swalwell","Michael Bennett",
                "Tim Ryan", "Deval Patrick", "Steve Bullock","Joe Sestak", "Seth Moulton","Mike Gravel",
                "Donald Trump","Mike Pence")
candidates_2016 <- c("Hillary Clinton", "Tim Kaine", "Bernie Sanders","Lawrence Lessig","Jim Webb","Martin O'Malley",
                     "Lincoln Chafee","John Delaney","John Kasich","Ted Cruz","Marco Rubio","Ben Carson","Jeb Bush","Jim Gilmore","Carly Fiorina","Chris Christie",
                "Rand Paul","Rick Santorum","Mike Huckabee","George Pataki","Lindset Graham","Bobby Jindal",
                "Scott Walker","Rick Perry","Donald Trump", "Mike Pence")
candidates_2012 <- c("Mitt Romney", "Paul Ryan", "Ron Paul","Fred Karger","Newt Gingrich","Rick Santorum","Buddy Roemer",
                     "Rick Perry","Jon Huntsman","Michele Bachmann","Gary Johnson","Herman Cain",
                     "Thaddeus McCotter","Tim Pawlenty","Barack Obama","Joe Biden")


#ideally, other fields would include: date
transcripts.df <- bind_rows(transcripts, .id = "`1`") %>%
  rename(speaker="1", text="2") %>%
  select(speaker:location) %>%
  mutate(speaker=recode(speaker, "SANDERS"="Bernie Sanders", "CLINTON"="Hillary Clinton",
                        "OMALLEY"="Martin O'Malley", "HOLT"="Lester Holt", "CHAFEE"="Lincoln Chafee",
                        "BASH"="Dana Bash","UNKNOWN"="Unknown","QUESTION"="Question",
                        "MUIR"="David Muir","MADDOW"="Rachel Maddow","WARREN"="Elizabeth Warren",
                        "KLOBUCHAR"="Amy Klobuchar","BUTTIGIEG"="Pete Buttigieg","BIDEN"="Joe Biden",
                        "HARRIS"="Kamala Harris","BOokER"="Cory Booker","GABBARD"="Tulsi Gabbard",
                        "STEYER"="Tom Steyer","YANG"="Andrew Yang","CASTRO"="Julian Castro",
                        "OROURKE"="Beto O'Rourke","crowd crowd"="crowd","George S"="George Stephanopoulos",
                        "Voiceover Guy"="Voiceover","BLOOMBERG"="Michael Bloomberg","TODD"="Chuck Todd",
                        "Senator Warren"="Elizabeth Warren","Amy Klobachar"="Amy Klobuchar",
                        "Mayor DeBlasio"="Bill DeBlasio", "Eric Stalwell"="Eric Swalwell","DEMINT"="Jim Demint",
                        "CAIN"="Herman Cain", "GINGRICH"="Newt Gingrich", "UNIDENTIFIED"="Unidentified",
                        "SAWYER"="Dianne Sawyer", "ROMNEY"="Mitt Romney", "STEPHANOPOULOS"="George Stephanopoulos",
                        "PERRY"="Rick Perry", "SANTORUM"="Rick Santorum", "VOICE"="Voiceover", "BAIER"="Bret Baier",
                        "HUNTSMAN"="Jon Huntsman","CHRISTIE"="Chris Christie","KASICH"="John Kasich",
                        "CRUZ"="Ted Cruz", "RUBIO"="Marco Rubio","BUSH"="Jeb Bush","CARSON"="Ben Carson",
                        "BLITZER"="Wolf Blitzer","FIORINA"="Carly Fiorina","TRUMP"="Donald Trump",
                        "HUCKABEE"="Mike Huckabee", "WALKER"="Scott Walker", "ANNOUNCER"="Voiceover",
                        "FRANTA"="Connor Franta", "RAMSEY"="Franchesca Ramsey","BROWNLEE"="Marques Brownlee",
                        "COOPER"="Anderson Cooper","WEBB"="Jim Webb","LEMON"="Don Lemon",
                        "Abby Phillips"="Abby Phillip", "John H"="John Hickenlooper", "MITCHELL"="Andrea Mitchell",
                        "MCELVEEN"="Josh McElveen","TAPPER"="Jake Tapper","STUDENT"="Question","MALE"="Question",
                        "NEVADA"="Question","MODERATOR"="Moderator","NARRATOR"="Narrator","PROTESTER"="Protester",
                        "BREAM"="Shannon Bream","STRAWN"="Matt Strawn","HEWITT"="Hugh Hewitt",
                        "EPPERSON"="Sharon Epperson","SANTELLI"="Rick Santelli","OBERHELMAN"="Doug Oberhelman",
                        "LIESMAN"="Steve Liesman","CRAMER"="Jim Cramer","HARWOOD"="John Harwood",
                        "BARTIROMO"="Maria Bartiromo", "RADDATZ"="Martha Raddatz","LEVESQUE"="Neil Levesque",
                        "WELKER"="Kristen Welker","PARKER"="Ashley Parker","BURNETT"="Erin Burnett",
                        "LACEY"="Marc Lacey","NAWAZ"="Amna Nawaz","ALCINDOR"="Yamiche Alcindor",
                        "HAUC"="Vanessa Hauc", "RALSTON"="Jon Ralston","JACKSON"="Hallie Jackson",
                        "PROTESTORS"="Protestors","WOODRUFF"="Judy Woodruff", "WOODDRUFF"="Judy Woodruff",
                        "ALBERTA"="Tim Alberta","CANDIDATE"="Candidate", "WHITAKER"="Bill Whitaker",
                        "SCOTT"="Rick Scott", "LOPEZ"="Juan Carlos Lopez", "ABC Announcer"="Announcer",
                        "JKING"="John King","KING"="Steve King", "ODONNELL"="Norah O'Donnell",
                        "GARRETT"="Major Garrett","BRENNAN"="Margaret Brennan", "GERGEN"="David Gergen",
                        "ERICKSON"="Erick Erickson","BACHMANN"="Michelle Bachmann","GEORGE"="George Stephanopoulos",
                        "PAUL"="Ron Paul","RESIDENT"="Resident", "SEIB"="Gerald Seib","EVANS"="Kelly Evans",
                        "WILLIAMS"="Brian Williams","EBANS"="Kelly Evans", "KELLY"="Kate Kelly",
                        "WALLACE"="Chris Wallace","JOHNSON"="Gary Johnson", "WILKINS"="Sterling Wilkins",
                        "Unknown Male"="Unknown", "Unknown Female"="Unknown", "N Henderson"="Nia-Malika Henderson",
                        "crowd"="Crowd", "Moderator Guy"="Moderator","B Pfannenstiel"="Brianne Pfannenstiel",
                        "Brianne P"="Brianne Pfannenstiel", "Abby P"="Abby Phillip",
                        "IGeorge S"="George Stephanopoulos", "Savannah G"="Savannah Guthrie", "Savanagh G"="Savannah Guthrie",
                        "Beto ORourke"="Beto O'Rourke")) %>%
  mutate(speaker=ifelse(speaker=="Ron Paul" & year == 2016, "Rand Paul", speaker)) %>%
  mutate(candidate = ifelse(speaker %in% candidates_2020 & year == 2020, 1, 
                            ifelse(speaker %in% candidates_2016 & year == 2016, 1, 
                                   ifelse(speaker %in% candidates_2012 & year == 2012, 1, 0)))) %>%
  mutate(date=NA) %>%
  mutate(date=ifelse(year==2020 & location == "Miami1", "2019-06-26", date)) %>%
  mutate(date=ifelse(year==2020 & location == "Miami2", "2019-06-27", date)) %>%
  mutate(date=ifelse(year==2020 & location == "Detroit1", "2019-07-30", date)) %>%
  mutate(date=ifelse(year==2020 & location == "Detroit2", "2019-07-31", date)) %>%
  mutate(date=ifelse(year==2020 & location == "Houston", "2019-09-12", date)) %>%
  mutate(date=ifelse(year==2020 & location == "Columbus", "2019-10-15", date)) %>%
  mutate(date=ifelse(year==2020 & location == "Atlanta", "2019-11-20", date)) %>%
  mutate(date=ifelse(year==2020 & location == "LosAngeles", "2019-12-19", date)) %>%
  mutate(date=ifelse(year==2020 & location == "LasVegas", "2019-02-19", date)) %>%
  mutate(date=ifelse(year==2020 & location == "WashingtonDC", "2019-03-15", date)) %>%
  mutate(date=ifelse(year==2020 & location == "SouthCarolina", "2019-02-25", date)) %>%
  mutate(date=ifelse(year==2020 & location == "NewHampshire", "2019-02-07", date)) %>%
  mutate(date=ifelse(year==2020 & location == "Iowa", "2019-01-14", date)) %>%
  mutate(date=ifelse(year==2016 & type=="Rep" & location == "DesMoines", "2016-01-28", date)) %>%
  mutate(date=ifelse(year==2016 & type=="Rep" & location == "LasVegas", "2016-12-15", date)) %>%
  mutate(date=ifelse(year==2016 & type=="Rep" & location == "SimiValley", "2015-09-16", date)) %>%
  mutate(date=ifelse(year==2016 & type=="Dem" & location == "Charleston", "2016-01-17", date)) %>%
  mutate(date=ifelse(year==2016 & type=="Dem" & location == "LasVegas", "2015-10-13", date)) %>%
  mutate(date=ifelse(year==2016 & type=="Dem" & location == "Manchester", "2016-01-28", date)) %>%
  mutate(date=ifelse(year==2012 & type=="Rep" & location == "Columbia", "2011-05-05", date)) %>%
  mutate(date=ifelse(year==2012 & type=="Rep" & location == "DesMoines", "2011-12-10", date)) %>%
  mutate(date=ifelse(year==2012 & type=="Rep" & location == "LasVegas", "2011-10-18", date)) %>%
  mutate(date=ifelse(year==2012 & type=="Rep" & location == "MyrtleBeach", "2012-01-16", date)) %>%
  mutate(date=ifelse(year==2012 & type=="Rep" & location == "NH", "2012-01-08", date)) %>%
  mutate(date=ifelse(year==2012 & type=="Rep" & location == "Orlando", "2011-09-22", date)) %>%
  mutate(date=ifelse(year==2012 & type=="Rep" & location == "Rochester", "2011-11-09", date)) %>%
  mutate(date=ifelse(year==2012 & type=="Rep" & location == "SiouxCity", "2011-12-15", date))
  


#TODO
#add general and VP debates
#go further back


#TOTAL DEBATES
#2020: 13 dem debates
#2016: 9 dem debates, 13 dem forums, 12 rep debates, 7 rep forums
#2012: 20 rep debates and some forums

#kennedy-nixon debated in 1960 (4 debates, first is that infamous one)
##debates started in earnest in 1978

#https://www.debates.org/voter-education/debate-transcripts/ 
#they have pretty much all presidential and vice presidential debate transcripts

save(transcripts.df, file="data/transcripts.RData")


#### Check to make sure splitting worked correctly.
#### This should just return the names of each participant
for(i in 1:length(transcripts)){
  test <- transcripts[[i]]
  print(unique(test$`1`))
}


