######## Norms Analysis of Debate Transcripts ########

library(tidyverse)
library(martherus)
library(tidylog)
library(tidytext)

load("data/transcripts.RData")

norms_dictionary <- read_csv("data/norms_dictionary.csv")

pattern_carepos  <- paste0(paste0(norms_dictionary$care_pos, collapse="|"))
pattern_careneg  <- paste0(paste0(norms_dictionary$care_neg, collapse="|"))
pattern_caregroup  <- paste0(paste0(norms_dictionary$care_group, collapse="|"))
pattern_god  <- paste0(paste0(norms_dictionary$god, collapse="|"))
pattern_ingrouppos <- paste0(paste0(norms_dictionary$ingroup_pos, collapse="|"))
pattern_ingroupneg  <- paste0(paste0(norms_dictionary$ingroup_neg, collapse="|"))
pattern_authority <- paste0(paste0(norms_dictionary$authority, collapse="|"))
pattern_country  <- paste0(paste0(norms_dictionary$country, collapse="|"))
pattern_conservatism  <- paste0(paste0(norms_dictionary$conservatism, collapse="|"))

debate_norm <- transcripts.df %>%
  mutate(care_pos=str_detect(pattern=pattern_carepos, string=text),
         care_neg=str_detect(pattern=pattern_careneg, string=text),
         care_group=str_detect(pattern=pattern_caregroup, string=text),
         god=str_detect(pattern=pattern_god, string=text),
         ingroup_pos=str_detect(pattern=pattern_ingrouppos, string=text),
         ingroup_neg=str_detect(pattern=pattern_ingroupneg, string=text),
         authority=str_detect(pattern=pattern_authority, string=text),
         country=str_detect(pattern=pattern_country, string=text),
         conservatism=str_detect(pattern=pattern_conservatism, string=text))



######## Proportion of statements that include normative words by party and by party-year
counts <- debate_norm %>%
  group_by(type, year) %>%
  summarize(care=(sum(care_pos, na.rm=T)/n()),
            care_group=(sum(care_group, na.rm=T)/n()),
            god=(sum(god, na.rm=T)/n()),
            country=(sum(country, na.rm=T)/n()),
            conservatism=(sum(conservatism, na.rm=T)/n()),
            loyalty=(sum(authority, na.rm=T)/n())) %>%
  pivot_longer(cols=care:loyalty, names_to="norm") %>%
  filter(!is.na(type))

yearly_counts <- debate_norm %>%
  group_by(type, year) %>%
  summarize(care=(sum(care_pos, na.rm=T)/n()),
            care_group=(sum(care_group, na.rm=T)/n()),
            god=(sum(god, na.rm=T)/n()),
            country=(sum(country, na.rm=T)/n()),
            conservatism=(sum(conservatism, na.rm=T)/n()),
            loyalty=(sum(authority, na.rm=T)/n())) %>%
  pivot_longer(cols=care:loyalty, names_to="norm") %>%
  filter(!is.na(type))

yearly_counts$norm <- as.factor(yearly_counts$norm)
levels(yearly_counts$norm) <- c("Care","Group Care","Conservatism","Country","God","Loyalty")


pdf("figures/debate_props.pdf", width=11, height=8.5)
ggplot(counts, aes(x=norm, y=value, fill=type)) +
  geom_col() +
  facet_grid(rows=vars(counts$type)) +
  theme_james() +
  scale_fill_manual(values=c("darkblue", "darkred")) +
  xlab("Norm") +
  ylab("Proportion") +
  theme(text=element_text(size=19)) +
  scale_x_discrete(labels=addline_format(c("Care","Group-Specific Care","Conservatism","Country","God","Loyalty")))
dev.off()


pdf("figures/debate_yearly_props.pdf", width=11, height=8.5)
ggplot(yearly_counts, aes(x=year, y=value, color=type, group=type)) +
  geom_line() +
  facet_grid(rows=vars(yearly_counts$norm)) +
  #theme_james() +
  xlab("Year") +
  ylab("Proportion") +
  theme(text=element_text(size=14)) +
  scale_color_manual(values=c("darkblue", "darkred"))
dev.off()


################## Within Dems, who uses most care words?
candidates <- c("Joe Biden","Bernie Sanders", "Elizabeth Warren", "Amy Klobuchar", "Andrew Yang",
                "Kamala Harris", "Kirsten Gillibrand", "Tulsi Gabbard", "Michael Bloomberg", "Cory Booker",
                "Julian Castro", "Beto O'Rourke", "Jay Inslee", "Pete Buttigieg", "Marianne Williamson",
                "Tom Steyer", "John Hickenlooper", "Bill DeBlasio", "Eric Swalwell","Michael Bennett",
                "Tim Ryan")
top_candidates <- c("Joe Biden", "Bernie Sanders", "Elizabeth Warren",
               "Amy Klobuchar","Andrew Yang", "Pete Buttigieg", "Kamala Harris", "Cory Booker")

dem_care <- debate_norm %>%
  filter(type=="Dem" & year==2020 & speaker %in% top_candidates) %>%
  group_by(speaker) %>%
  summarize(care=sum(care_pos, care_group)/n())




############### Above we just say whether a statement has a care word or not. Let's try to count them
debate_norm_count <- transcripts.df %>%
  mutate(care_pos=str_count(pattern=pattern_carepos, string=text),
         care_neg=str_count(pattern=pattern_careneg, string=text),
         care_group=str_count(pattern=pattern_caregroup, string=text),
         god=str_count(pattern=pattern_god, string=text),
         ingroup_pos=str_count(pattern=pattern_ingrouppos, string=text),
         ingroup_neg=str_count(pattern=pattern_ingroupneg, string=text),
         authority=str_count(pattern=pattern_authority, string=text),
         country=str_count(pattern=pattern_country, string=text),
         conservatism=str_count(pattern=pattern_conservatism, string=text))

counts <- debate_norm_count %>%
  group_by(type, year) %>%
  summarize(care=(sum(care_pos, na.rm=T)),
            care_group=(sum(care_group, na.rm=T)),
            god=(sum(god, na.rm=T)),
            country=(sum(country, na.rm=T)),
            conservatism=(sum(conservatism, na.rm=T)),
            loyalty=(sum(authority, na.rm=T))) %>%
  pivot_longer(cols=care:loyalty, names_to="norm") %>%
  filter(!is.na(type))

dem_care <- debate_norm_count %>%
  filter(type=="Dem") %>%
  group_by(speaker, location, year) %>%
  summarize(care=sum(care_pos, care_group)/n())

ggplot(counts, aes(x=year, y=value, color=type, group=type)) +
  geom_line() +
  facet_grid(rows=vars(counts$norm)) +
  #theme_james() +
  xlab("Year") +
  ylab("Proportion") +
  theme(text=element_text(size=14)) +
  scale_color_manual(values=c("darkblue", "darkred"))





###### Need to find proportion of total words or something. So unnest tokens and then match to care dictionary
care_group <- norms_dictionary$care_group

props_norm_words <- transcripts.df %>%
  unnest_tokens(input=text, output=word, token="words") %>%
  anti_join(get_stopwords()) %>%
  mutate(care_group=str_detect(pattern=pattern_caregroup, string=word)) %>%
  group_by(type, year) %>%
  summarize(care=sum(care_group)/n())

props_norm_words


props_norm_words <- transcripts.df %>%
  unnest_tokens(input=text, output=word, token="words") %>%
  anti_join(get_stopwords()) %>%
  group_by(type, year) %>%
  count(word, sort=TRUE)




