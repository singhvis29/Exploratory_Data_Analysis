#Data Creation

library(tidyverse)
library(readr)
library(OneR)
spotify <- read_csv("spotify.filtered.data.csv")

spotify_df<-dplyr::select(spotify,genre,popularity,acousticness,loudness,duration_ms,liveness,tempo,speechiness,danceability,mode,track_name, valence)


x<-boxplot(spotify_df$popularity,plot=FALSE)$out #okay
y<-boxplot(spotify_df$loudness,plot=FALSE)$out
z<-boxplot(spotify_df$acousticness,plot=FALSE)$out#good
#a<-boxplot(spotify_df$energy,plot=FALSE)$out#good
b<-boxplot(spotify_df$duration_ms,plot=FALSE)$out
c<-boxplot(spotify_df$instrumentalness,plot=FALSE)$out
d<-boxplot(spotify_df$liveness,plot=FALSE)$out
e<-boxplot(spotify_df$tempo,plot=FALSE)$out#good
f<-boxplot(spotify_df$speechiness,plot=FALSE)$out
g<-boxplot(spotify_df$valence,plot=FALSE)$out


spotify_df_final <- spotify_df[-which(spotify_df$popularity %in% x),]
spotify_df_final <- spotify_df_final[-which(spotify_df_final$loudness %in% y),]
#spotify_df_final <- spotify_df_final[-which(spotify_df_final$acousticness %in% z),]
#spotify_df_final <- spotify_df_final[-which(spotify_df_final$energy %in% a),]
spotify_df_final <- spotify_df_final[-which(spotify_df_final$duration_ms %in% b),]
#spotify_df_final <- spotify_df_final[-which(spotify_df_final$instrumentalness %in% c),]
spotify_df_final <- spotify_df_final[-which(spotify_df_final$liveness %in% d),]
spotify_df_final <- spotify_df_final[-which(spotify_df_final$tempo %in% e),]
spotify_df_final <- spotify_df_final[-which(spotify_df_final$speechiness %in% f),]
#spotify_df_final <- spotify_df_final[-which(spotify_df_final$valence %in% g),]

### Categorical Variables
## Speechiness
spotify_df_final$speechiness.level <- as.numeric(bin(spotify_df_final$speechiness, nbins = 3, method = "content"))
spotify_df_final$speechiness.level <- factor(spotify_df_final$speechiness.level,levels=c(1,2,3),labels=c("Low","Medium","High"))

## Loudness
spotify_df_final$loudness.level <- as.numeric(bin(spotify_df_final$loudness, nbins = 3, method = "content"))
spotify_df_final$loudness.level <- factor(spotify_df_final$loudness.level,levels=c(1,2,3),labels=c("Low","Medium","High"))

## Liveness
spotify_df_final$liveness.level <- as.numeric(bin(spotify_df_final$liveness, nbins = 2, method = "content"))
spotify_df_final$liveness.level <- factor(spotify_df_final$liveness.level,levels=c(1,2),labels=c("Low","High"))

## Tempo
#spotify_df_final$tempo.level <- as.numeric(bin(spotify_df_final$tempo, nbins = 4, method = "content"))
#spotify_df_final$tempo.level <- factor(spotify_df_final$tempo.level,levels=c(1,2,3,4),labels=c("Slow", "Moderate", "Fast"))

spotify_df_final = spotify_df_final %>%
  mutate(tempo.level = cut(tempo, breaks = c(-Inf,100,135,Inf), labels = factor(c("Slow", "Moderate", "Fast"))))

spotify_df_final$duration_s = spotify_df_final$duration_ms/1000

spotify_df_final %>% group_by(tempo.level) %>% count()

spotify_df_final = spotify_df_final %>% filter(genre %in% c("Jazz","Country","Electronic","Hip-Hop","Pop","Rock"))

write.csv(spotify_df_final, file = paste0("spotify.dataset.final.csv"),row.names=FALSE)




