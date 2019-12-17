library(tidyverse)
library(rtweet)
library(httpuv)

## autheticate via web browser
token <- create_token(
  app = "Daljit_R",
  consumer_key = "rVivFp6oLxRtraTR0ooI5pqPN",
  consumer_secret = "lMVZgtJllyeG2Lhkhlz2XoG2G7iFj9dT7DPTD3IlljPlwuVB2U",
  access_token = "94237853-AVRrIoF0xhDFxbbewzpRXxMiV6vuYboZ3RSyWxaOP",
  access_secret = "MU1v18oIbwNYgOkiZEmYetCCTdlMqBtnH52aIBVTyyVMQ")


######### my favourite accounts #############
my_favs <- get_favorites("DaljitJohal",n=3000) %>% 
  group_by('account' = screen_name) %>% 
  tally(sort=TRUE, name='likes') %>% 
  slice(1:10)

theme_set(theme_bw())

# Plot
ggplot(my_favs, aes(x=reorder(account,likes), y=likes)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=account, 
                   xend=account, 
                   y=0, 
                   yend=likes)) + 
  labs(title="My most liked Twitter accounts") +
  xlab("account") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + coord_flip()



harry_favs <- get_favorites("HarryCook1",n=3000) %>% 
  group_by('account' = screen_name) %>% 
  tally(sort=TRUE, name='likes') %>% 
  slice(1:10)

theme_set(theme_bw())

# Plot
ggplot(harry_favs, aes(x=reorder(account,likes), y=likes)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=account, 
                   xend=account, 
                   y=0, 
                   yend=likes)) + 
  labs(title="Harry's most liked Twitter accounts") +
       xlab("account") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + coord_flip()



Scott_favs <- get_favorites("sc0ttzilla",n=3000) %>% 
  group_by('account' = screen_name) %>% 
  tally(sort=TRUE, name='likes') %>% 
  slice(1:10)

theme_set(theme_bw())

# Plot
ggplot(Scott_favs, aes(x=reorder(account,likes), y=likes)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=account, 
                   xend=account, 
                   y=0, 
                   yend=likes)) + 
  labs(title="Scott's most liked Twitter accounts") +
  xlab("account") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + coord_flip()


## search for 18000 tweets using the rstats hashtag
rt <- search_tweets(
  "#arsenal", n = 18000, include_rts = FALSE
)

## preview tweets data
rt

## preview users data
users_data(rt)

## plot time series (if ggplot2 is installed)
ts_plot(rt)

## plot time series of tweets
ts_plot(rt, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #arsenal Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

