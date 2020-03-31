library(rvest)

transcripts <- data.frame(url=c("https://www.rev.com/blog/transcripts/march-democratic-debate-transcript-joe-biden-bernie-sanders",
                  "https://www.rev.com/blog/transcripts/south-carolina-democratic-debate-transcript-february-democratic-debate",
                  "https://www.rev.com/blog/transcripts/democratic-debate-transcript-las-vegas-nevada-debate",
                  "https://www.rev.com/blog/transcripts/new-hampshire-democratic-debate-transcript",
                  "https://www.rev.com/blog/transcripts/january-iowa-democratic-debate-transcript",
                  "https://www.rev.com/blog/transcripts/december-democratic-debate-transcript-sixth-debate-from-los-angeles",
                  "https://www.rev.com/blog/transcripts/november-democratic-debate-transcript-atlanta-debate-transcript",
                  "https://www.rev.com/blog/transcripts/october-democratic-debate-transcript-4th-debate-from-ohio",
                  "https://www.rev.com/blog/transcripts/democratic-debate-transcript-houston-september-12-2019",
                  "https://www.rev.com/blog/transcripts/transcript-of-july-democratic-debate-2nd-round-night-2-full-transcript-july-31-2019",
                  "https://www.rev.com/blog/transcripts/transcript-of-july-democratic-debate-night-1-full-transcript-july-30-2019",
                  "https://www.rev.com/blog/transcripts/transcript-from-night-2-of-the-2019-democratic-debates",
                  "https://www.rev.com/blog/transcripts/transcript-from-first-night-of-democratic-debates"))

transcripts <- apply(transcripts, 1, function(x){
  URL <- read_html(x['url'])

  results <- URL %>% html_nodes("head")
  
  records <- vector("list", length = length(results))
  
  for (i in seq_along(records)) {
    title <- xml_contents(results[i] %>% html_nodes("title"))[1] %>% html_text(trim = TRUE)
    desc <- html_nodes(results[i], "meta[name=description]") %>% html_attr("content")
    kw <- html_nodes(results[i], "meta[name=keywords]") %>% html_attr("content")
  }
  
  return(data.frame(name = x['name'],
                    url = x['url'],
                    title = ifelse(length(title) > 0, title, NA),
                    description = ifelse(length(desc) > 0, desc, NA),
                    kewords = ifelse(length(kw) > 0, kw, NA)))
})



URL <- read_html(transcripts[1])


