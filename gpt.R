#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

install.packages("httr")
install.packages("jsonlite")
library(stringr)
library(httr)
library(jsonlite)


# Get and Save Personal API
my_API <- "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"
# Loading Required Libraries
library(stringr)
library(httr)
# Asking Questions to ChatGPT, Saving and Cleaning Answer
hey_chatGPT <- function(answer_my_question) {
  chat_GPT_answer <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_API)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-4",
      messages = list(
        list(
          role = "user",
          content = answer_my_question
        )
      )
    )
  )
  str_trim(content(chat_GPT_answer)$choices[[1]]$message$content)
}

response <- hey_chatGPT("In R, use the dataframe 'trade_history' to create a gt table for each club in unique_clubs showing a history of all their trades")
cat(response)

