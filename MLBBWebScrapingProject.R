library(tidyverse)
library(shiny)

#######DATA SCRAPING & CLEANING#######
#scraping the data from the website & turning it into one long column
characters = rvest::read_html("https://mobile-legends.fandom.com/wiki/List_of_heroes")
df = characters %>% rvest::html_elements("td") %>% rvest::html_text2()
df = data.frame(df)
df = df %>%  filter(!row_number() %in% c(1))
df = df %>% rename(total_data_uncleaned = df)

#making the lists
Names = df %>%
  slice(which(row_number() %% 9 == 1))
Class = df %>%
  slice(which(row_number() %% 9 == 3))
Trait = df %>%
  slice(which(row_number() %% 9 == 4))
Role = df %>%
  slice(which(row_number() %% 9 == 5))
Homeland = df %>%
  slice(which(row_number() %% 9 == 6))
Price = df %>%
  slice(which(row_number() %% 9 == 7))
Date = df %>%
  slice(which(row_number() %% 9 == 8))

#merging mini-data-frames into one data frame & renaming the columns 
final = list(Names, Class, Trait, Role, Homeland, Price, Date)

final = final %>% merge(final)
final = final %>% rename(Names = total_data_uncleaned)
final = final %>% rename(Class = total_data_uncleaned.1)
final = final %>% rename(Trait = total_data_uncleaned.2)
final = final %>% rename(Role = total_data_uncleaned.3)
final = final %>% rename(Homeland = total_data_uncleaned.4)
final = final %>% rename(Price = total_data_uncleaned.5)
final = final %>% rename(Date = total_data_uncleaned.6)

#fixing the multi-columns & wrong-columns 
#first, price:
final = final %>% separate(Price, c("Gold", "Diamonds/Tickets"), sep = "\n")

#second, Freya, Pharsa, & Odette:
#Freya:
final[which(grepl('Freya', final$Names)), "Gold"] <- NA
final[which(grepl('Freya', final$Names)), "Diamonds/Tickets"] <- 599
#Odette:
final[which(grepl('Odette', final$Names)), "Gold"] <- NA
final[which(grepl('Odette', final$Names)), "Diamonds/Tickets"] <- NA
#Pharsa
final[which(grepl('Pharsa', final$Names)), "Gold"] <- 32000
final[which(grepl('Pharsa', final$Names)), "Diamonds/Tickets"] <- 599

#third, class, trait, & role: 
final$Classes = final$Class
final$Traits = final$Trait
final$Roles = final$Role
final = final %>% separate(Class, c("Class 1", "Class 2"), sep = "/")
final = final %>% separate(Trait, c("Trait 1", "Trait 2"), sep = "/")
final = final %>% separate(Role, c("Role 1", "Role 2"), sep = "/")
final <- final[, c(1, 12, 2, 3, 13, 4, 5, 14, 6, 7, 8, 9, 10, 11)]

#change string #s to int #s
final = final %>% transform(Gold = as.integer(Gold))
final = final %>% transform(Diamonds.Tickets = as.integer(Diamonds.Tickets))
final = final %>% transform(Date = as.integer(Date))



#######SHINY APPLICATION CREATION#######
ui <- fluidPage(
  titlePanel("Are these your characters?:"),
  fluidRow(
    column(4,
           selectInput("role",
                       "Roles:",
                       c("All",
                         unique(as.character(final$Roles))))
    ),
    column(4,
           selectInput("home",
                       "Homelands",
                       c("All",
                         unique(as.character(final$Homeland))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)

server <- function(input, output, session) {
  output$table <- DT::renderDataTable(DT::datatable({
    data <- final
    if (input$role != "All") {
      data <- data[data$Roles == input$role,]
    }
    if (input$home != "All") {
      data <- data[data$Homeland == input$home,]
    }
    data
  }))
}
shinyApp(ui, server)

                            