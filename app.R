############ LOADING LIBRARIES & SETTING WD ############
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(stringr)
library(scales)
library(grid)
library(gridExtra)
library(jpeg)
library(png)

path <- "~/Praca&pohovory/DataScentics/BookRecommender"
setwd(path)


############ LOADING DATASETS ############ 
# https://github.com/zygmuntz/goodbooks-10k

books <- read.csv("books.csv", header = TRUE) %>%
    select(-c(best_book_id, work_id, books_count, isbn13, 
              work_ratings_count, work_text_reviews_count)) %>%
    filter(!(language_code %in% c("rus", "pol", "jpn", "per", "ara"))) %>%
    mutate(language_code = ifelse(grepl("en", language_code), "en", language_code),
           ratings_count = ratings_1 + ratings_2 + ratings_3 + ratings_4 + ratings_5,
           average_rating = round((1*ratings_1 + 2*ratings_2 + 3*ratings_3 + 4*ratings_4 + 5*ratings_5)/ratings_count, 2))
book_tags <- read.csv("book_tags.csv", header = TRUE)
tags      <- read.csv("tags.csv", header = TRUE)
genres    <- read.csv("Genres.csv", header = FALSE) %>% t() %>% as.vector()

languages <- c("Not in the list", sort(unique(books$language_code)[-2]))


############ STATISTICS ############
# TOP RATED AUTHORS
top5_authors <- books %>% 
    group_by(authors) %>%
    summarize(ratings_count = sum(ratings_count),
              ratings_1 = sum(ratings_1),
              ratings_2 = sum(ratings_2),
              ratings_3 = sum(ratings_3),
              ratings_4 = sum(ratings_4),
              ratings_5 = sum(ratings_5)) %>%
    mutate(average_rating = (1*ratings_1 + 2*ratings_2 + 3*ratings_3 + 4*ratings_4 + 5*ratings_5)/(ratings_1 + ratings_2 + ratings_3 + ratings_4 + ratings_5)) %>%
    filter(ratings_count > 100000)
top5_authors$authors <- str_wrap(top5_authors$authors, width = 20)

top5rated_authors    <- top5_authors %>% select(authors, average_rating) %>% top_n(5, average_rating)
top5reviewed_authors <- top5_authors %>% select(authors, ratings_count)%>% top_n(5, ratings_count)

# TOP RATED BOOKS
top5rated_books     <- books %>% select(title, average_rating) %>% top_n(5, average_rating)
top5reviewed_books  <- books %>% select(title, ratings_count)%>% top_n(5, ratings_count)
top5rated_books$title    <- str_wrap(top5rated_books$title, width = 20)
top5reviewed_books$title <- str_wrap(top5reviewed_books$title, width = 20)


plot1 <- top5rated_authors %>%
    ggplot(., aes(x=reorder(authors, -average_rating), y=average_rating)) +
    geom_bar(stat='identity') +
    labs(title="Top5 rated authors", x ="Titles", y = "Rating") + 
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 

plot2 <- top5reviewed_authors %>%
    ggplot(., aes(x=reorder(authors, -ratings_count), y=ratings_count)) +
    geom_bar(stat='identity') +
    labs(title="Top5 most reviewed authors", x ="Titles", y = "Reviews count") + 
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + 
    scale_y_continuous(labels = comma)

plot3 <- top5rated_books %>%
    ggplot(., aes(x=reorder(title, -average_rating), y=average_rating)) +
    geom_bar(stat='identity') +
    labs(title="Top5 rated books", x ="Titles", y = "Rating") + 
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 

plot4 <- top5reviewed_books %>%
    ggplot(., aes(x=reorder(title, -ratings_count), y=ratings_count)) +
    geom_bar(stat='identity') +
    labs(title="Top5 most reviewed books", x ="Titles", y = "Reviews count") + 
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + 
    scale_y_continuous(labels = comma)


############ HELPFUL FUNCTIONS ############
text_format <- function(text){
    # Regex function for string manipulation
    text <- gsub("[^[:alnum:]]", "", text)
    text <- tolower(text)
    return(text)
}

filtration <- function(data, selected_column, searched_text){
    # Regex function for string comparison
    tmp <- data[str_detect(text_format(data[[selected_column]]), 
                           paste(text_format(searched_text),collapse = '|')),]
    return(tmp)
}

minmax <- function(x){
    # Minmax data normalization
    return((x-min(x))/(max(x)-min(x)))
}

visualise_recommendation <- function(recommendations){
    
    plot <- list()
    dir.create("recommended_images")
    
    nbooks <- nrow(recommendations)
    for(i in 1:nbooks){
        # Create dir & Download the images
        img  <- recommendations[i,3]
        img_format <- substr(img, nchar(img)-2, nchar(img))
        name <- paste0("recommended_images/",i,".",img_format)
        suppressMessages(
            download.file(as.character(img), destfile = name, mode = "wb")
        )
        
        # Assign Object
        if (img_format == "jpg") {
            plot[[i]] = rasterGrob(readJPEG(name))
        } else {
            plot[[i]] = rasterGrob(readPNG(name))
        }
    }
    do.call(marrangeGrob, args = list(plot, ncol = nbooks, nrow = 1, top=""))
    
}


############ MAIN RECOMMENDATION FUNCTION ############

recommendation_engine <- function(books, input_title, input_author, input_language, input_year, input_genres, topN){
    # books ............ loaded books dataset
    # input_title ...... book title
    # input_language ... language code that specifies which language is used in the book
    # input_year ....... publication year
    # input_genres ..... genres of the inputed book
    
    # Inspection of inputed title & check if is in the list
    selection <- filtration(books, "title", input_title)
    
    if ((nrow(selection)==0)|((nrow(selection)>1) & (input_author!="") & !(is.na(input_year)) & !(is.null(input_genres)))) {
        # If not in the list
        if ((input_author=="") & is.null(input_genres)) {
            df_visual <- NULL
            txt <- "Please, fill additional information about the book."
        } else {
            # Which books have similar genres tags
            tmp_genres <- filtration(tags, "tag_name", input_genres)
            tmp_genres <- book_tags %>% 
                inner_join(tmp_genres, by = "tag_id") %>%
                mutate(goodreads_book_id_char = as.character(goodreads_book_id)) %>%
                group_by(goodreads_book_id_char) %>%
                summarize(goodreads_book_id = mean(goodreads_book_id), num_of_tags = n()) %>%
                select(-goodreads_book_id_char)
            
            # Creating new columns specifing which books have similar year of publication, language, genres, authors
            df <- books %>% mutate(is_author = ifelse(authors %in% input_author, 1, 0))
            df <- df %>% mutate(is_langue = ifelse(language_code %in% input_language, 1, 0))
            df <- df %>% mutate(year_dist = abs(input_year - original_publication_year),
                                year_dist = ifelse(is.na(year_dist), mean(na.omit(year_dist)), year_dist),
                                year_dist = 1 - minmax(year_dist))
            df <- df %>% left_join(tmp_genres, by = "goodreads_book_id") %>% 
                         mutate(num_of_tags = ifelse(is.na(num_of_tags), mean(na.omit(num_of_tags)), num_of_tags),
                                tags_dist = minmax(num_of_tags),
                                ratings_dist = minmax(average_rating))
            # Creating a vector of similarities
            df <- df %>% mutate(similarity  = is_author + is_langue + year_dist^2 + tags_dist^2 + ratings_dist^2)
            
            # DF visualisation
            df_visual <- df %>% top_n(topN, similarity) %>% 
                                select(title, small_image_url, image_url)
            
            # Selecting TOP N suggestion books
            df_top5 <- df %>% top_n(topN, similarity) %>% 
                              select(title, authors, original_publication_year, average_rating)
            # Output text
            collapsed_df <- apply(df_top5, 1, function(x) paste(x, collapse = ", "))
            txt <- c("As your next book we suggest one of the following (title, author/s, publication year, avg rating):", 
                         paste0(1:length(collapsed_df),". ",collapsed_df))
        }
    } else if (nrow(selection)==1) {
        # Information about the book
        bookID    <- selection$book_id
        gr_bookID <- selection$goodreads_book_id
        author    <- selection$authors
        language  <- selection$language_code
        year      <- selection$original_publication_year
        
        # Tags selection
        tmp_book_tags <- book_tags %>% filter(goodreads_book_id == gr_bookID) %>% top_n(5, count)
        tmp_tags <- tags %>% filter(tag_id %in% tmp_book_tags$tag_id) %>% select(tag_name) %>% pull()
        tmp_genres <- filtration(tags, "tag_name", tmp_tags)
        tmp_genres <- book_tags %>% 
            inner_join(tmp_genres, by = "tag_id") %>%
            mutate(goodreads_book_id_char = as.character(goodreads_book_id)) %>%
            group_by(goodreads_book_id_char) %>%
            summarize(goodreads_book_id=mean(goodreads_book_id), num_of_tags = n()) %>%
            select(-goodreads_book_id_char)
       
        # Creating new columns specifing which books have similar year of publication, language, genres, authors
        df <- books %>% mutate(is_author = ifelse(authors == author, 1, 0))
        df <- df %>% mutate(is_langue = ifelse(language_code == language, 1, 0))
        df <- df %>% mutate(year_dist = abs(year - original_publication_year),
                            year_dist = ifelse(is.na(year_dist), mean(na.omit(year_dist)), year_dist),
                            year_dist = 1 - minmax(year_dist))
        df <- df %>% left_join(tmp_genres, by = "goodreads_book_id") %>% 
            mutate(num_of_tags = ifelse(is.na(num_of_tags), mean(na.omit(num_of_tags)), num_of_tags),
                   tags_dist = minmax(num_of_tags),
                   ratings_dist = minmax(average_rating)) %>%
            filter(book_id != bookID)
        
        # Creating a vector of similarities
        df <- df %>% mutate(similarity  = is_author + is_langue + year_dist^2 + tags_dist^2 + ratings_dist^2)
        
        # DF visualisation
        df_visual <- df %>% top_n(topN, similarity) %>% 
            select(title, small_image_url, image_url)
        
        # Selecting TOP5 suggestion books
        df_top5 <- df %>% top_n(topN, similarity) %>% 
            select(title, authors, original_publication_year, average_rating)
        
        # Output text
        collapsed_df <- apply(df_top5, 1, function(x) paste(x, collapse = ", "))
        txt <- c("As your next book we suggest one of the following (title, author/s, publication year, avg rating):", 
                     paste0(1:length(collapsed_df),". ",collapsed_df))
    } else {
        # If is in the list but there is several selections
        sel <- selection %>% select(title) %>% pull()
        df_visual <- NULL
        txt <- c("Please, copy from displayed books or fill additional information:", paste("-",sel))
    }
    
    # Returns text to be displayed and dataset containing urls for books visualisation
    return(list(txt = txt, df_visual = df_visual))
}
# ############ TRAIN INPUTS ############
# input_title    <- "The Hunger Games"
# input_author   <- "J.R.R. Tolki"
# input_language <- "en"
# input_year     <- 1995
# input_genres   <- c("scifi", "fiction", "action")
# topN <- 5


############ SHINY APP ############
about_page <- tabPanel(
    title = "About",
    "Created by Jakub Hrban",
    br(),
    "2021 August"
)

statistics_page <- tabPanel(
    title = "Hall Of Fame",
    mainPanel(
        plotOutput("plot1"),
        plotOutput("plot2"),        
        plotOutput("plot3"),
        plotOutput("plot4"),
    )
)

main_page <- tabPanel(
    title = "Choose your next one",
    titlePanel("Book Recommendation"),
    sidebarLayout(
        sidebarPanel(
            textInput("title_var", "Most recently finished book :", value = "", placeholder = "Here enter the book title"),
            prettySwitch("pretty", "Additional info", value = FALSE, slim = TRUE),

            uiOutput("additional_info1"),
            uiOutput("additional_info2"),
            uiOutput("additional_info3"),
            uiOutput("additional_info4"),
            
            br(),
            actionButton("run_button", "Recommend Book", icon = icon("play"))
            
        ),
        mainPanel(
            verbatimTextOutput("text", placeholder = FALSE),
            plotOutput("plot")
        )
    )
)

ui <- navbarPage(
    title = "Book Recommender",
    theme = shinytheme('united'),
    main_page,
    statistics_page,
    about_page
)

server <- function(input, output) {
    
    output$additional_info1 <- renderUI({
        req(input$pretty)
        textInput("author_var", "Author Name")
    })
    output$additional_info2 <- renderUI({
        req(input$pretty)
        selectInput("language_var", "Language", choices = languages)
    })
    output$additional_info3 <- renderUI({
        req(input$pretty)
        numericInput("year_var", "Year Published", NULL)
    })
    output$additional_info4 <- renderUI({
        req(input$pretty)
        selectizeInput("genre_vars", "Genre", choices = genres,
                       selected = NULL, multiple = TRUE, options = list(maxItems = 3))
    })
    
    pretty_var    <- eventReactive(input$run_button,input$pretty)
    
    title_var     <- eventReactive(input$run_button,input$title_var)
    author_var    <- eventReactive(input$run_button,input$author_var)
    language_var  <- eventReactive(input$run_button,input$language_var)
    year_var      <- eventReactive(input$run_button,input$year_var)
    genre_vars    <- eventReactive(input$run_button,input$genre_vars)
    
    # Printing text
    output$text <- renderPrint({
        if (!pretty_var()){
            cat(recommendation_engine(books, title_var(), "", NULL, NA, NULL, 5)$txt, sep = "\n")
        } else {
            cat(recommendation_engine(books, title_var(), author_var(), language_var(), year_var(), genre_vars(), 5)$txt, sep = "\n")
        }
    })
    
    # Visualising book covers
    output$plot <- renderPlot({
        if (!pretty_var()){
            recommendation <- recommendation_engine(books, title_var(), "", NULL, NA, NULL, 5)
            if (is.null(recommendation$df_visual)) {
                plot.new()
            } else {
                visualise_recommendation(recommendation$df_visual)
            }
        } else {
            recommendation <- recommendation_engine(books, title_var(), author_var(), language_var(), year_var(), genre_vars(), 5)
            if (is.null(recommendation$df_visual)) {
                plot.new()
            } else {
                visualise_recommendation(recommendation$df_visual)
            }
        }
    })
    
    # Visualising TOP5 books/authors by rating/# of reviews
    output$plot1 <- renderPlot({
        plot1
    })
    output$plot2 <- renderPlot({
        plot2
    })
    output$plot3 <- renderPlot({
        plot3
    })
    output$plot4 <- renderPlot({
        plot4
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

