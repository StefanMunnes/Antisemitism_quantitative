welcome <- c(
  "# Welcome to the Interactive Data Visualisation",
  "On this page you can examine and compare the characteristics of antisemitic comments from different discourse events. To do this, select **two discourse events** at the upper left of this page that you want to compare.  ",
  "The discourse-specific information and visualisations are made comparable in two main columns. A short description of the content and some general information about the data as well as the number of (antisemitic) comments are displayed in the top row. The different visualisations can be selected and are displayed in the lower row.  ",
  "For each of these visualisations, there are **settings** that can be used to adjust the display of the results. To help you understand each graph type and the results, there is an **interpretation** example for the default discourse events. The visualisations are fully **interactive**. Move the mouse over graphical elements, zoom in or move elements to get a better insight.  ",
  "The different visualisations can be selected via the **tabs** in this part of the window. You can choose to display:  ",
  "1.	the frequency distributions of the different labels
  2.	the co-occurrence of these labels
  3.	the frequency distribution of antisemitic specific keywords
  4.	the network of these keywords  ",
  "  ",
  "### Author and Replication Materials",
  "[Stefan Munnes](mailto:munnes@wzb.eu)",
  "([WZB](https://www.wzb.eu/de/personen/stefan-munnes); [Decoding Antisemitism](https://decoding-antisemitism.eu/team/stefan/))  ",
  "Replication Material available on [GitHub](https://github.com/StefanMunnes/Antisemitism_quantitative/tree/main/report/shiny).  ",
  "Many thanks to the Berlin Social Science Center ([WZB](https://www.wzb.eu/en)) for hosting this application."
)

# create table overview for short labels and long version with headings
codes_list <- read.csv2("data/doc/codes_lab_short.csv")

names(codes_list) <- c("code", "Label", "Concept name")

codes_list$code_main <- as.numeric(substr(codes_list$code, 1, 1))

codes_grp <- seq(1:6)
names(codes_grp) <- c(
  "Classic Antisemitic Tropes", "Tropes of Political or Financial Power",
  "Secondary Antisemitism", "Further Post-Holocaust Concepts",
  "Attacks on Israel’s Legitimacy", "Speech Acts"
)

codes_tbl_ls <- sapply(codes_grp, function(grp) {
  table <- knitr::kable(
    codes_list[codes_list$code_main == grp, c("Label", "Concept name")],
    format = "markdown", row.names = FALSE
  )

  heading <- c(paste("###", names(codes_grp[grp])), table)
  if (grp > 1) heading <- c("---", heading)

  return(heading)
})

codes_tbl <- unlist(codes_tbl_ls, use.names = FALSE)


interpr_code_freq <- "Of the classic antisemitic tropes, ‘Evil/The Devil’ is the most common in social media comments on the Arab-Israeli conflict. This attribution is found, depending on the country, in 30% to 40% of the antisemitic comments in 2021. It is most common in German data, least common in French data. Compared to the 2023 discourse event, for which data is only available for the UK, the percentage has decreased to just over 10%."

interpr_code_net <- "At both points in time in the discourse event of the Arab-Israeli conflict, the label ‘Israel’s Sole Guilt in the Conflict’ appears most frequently with other labels in the same comment, mainly with the classic antisemitic trope of 'Evil/The Devil'. Compared to 2021, in the 2023 comments Israel-related antisemitic labels are much more frequently combined with the speech act of 'Affirming, Desiring, Calling for Violence’."

interpr_keyw_freq <- "Shown are the 30 most clearly antisemitic keywords from the two discourse events on the Arab-Israeli conflict in 2021 and 2023 that appear in at least 5 comments. These words are more likely to appear in comments labelled as antisemitic and were calculated using the chi-squared independence test. They may also appear in non-antisemitic comments but with a lower probability, taking into account the uneven distribution.\n
For the two discourse events, some keywords remain relatively constant and are formative for the entire discourse, e.g. 'apartheid' or 'zionist'. However, other keywords are more specific to the respective antisemitic discourse, e.g. 'settlers' or 'unarmed' in 2021 and 'occupation' or 'genocide' in 2023.\n
Overall, there is a strong reference to political subjects ('palestinians', 'israeli') as well as strong negative descriptions of activities ('oppression', 'kill', 'cleansing')."

interpr_keyw_net <- "These network plots show the relationships between keywords. Node size indicates how many comments the keyword appears in, and edge thickness indicates how often keywords appear together in a comment. For a better overview, only the 30% most important connections are shown.\n
For the discourse event Arab-Israeli conflict in 2021, the central importance of the two conflict parties 'palestinians' and 'israel' is shown, which are also strongly linked. It is therefore not surprising that most of the keywords are relevant for both terms. There are some small differences, such as the one-sided reference to 'oppression' and 'palestinians', as well as the stronger connection between 'israel' and 'killing' and 'terrorist'.\n
A similar picture emerges for the year 2023. Particularly striking is the slogan 'free' and 'palestine' plus 'emoji', which indicates a stronger political mission on the one hand, and an attribution of guilt on the other with 'regime', 'israel', 'apartheid', 'illegal' and 'occupation'."

validate <- paste0(
  "No valid data available\n",
  "Choose different country or discourse event"
)

text <- list(
  welcome, codes_tbl,
  interpr_code_freq, interpr_code_net,
  interpr_keyw_freq, interpr_keyw_net,
  validate
)
names(text) <- c(
  "welcome", "codes_tbl",
  "interpr_code_freq", "interpr_code_net",
  "interpr_keyw_freq", "interpr_keyw_net",
  "validate"
)

save(text, file = "report/shiny/text.Rdata")
