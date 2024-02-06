welcome <- c(
  "# Welcome to the interactive data visualization",
  "On this page you can examine and compare the characteristics of antisemitic comments from different discourse events. To do this, select two discourse events at the top of this page that you want to compare with each other. A short description of the content and some general information about the data and the number of (antisemitic) comments will appear for each of them.\ ",
  "The different visualizations can be selected via the tabs in this part of the window. You can choose to display:",
  "1.	the frequency distributions of the different labels
  2.	the co-occurrence of these labels
  3.	the frequency distribution of antisemitic specific keywords
  4.	the network of these keywords",
  "\ ",
  "There are options for each of these visualizations that can be used to adjust the display of the results. An interpretation example for each graphic helps to understand it correctly. There is also additional information on the codes used or the method of the antisemitic keywords."
)

codes_tbl <- c(
  "| Short        | Label                            |
  |--------------|----------------------------------|
  | Oth_Foreign  | The Other/Foreign                |
  | Evil_Devil   | Evil/The Devil                   |
  | Child_Murder | Blood Libel/Child Murder         |
  | Repul_Dehum  | Repulsiveness and Dehumanisation |
  | Immorality   | Immorality                       |
  | Lie_Deceit   | Lie and Deceit                   |
  | Vengefulness | Vengefulness                     |
  | Disloyalty   | Disloyalty/Jewish Loyalty        |
  | Blame_for_AS | Blame for Antisemitism           |"
)


interpr_code_freq <- "Of the classic antisemitic tropes, ‘Evil/The Devil’ is the most common one in social media comments on the Arab-Israeli conflict. This attribution is found in 30% to 40% of antisemitic comments in 2021. Most common in Germany, least common in France. Compared to the discourse event from 2023, for which data is only available for the United Kingdom, the percentage has decreased to just over 10%."

interpr_code_net <- "At both points in time in the discourse event of the Arab-Israeli conflict, the label ‘Israel’s Sole Guilt in the Conflict’ appears most frequently with other labels in the same comment, mainly with the classic antisemitic trope of 'Evil/The Devil'. Compared to 2021, in the 2023 comments, the Israel-related antisemitic labels are much more frequently combined with the speech act of 'Affirming, Desiring, Calling for Violence’."

interpr_keyw_freq <- "..."

interpr_keyw_net <- "..."

text <- list(
  welcome, codes_tbl,
  interpr_code_freq, interpr_code_net,
  interpr_keyw_freq, interpr_keyw_net
)
names(text) <- c(
  "welcome", "codes_tbl",
  "interpr_code_freq", "interpr_code_net",
  "interpr_keyw_freq", "interpr_keyw_net"
)

save(text, file = "report/shiny/text.Rdata")
