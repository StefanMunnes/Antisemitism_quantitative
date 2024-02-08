welcome <- c(
  "# Welcome to the Interactive Data Visualization",
  "On this page you can examine and compare the characteristics of antisemitic comments from different discourse events**. To do this, select **two discourse events** at the top of this page that you want to compare with each other. A short description of the content and some general information about the data and the number of (antisemitic) comments will appear for each of them.  ",
  "For each of these visualisations there are **settings** that can be used to adjust the display of the results. To help you understand each graph, there is an **interpretation** example.  ",
  "In addition to selecting discourse events and plotting settings, the visualisations are fully **interactive**. Move the mouse over graphical elements, zoom in or move elements to get a better insight.  ",
  "The different visualizations can be selected via the **tabs** in this part of the window. You can choose to display:",
  "1.	the frequency distributions of the different labels
  2.	the co-occurrence of these labels
  3.	the frequency distribution of antisemitic specific keywords
  4.	the network of these keywords  ",
  "  ",
  "### Author and Reproduction Materials",
  "[Stefan Munnes](mailto:munnes@wzb.eu)
  ([WZB](https://www.wzb.eu/de/personen/stefan-munnes); [Decoding Antisemitism](https://decoding-antisemitism.eu/team/stefan/); [GitHub](https://github.com/StefanMunnes/Antisemitism_quantitative/tree/main/report/shiny))  ",
  "Many thanks to the Berlin Social Science Center ([WZB](https://www.wzb.eu/en)) for hosting this application."
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

interpr_keyw_freq <- "Shown are the 30 most clearly antisemitic keywords from the two discourse events on the Arab-Israeli conflict in 2021 and 2023 that appear in at least 5 comments. These are words that are more likely to appear in comments labeled as antisemitic and were calculated using the chi-squared independence test. They may also appear in non-antisemitic comments, but with a lower probability, taking into account the uneven distribution.\n
For the two discourse events, it can be seen that some keywords remain relatively constant and are formative for the entire discourse, e.g. 'apartheid' or 'zionist'. However, there are also keywords that are more specific to the respective antisemitic discourse, e.g. 'settlers' or 'unarmed' in 2021 and 'occupation' or 'genocide' in 2023.\n
Overall, there is a strong reference to political subjects ('palestinians', 'israeli') as well as strong negative descriptions of activities ('oppression', 'kill', 'cleansing')."

interpr_keyw_net <- "These network plots show the relationships between keywords. Node size indicates how many comments the keyword appears in, edge thickness indicates how often keywords appear together in a comment. For a better overview, only the 30% most important connections are shown.\n
For the discourse event Arab-Israeli conflict in 2021, the central importance of the two conflict parties 'palestinians' and 'israel' is shown, which are also strongly linked. It is therefore not surprising that most of the keywords are relevant for both terms. There are some small differences, such as the one-sided reference to 'oppression' and 'palestinians', as well as the stronger connection between 'israel' and 'killing' and 'terrorist'.\n
A similar picture emerges for the year 2023. Striking is the strong slogan 'free' and 'palestine' plus 'emoji', which indicates a stronger political mission on the one hand, as well as an attribution of guilt on the other hand with 'regime', 'israel', 'apartheid', 'illegal' and 'occupation'."

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
