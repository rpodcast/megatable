library(tidyverse)
library(janitor)
library(webshot)
library(robotstxt)
library(rvest)

# set up data frame for keeping results
megaman_df <- tibble::tibble(
  game_index = c(1:8),
  enemy_url = glue::glue("http://megaman.wikia.com/wiki/List_of_Mega_Man_{index}_Enemies", index = game_index)
)


url <- "http://megaman.wikia.com/wiki/List_of_Mega_Man_2_Enemies"

enemy_chart_content <- read_html(url)

# let's isolate element 63 which corresponds to the acid drop enemy
# why is table[2] skipped?  Looks like that one is invisible

# xpath for name of enemy acid drop:  //*[@id="mw-content-text"]/table[3]/tbody/tr[2]/td/table[1]/tbody/tr[1]/td
# xpath for name of enemy bubble bat: //*[@id="mw-content-text"]/table[3]/tbody/tr[2]/td/table[3]/tbody/tr[1]/td
# xpath for name of enemy big fish:   //*[@id="mw-content-text"]/table[3]/tbody/tr[2]/td/table[4]/tbody/tr[1]/td
# xpath for name of enemy blocky:     //*[@id="mw-content-text"]/table[3]/tbody/tr[2]/td/table[5]/tbody/tr[1]/td

# xpath for data of enemy acid drop:  //*[@id="mw-content-text"]/table[3]/tbody/tr[2]/td/table[1]/tbody/tr[2]/td/table
# xpath for data of enemy bubble bat: //*[@id="mw-content-text"]/table[3]/tbody/tr[2]/td/table[3]/tbody/tr[2]/td/table
# xpath for data of enemy big fish:   //*[@id="mw-content-text"]/table[3]/tbody/tr[2]/td/table[4]/tbody/tr[2]/td/table

# xpath for hazards table:    //*[@id="mw-content-text"]/table[4] 
# xpath for sub-bosses table: //*[@id="mw-content-text"]/table[5]
# xpath for robot masters table:  //*[@id="mw-content-text"]/table[6]
# xpath for wily bosses table: //*[@id="mw-content-text"]/table[7]

# try to dynamically determine how many rows are in the table
# first try to determine what is different in 'valid' rows
# looks like the non-valid tables have an attribute class='hiddenStructure'

#//*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td/table[1]


#//*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td

# this grabs all of the "tables" in the big table of enemies
table_xpath <- '//*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td'
enemy_tables <- html_nodes(enemy_chart_content, xpath = table_xpath) %>%
  html_nodes(., "table") %>%
  html_nodes(., "tbody")



# //*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td
# //*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td/table[1]/tbody
# //*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td/table[1]/tbody/tr[1]

# enemy pic    
# //*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td/table[1]/tbody/tr[1]/td
# #mw-content-text > div.mw-parser-output > table:nth-child(11) > tbody > tr:nth-child(2) > td > table:nth-child(1) > tbody > tr:nth-child(1) > td
# enemy stats  
# //*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td/table[1]/tbody/tr[2]/td
# #mw-content-text > div.mw-parser-output > table:nth-child(11) > tbody > tr:nth-child(2) > td > table:nth-child(1) > tbody > tr:nth-child(2) > td

# this blcok gets enemy name and image
# need href attributes for link to image and then enemy name
enemy_tables[[1]] %>%
  html_elements(., xpath = "tr[1]/td") %>%
  html_elements("a") %>%
  html_text() # gets text (second element)
  html_attr("href") # gets hyperlink (first element)


# this blcok gets enemy stats
enemy_tables[[1]] %>%
  html_elements(., xpath = "tr[2]/td") %>%
  html_elements(., "table") %>%
  html_table()



purrr::map(enemy_tables, ~{
  html_nodes(.x, "table")
   # html_nodes(., "tbody")
  })

# create tibble to host enemy content
enemy_df <- tibble::tibble(
  tbl_index = 1:length(enemy_tables)
) %>%
  mutate(valid_tables = purrr::map_lgl(tbl_index, ~{
    html_nodes(enemy_tables[.x], "table")
    is.na(html_attr(enemy_nodeset[.x], "class"))
  })) %>%
  filter(valid_tables)


custom_path <- '//*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td/table[1]'
html_node(enemy_chart_content, xpath = custom_path) %>% html_attrs(.)

custom_path <- '//*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td/table[34]'
html_node(enemy_chart_content, xpath = custom_path) %>% html_attrs(.)

# AHA!  All of the valid rows will not have any attribute of class defined
# //*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td/table[1]

# //*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td/table[2]

custom_path <- '//*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td/table[1]'
html_nodes(enemy_chart_content, xpath = custom_path) %>%
  html_nodes(., "tbody") %>%
  html_nodes(., "tr") %>%
  html_nodes(., "td") %>%
  html_text()
  map_chr(., ~html_attr(.x, "class"))
  

enemy_nodeset <- html_nodes(enemy_chart_content, xpath = custom_path)

enemy_nodeset[2] %>%
  html_nodes(., "tr") %>%
  html_nodes(., "td") %>%
  html_nodes(., "table") %>%
  html_nodes(., "tr") %>%
  html_text()



# experiment with selecting based on xpath
custom_path <-  '//*[@id="mw-content-text"]/div[1]/table[3]'
enemy_table2 <- html_nodes(enemy_chart_content, xpath = custom_path) %>%
  html_nodes(., "tr") %>%
  html_nodes(., "td") %>%
  html_nodes(., "table") %>%
  html_nodes(., "tr")
enemy_table2
#map(., ~html_table(.x, fill = TRUE, header = FALSE))

# KEY TIP: need to remove "tbody" from the xpaths obtained from chrome inspector
# the following gets the data associated with acid drop
custom_path <-  '//*[@id="mw-content-text"]/table[3]/tr[2]/td/table[1]/tr[2]/td/table'
html_node(enemy_chart_content, xpath = custom_path) %>% html_table(.)

custom_path <-  '//*[@id="mw-content-text"]/table[3]/tr[2]/td/table[33]/tr[2]/td/table'
html_node(enemy_chart_content, xpath = custom_path) %>% html_table(.)

# the following gets the name associated with the enemy as well as the link associated with it
custom_path <- '//*[@id="mw-content-text"]/table[3]/tr[2]/td/table[1]/tr[1]/td'
html_node(enemy_chart_content, xpath = custom_path) %>% html_node("b") %>% html_node("a") %>% html_attr("href")
html_node(enemy_chart_content, xpath = custom_path) %>% html_node("b") %>% html_node("a") %>% html_attr("title")



html_node(enemy_chart_content, xpath = custom_path) %>% html_node("table") %>% html_attrs(.)

# I used selector gadget to select any table and then got rid of the top one
# however this seems to have unintended consequences
css_sel <- "table:nth-child(2)"
css_sel <- "table"

# I took the second element as that corresponds to the enemies table or so it seems
# taking the td elements gives us all of the entries for "Fly Boy" 
enemy_table <- html_nodes(enemy_chart_content, css_sel) %>%
  #.[2] %>%
  html_nodes("td")

# the first of the above td elements gives the name of the enemy
# the second of the above td elemtns is actually the sub table with the data we want
blah <- map(enemy_table, ~html_text(.x))
blah2 <- map(enemy_table, ~html_node(.x, "table"))


acid_raw <- blah2[63]
acid1 <- map(acid_raw, ~html_nodes(.x, "tr"))
acid2 <- map(acid1, ~html_nodes(.x, "td"))
acid3 <- map(acid2, ~html_nodes(.x, "table"))
acid4 <- map(acid3, ~html_table(.x, fill = FALSE, header = FALSE))
map(acid_raw, ~html_text(.x))
# a lot of these will have class "xml_missing" so we can flag which ones
# inspired by https://stackoverflow.com/questions/42135192/web-scraping-using-r-error-in-bind-rows-x-id 
keep <- map_lgl(blah2, ~class(.) != "xml_missing")

blah3 <- map(blah2[keep], ~html_node(.x, "tr")) 
keep <- map_lgl(blah3, ~class(.) != "xml_missing")

blah4 <- map(blah3[keep], ~html_table(.x, fill = TRUE, header = FALSE))

# here let's see about grabbing that data portion as a data frame
# SUCCESS!
enemy_table[[2]] %>%
  html_node("table") %>%
  html_table(fill = FALSE, header = FALSE)