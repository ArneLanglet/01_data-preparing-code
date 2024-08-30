library(stringi)
library(maps)




####### check duplicates 

# in title & DOI (where available)

app_df$titles <- str_to_lower(app_df$titles)

## delete first from wos
app_df <- app_df %>%
  arrange(collection)

duplicates <- app_df %>%
  group_by(titles) %>%
  filter(n() > 1) %>% group_by(titles, authors) %>% count() 

app_df <- app_df %>%
  distinct(titles, .keep_all = TRUE)




app_df$digitalObjectIdentifier <- str_to_lower(app_df$digitalObjectIdentifier)

duplicates_doi <- app_df %>%
  filter(digitalObjectIdentifier != "") %>% 
  filter(digitalObjectIdentifier != 0) %>% 
  filter(!is.na(digitalObjectIdentifier)) %>% 
  group_by(digitalObjectIdentifier) %>%
  filter(n() > 1) %>% group_by(digitalObjectIdentifier) %>% count() 


## deleting duplicate DOIs where they are not NA or ""

# Step 1: Temporarily remove rows 
temp_excluded <- app_df %>% 
  filter(is.na(digitalObjectIdentifier) | digitalObjectIdentifier == "" | digitalObjectIdentifier == "0")

# Step 2: Filter out these rows and then apply distinct()
app_df_filtered <- app_df %>%
  filter(!(is.na(digitalObjectIdentifier) | digitalObjectIdentifier == "" | digitalObjectIdentifier == "0")) %>%
  distinct(digitalObjectIdentifier, .keep_all = TRUE)

# Step 3: Add back the temporarily removed rows
app_df <- bind_rows(app_df_filtered, temp_excluded)

############# cleaning

# those without date info have no other info for most of the variables
na_dates <- app_df%>% filter(is.na(dates))

app_df <- app_df%>% filter(!is.na(dates) & dates != 1800)

## authors

# string to title
app_df$authors <- str_to_title(app_df$authors)



# keywords to lower
app_df$keywords <- str_to_lower(app_df$keywords)
# to UTF-8
app_df$keywords <- stri_enc_toutf8(app_df$keywords)
## replace NA with -



#### prepare keywords list to select input from


app_df$keywords <- str_to_lower(app_df$keywords)
app_df$keywords <- gsub("[\r\n]", " ", app_df$keywords)
app_df$keywords <- gsub(" - ", " ", app_df$keywords)
app_df$keywords <- gsub("-", " ", app_df$keywords)

app_df$keywords <- gsub(";", ",", app_df$keywords)
app_df$keywords <- gsub(":", ",", app_df$keywords)
app_df$keywords <- gsub(",", ", ", app_df$keywords)
app_df$keywords <- gsub("\"", "", app_df$keywords)
app_df$keywords <- gsub("\'", "", app_df$keywords)

app_df$keywords <- gsub("\\s*\\([^\\)]+\\)", "", as.character(app_df$keywords))

app_df$keywords <- gsub("\\b\\d+\\b", "", app_df$keywords)

app_df$keywords[is.na(app_df$keywords)]  <- "-"


# Remove all " and ` and ' from the string

app_df$keywords<- gsub('[\"`\'\'\']', '', app_df$keywords)
app_df$keywords <- gsub("<[^>]+>", "", app_df$keywords)
app_df$keywords <- gsub("\\$", "", app_df$keywords)
app_df$keywords <- gsub("/", "", app_df$keywords)


## clean type
# "awarded grant" is deleted - no information


app_df <- app_df %>% filter(documentType != "awarded grant")
app_df <- app_df %>% filter(documentType != "erratum")
app_df <- app_df %>% filter(documentType != "magazines")
app_df <- app_df %>% filter(documentType != "NEWS ITEM")



# Assuming you have a vector 'document_types' with all the document type strings

# Define logical groups
groupings <- list(
  article = c("journal article", "article", "research-article", "article; book chapter",
              "article; data paper", "article; early access", "article; proceedings paper",  "scholarly journals", 
              "article; meeting", "article; meeting paper", 
              "english abstract",  "journal article; research support, non-u.s. gov't", "retracted",
              "review", "english abstract; journal article; research support, non-u.s. gov't",
              "review; early access"
  ),
  book_or_chapter = c("books", "book review", "book chapter", "book", "book; meeting", "book chapter; meeting",
                      "book; meeting; book chapter", "book; book chapter", "chapter", "book chapter", "editorial material; book chapter", "book chapter; meeting paper"),
  proceeding = c("conference papers & proceedings", "proceedings paper",
                 "conference paper", "conference review", "working papers"),
  #  review = c("review", "review; early access"),
  editorial = c("editorial material", "letter", "editorial", "note"),
  dissertations = c("dissertations & theses", "dissertation/thesis"),
  data_or_software = c("software", "short survey", "data study"),
  other = c("other sources")
  # Add more groupings as necessary
)

# Function to assign group based on keyword matching
assign_group <- function(document_type, groups) {
  for (group_name in names(groups)) {
    if (tolower(document_type) %in% tolower(groups[[group_name]])) {
      return(group_name)
    }
  }
  return(NA) # Return NA if no group matches
}


# Clean and group document types
app_df$documentType <- sapply(app_df$documentType, assign_group, groups = groupings)

na_documentType <- app_df %>% filter(is.na(documentType))
app_df <- app_df %>% filter(documentType != "meeting")



# publishers to lower
app_df$publishers <- str_to_lower(app_df$publishers)


# Check the result



### add iucn classification information



app_df <- merge(app_df, iucn_class, by = "digitalObjectIdentifier", all.x = TRUE)

app_df$realm <- ifelse(is.na(app_df$realm.x), app_df$realm.y, app_df$realm.x)
app_df$biome <- ifelse(is.na(app_df$biome.x), app_df$biome.y, app_df$biome.x)




## add storing data

storing <- read_sheet(my_sheet, sheet = "file_status")


app_df$status <- NULL


app_df$digitalObjectIdentifier <- gsub("https://doi.org/", "", app_df$digitalObjectIdentifier)

app_df$digitalObjectIdentifier <- gsub("/", "_", app_df$digitalObjectIdentifier)
app_df$digitalObjectIdentifier <- str_to_lower(app_df$digitalObjectIdentifier)
storing$doi <- str_to_lower(storing$doi)


app_df <- merge(app_df, storing[c("doi", "status")], by.y = "doi",
              by.x = "digitalObjectIdentifier", all.x = TRUE, all.y = TRUE)




#data$status[data$status==""] <- NA

app_df$status[is.na(app_df$status)] <- "PDF not yet saved"

############### add geographical info


shapename <- read_sf('C:/Users/Lenovo/Dropbox/fao_lit/FAO_lit/02_application/ne_50m_geography_marine_polys.shp')


app_df$geographicKeywords <- str_to_lower(app_df$geographicKeywords)

shapename$name <- str_to_lower(shapename$name)

regions <- unique(shapename$name)

###
# Function to find and return the first matching region
find_first_match <- function(keyword) {
  # If the keyword is NA, return NA without checking for matches
  if (is.na(keyword)) {
    return(NA)
  }
  for (region in regions) {
    if (str_detect(keyword, region)) {
      return(region)
    }
  }
  return(NA)  # Return NA if no match found
}
###

app_df <- app_df %>% 
  mutate(marine_polys = sapply(geographicKeywords, find_first_match))


table(app_df$marine_polys)


#### create an overview: 


overview <- data.frame(openASFA = nrow(openasfa_list),
                       proquest = nrow(proquest_list),
                       scopus = nrow(scopus),
                       wos = nrow(wos_list),
                       duplicates_title = nrow(duplicates),
                       duplicates_title = nrow(duplicates_doi),
                       NAs_taxonomic = sum(is.na(openasfa_list$taxonomicKeywords)),
                       NAs_geographic = sum(is.na(openasfa_list$geographicKeywords)),
                       NAs_marineRegion = sum(is.na(openasfa_list$marineRegionKeywords)),
                       NAs_ecosystem = sum(is.na(openasfa_list$ecosystemClassification)),
                       NAs_habitat = sum(is.na(app_df$habitat)),
                       NAs_restoration = sum(is.na(app_df$`restoration type addressed`)),
                       NAs_assessment = sum(is.na(app_df$`assessment type`)),
                       NAs_evaluation = sum(is.na(app_df$`restoration evaluation`)),
                       final_sample = nrow(app_df),
                       NAs_date = nrow(na_dates),
                       NAs_realm = sum(is.na(app_df$realm)),
                                       NAs_biome = sum(is.na(app_df$biome)))

write.xlsx(overview, "overview.xlsx")



app_df <- app_df %>% select(
  titles,
  authors,
  affiliationBroad,
  affiliationNarrow,
  dates,
  digitalObjectIdentifier,
  countryFirstauthor,
  publishers,
  marine_polys,
  documentType,
  summaries, 
  status,
  keywords, 
  geographicKeywords,
  taxonomicKeywords,
  marineRegionKeywords,
  environmentalRegimes,
  ecosystemClassification,
  habitat,
  `restoration evaluation`,
  `restoration type addressed`, 
  `assessment type`,
  languages,
  collection,
  city,
  urls,
  realm,
  biome

)




setwd("C:/Users/Lenovo/Dropbox/fao_lit/FAO_lit/02_application/")


write.xlsx(app_df, "app_df.xlsx")
write.csv(app_df, "app_df.csv")


sheet_write(app_df, ss = "https://docs.google.com/spreadsheets/d/1HgqxMdcz_YqxOgCIrapNC1h-cSGALLxUgu4PDrG6eoc/edit#gid=1380712147",
            sheet = "app_df")





app_df <- app_df %>% left_join(world.cities, by = c("city" = "name"))

app_df$country_from_cities <- app_df$country.etc
app_df$country_from_cities <- ifelse(app_df$city == "Washington", "USA", app_df$country_from_cities)

filtered_data$country <- coalesce(filtered_data$country.etc, filtered_data$countryFirstauthor)
      
      
      world <- ne_countries(scale = "medium", returnclass = "sf")
      world$sovereignt <- str_to_lower(world$sovereignt)
      countries_list <- unique(world$sovereignt)


data$Abstract <- data$summaries

data$url <- paste0("<a href='",data$urls,"'>",
                   data$titles,
                   "</a>")




data$DOI <- paste0("<a href='",data$urls,"'>",
                   data$digitalObjectIdentifier,
                   "</a>")


data$Title <- paste0(data$url)
data$Literature <- paste0("Abstract: ", data$Abstract)


data$publishers <- str_to_title(data$publishers)
data$publishers <- str_replace(data$publishers, "&", "And")    
data$publishers <- str_replace(data$publishers, "-", ": ")    





words <- data %>% filter(!is.na(keywords)) %>%
  select(keywords) %>% 
  as.list()


words <- words$keywords %>% str_split(", ") %>%
  unlist()

words <- trimws(words, which = c("left"))
words <- trimws(words, which = c("right"))

words <- stri_enc_toutf8(words)
words <- gsub("(?<=\\s|^)[^-\\s]{1,2}(?=\\s|$)", "", words, perl = TRUE)


words <- words %>% unique() %>% 
  sort()

### ecosystem classification

data$ecosystemClassification <- data$realm

data$ecosystemClassification <- str_to_lower(data$ecosystemClassification)

ecosystems <- data %>% filter(!is.na(ecosystemClassification)) %>%
  select(ecosystemClassification) %>% 
  as.list()


ecosystems <- ecosystems$ecosystemClassification %>% str_split(", ") %>%
  unlist()

#ecosystems <- gsub("c(", "", ecosystems)
ecosystems <- gsub("\\s*\\([^\\)]+\\)", "", as.character(ecosystems))


ecosystems <- trimws(ecosystems, which = c("left"))
ecosystems <- trimws(ecosystems, which = c("right"))

ecosystems <- stri_enc_toutf8(ecosystems)


ecosystems <- ecosystems %>% unique() %>% 
  sort()




### prepare habitat list
data$habitat <- data$biome

data$habitat <- str_to_lower(data$habitat)

habitats <- data %>% filter(!is.na(habitat)) %>%
  select(habitat) %>% 
  as.list()


habitats$habitat <- gsub("\n", "", habitats$habitat)

habitats <- habitats$habitat %>% str_split(", ") %>%
  unlist()

habitats <- trimws(habitats, which = c("left"))
habitats <- trimws(habitats, which = c("right"))

habitats <- stri_enc_toutf8(habitats)


habitats <- habitats %>% unique() %>% 
  sort()

# merge with marine region coordinates

shapename <- read_sf('ne_50m_geography_marine_polys.shp')


shapename$name <- str_to_lower(shapename$name)

# Assuming 'data' and 'shapename' are your dataframes
# and 'name' is the common column in both dataframeshttp://127.0.0.1:22527/graphics/plot_zoom_png?width=1200&height=879

data <- merge(data, shapename[c("name", "geometry")], by.y = "name",
              by.x = "marine_polys", all.x = TRUE)

table(data$marine_polys)

### marine regions

data$marineRegionKeywords

marine_polys <- data %>% filter(!is.na(marine_polys)) %>%
  select(marine_polys) %>% 
  as.list()


marine_polys$marine_polys <- gsub("\n", "", marine_polys$marine_polys)

marine_polys <- marine_polys$marine_polys %>% str_split(", ") %>%
  unlist()

marine_polys <- trimws(marine_polys, which = c("left"))
marine_polys <- trimws(marine_polys, which = c("right"))

marine_polys <- stri_enc_toutf8(marine_polys)


marine_polys <- marine_polys %>% unique() %>% 
  sort()


# document type

data$documentType <- str_to_lower(data$documentType)





### add "all" rows 
#data <- data %>% add_row(topic = "All")



