## NEMO - Phenology 
## 1 Import data from Access 
## 2 Make data frame using SQL (# Alternative 1)
## 3 Estimate  hatching dates +/- uncertainty
## 4 

# Install/load packages
install.packages("odbc") # contains drivers to connect to a database
install.packages("DBI") # contains functions for interacting with the database

library(odbc)
library(DBI)
library(tidyverse)
library(dplyr)

################################################################################
## 1 Import data from Access
################################################################################

dbname <- "C:/Users/lea.lipphardt/Desktop/ENGASJEMENT_SEAPOP/NEMO/Spitsbergen_2024.mdb"

## Connect to the Access database using ODBC
con <- dbConnect(odbc::odbc(), 
                 .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dbname))

## List tables in the database
#dbListTables(con)
#dbListFields(con, "NestContent")

################################################################################
## 2 make data frame using SQL # Alternative 1
################################################################################

query <- "
SELECT s.spcENG, n.Locality, n.Area, n.NestNumber, v.VisitDate, nc.NestContentText, v.NestClutch
FROM (((Visits AS v
INNER JOIN NestContent AS nc ON v.NestStatus = nc.NestContentID)
INNER JOIN Nest AS n ON n.NestUniqueID = v.NestUniqueID)
INNER JOIN Species AS s ON n.Species = s.EUnr)
"
results <- dbGetQuery(con, query)

head(results)
str(results)


## Close the connection
dbDisconnect(con) # disconnect after your work is done to free resources

################################################################################
## 3 Estimate  dates +/- uncertanty

# Filter for manual check
# Make a table with species, year, locality and the following information: 
# Get lastE and firstC
# Estimate hatcing date
# Estimate uncertainty

################################################################################
# Control (no need to run this section)
df_selected <- results
nrow(df_selected)

# Subset
#df_selected <- results[1:1000, ]

# Count the number of occurrences of each unique character in NestClutch
table(results$NestClutch)

# Convert df_selected into a tibble and summarize with dplyr
results %>%
  as_tibble() %>%
  count(NestClutch)

################################################################################

# Convert VisitDate to Date format if it's not already
df_selected$VisitDate <- as.Date(df_selected$VisitDate)

# Extract year from VisitDate (makes it easier to iterate)
df_selected$Year <- format(df_selected$VisitDate, "%Y")

################################################################################

# Subset the data for a specific year, specie, location
df_selected_year <- subset(df_selected, Year == "2024")
#df_selected_specie <- subset(df_selected_year, spcENG == "Black-legged kittiwake")
#df_selected_location <- subset(df_selected_specie, Locality == "Ossian Sarsfjellet")


# Season Svarthamaren, Antarctica HERE ! :) :)xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


################################################################################
# Apply note to only egg/chick/empty entries and makes a note in a separate column 
filter_clutch_values <- function(df) {
  # Define valid clutch values for different categories
  egg_clutch_values <- c("1E", "2E", "3E", "1e", "2e", "3e", "1e,1E", "2e,2E", "3e,3E")
  chick_clutch_values <- c("1C", "2C", "3C", "1c", "2c", "3c", "1c,1C", "2c,2C", "3c,3C")
  empty_clutch_values <- c("")
  
  # Initialize the Note column with empty strings
  df$Note <- ""
  
  # Iterate over unique NestNumbers and Years
  for (nest in unique(df$NestNumber)) {
    for (year in unique(df$Year)) {
      # Subset the data for the current NestNumber and Year
      subset_df <- df[df$NestNumber == nest & df$Year == year, ]
      
      # Remove rows with NA values in NestClutch
      subset_df <- subset_df[!is.na(subset_df$NestClutch), ]
      
      # Check conditions for E, C, or empty entries and append notes
      if (all(subset_df$NestClutch %in% egg_clutch_values)) {
        df$Note[df$NestNumber == nest & df$Year == year] <- "Only E-entries"
      } else if (all(subset_df$NestClutch %in% chick_clutch_values)) {
        df$Note[df$NestNumber == nest & df$Year == year] <- "Only C-entries"
      } else if (all(subset_df$NestClutch %in% empty_clutch_values)) {
        df$Note[df$NestNumber == nest & df$Year == year] <- "Only empty-entries"
      }
    }
  }
  
  return(df)
}

df_selected_year <- filter_clutch_values(df_selected_year)

head(df_selected_year)

################################################################################
################################################################################
# Last_E 
# First_C

#  date 
# +/- uncertainty /  date accuracy 
################################################################################

# Find first visit with egg
find_first_E <- function(df) {
  # Define the clutch values to search for
  clutch_values <- c("1E", "1e", "2E", "2e", "3E", "3e","1E,1e", "1E,2E", "1E,2e",
                     "1E,3E", "1E,3e","1e,2E", "1e,2e", "1e,3E", "1e,3e", "2E,1E",
                     "2E,1e", "2E,2e", "2E,3E", "2E,3e", "2e,1E", "2e,1e", "2e,2E",
                     "2e,3E", "2e,3e", "3E,1E", "3E,1e", "3E,2E", "3E,2e", "3E,3e",
                     "3e,1E", "3e,1e", "3e,2E", "3e,2e", "3e,3E")
  
  # Create a new column `First_E` initialized with NA
  df$First_E <- as.Date(NA)
  
  # Iterate over unique NestNumbers and Years
  for (nest in unique(df$NestNumber)) {
    for (year in unique(df$Year)) {
      # Subset the data for current NestNumber and Year
      subset_df <- df[df$NestNumber == nest & df$Year == year, ]
      
      # Ensure VisitDate is in Date format
      if (!inherits(subset_df$VisitDate, "Date")) {
        subset_df$VisitDate <- as.Date(subset_df$VisitDate)
      }
      
      # Find the first occurrence of any specified clutch value
      matching_clutches <- subset_df[subset_df$NestClutch %in% clutch_values, ]
      
      if (nrow(matching_clutches) > 0) {
        # Get the first matching row based on VisitDate
        first_entry <- matching_clutches[which.min(matching_clutches$VisitDate), ]
        
        # Assign the First_E value to all rows for this NestNumber and Year
        df[df$NestNumber == nest & df$Year == year, "First_E"] <- first_entry$VisitDate
      }
    }
  }
  
  return(df)
}

# Find last visit with egg 
find_last_E <- function(df) {
  # Define the clutch values to search for
  clutch_values <- c("1E", "1e", "2E", "2e", "3E", "3e","1E,1e", "1E,2E", "1E,2e",
                     "1E,3E", "1E,3e","1e,2E", "1e,2e", "1e,3E", "1e,3e", "2E,1E",
                     "2E,1e", "2E,2e", "2E,3E", "2E,3e", "2e,1E", "2e,1e", "2e,2E",
                     "2e,3E", "2e,3e", "3E,1E", "3E,1e", "3E,2E", "3E,2e", "3E,3e",
                     "3e,1E", "3e,1e", "3e,2E", "3e,2e", "3e,3E")
  
  # Create a new column `Last_E` initialized with NA
  df$Last_E <- as.Date(NA)
  
  # Iterate over unique NestNumbers and Years
  for (nest in unique(df$NestNumber)) {
    for (year in unique(df$Year)) {
      # Subset the data for current NestNumber and Year
      subset_df <- df[df$NestNumber == nest & df$Year == year, ]
      
      # Find the last occurrence of any specified clutch value
      matching_clutches <- subset_df[subset_df$NestClutch %in% clutch_values, ]
      
      if (nrow(matching_clutches) > 0) {
        # Get the last matching row based on VisitDate
        last_entry <- matching_clutches[which.max(matching_clutches$VisitDate), ]
        
        # Assign the Last_E value to all rows for this NestNumber and Year
        df[df$NestNumber == nest & df$Year == year, "Last_E"] <- last_entry$VisitDate
      }
    }
  }
  
  return(df)
}

# Find first visit with chick
find_first_C <- function(df) {
  # Define the clutch values to search for
  clutch_values <- c("1C", "2C", "3C", "1c", "2c", "3c","1C,1c", "1C,2C", "1C,2c", "1C,3C", "1C,3c", 
                     "1C,1E", "1C,1e", "1C,2E", "1C,2e", "1C,3E", "1C,3e", "1c,1C", "1c,2C", "1c,2c",
                     "1c,3C", "1c,3c", "1c,1E", "1c,1e", "1c,2E", "1c,2e", "1c,3E", "1c,3e", "2C,1C",
                     "2C,1c", "2C,2c", "2C,3C", "2C,3c", "2C,1E", "2C,1e", "2C,2E", "2C,2e", "2C,3E",
                     "2C,3e", "2c,1C", "2c,1c", "2c,2C", "2c,3C", "2c,3c", "2c,1E", "2c,1e", "2c,2E",
                     "2c,2e", "2c,3E", "2c,3e", "3C,1C", "3C,1c", "3C,2C", "3C,2c", "3C,3c", "3C,1E",
                     "3C,1e", "3C,2E", "3C,2e", "3C,3E", "3C,3e", "3c,1C", "3c,1c", "3c,2C", "3c,2c",
                     "3c,3C", "3c,1E", "3c,1e", "3c,2E", "3c,2e", "3c,3E", "3c,3e", "1E,1C", "1E,1c",
                     "1E,2C", "1E,2c", "1E,3C", "1E,3c", "1e,1C", "1e,1c", "1e,2C", "1e,2c", "1e,3C",
                     "1e,3c", "2E,1C", "2E,1c", "2E,2C", "2E,2c", "2E,3C", "2E,3c", "2e,1C", "2e,1c",
                     "2e,2C", "2e,2c", "2e,3C", "2e,3c", "3E,1C", "3E,1c", "3E,2C", "3E,2c", "3E,3C",
                     "3E,3c", "3e,1C", "3e,1c", "3e,2C", "3e,2c", "3e,3C", "3e,3c")
  
  # Create a new column `First_C` initialized with NA
  df$First_C <- as.Date(NA)
  
  # Iterate over unique NestNumbers and Years
  for (nest in unique(df$NestNumber)) {
    for (year in unique(df$Year)) {
      # Subset the data for current NestNumber and Year
      subset_df <- df[df$NestNumber == nest & df$Year == year, ]
      
      # Find the first occurrence of any specified clutch value
      matching_clutches <- subset_df[subset_df$NestClutch %in% clutch_values, ]
      
      if (nrow(matching_clutches) > 0) {
        # Get the first matching row based on VisitDate
        first_entry <- matching_clutches[which.min(matching_clutches$VisitDate), ]
        
        # Assign the First_C value to all rows for this NestNumber and Year
        df[df$NestNumber == nest & df$Year == year, "First_C"] <- first_entry$VisitDate
      }
    }
  }
  
  return(df)
}

# Find last visit with chick 
find_last_C <- function(df) {
  # Define the clutch values to search for
  clutch_values <- c("1C", "2C", "3C", "1c", "2c", "3c","1C,1c", "1C,2C", "1C,2c", "1C,3C", "1C,3c", 
                     "1C,1E", "1C,1e", "1C,2E", "1C,2e", "1C,3E", "1C,3e", "1c,1C", "1c,2C", "1c,2c",
                     "1c,3C", "1c,3c", "1c,1E", "1c,1e", "1c,2E", "1c,2e", "1c,3E", "1c,3e", "2C,1C",
                     "2C,1c", "2C,2c", "2C,3C", "2C,3c", "2C,1E", "2C,1e", "2C,2E", "2C,2e", "2C,3E",
                     "2C,3e", "2c,1C", "2c,1c", "2c,2C", "2c,3C", "2c,3c", "2c,1E", "2c,1e", "2c,2E",
                     "2c,2e", "2c,3E", "2c,3e", "3C,1C", "3C,1c", "3C,2C", "3C,2c", "3C,3c", "3C,1E",
                     "3C,1e", "3C,2E", "3C,2e", "3C,3E", "3C,3e", "3c,1C", "3c,1c", "3c,2C", "3c,2c",
                     "3c,3C", "3c,1E", "3c,1e", "3c,2E", "3c,2e", "3c,3E", "3c,3e", "1E,1C", "1E,1c",
                     "1E,2C", "1E,2c", "1E,3C", "1E,3c", "1e,1C", "1e,1c", "1e,2C", "1e,2c", "1e,3C",
                     "1e,3c", "2E,1C", "2E,1c", "2E,2C", "2E,2c", "2E,3C", "2E,3c", "2e,1C", "2e,1c",
                     "2e,2C", "2e,2c", "2e,3C", "2e,3c", "3E,1C", "3E,1c", "3E,2C", "3E,2c", "3E,3C",
                     "3E,3c", "3e,1C", "3e,1c", "3e,2C", "3e,2c", "3e,3C", "3e,3c")
  
  # Create a new column `Last_C` initialized with NA
  df$Last_C <- as.Date(NA)
  
  # Iterate over unique NestNumbers and Years
  for (nest in unique(df$NestNumber)) {
    for (year in unique(df$Year)) {
      # Subset the data for current NestNumber and Year
      subset_df <- df[df$NestNumber == nest & df$Year == year, ]
      
      # Find the last occurrence of any specified clutch value
      matching_clutches <- subset_df[subset_df$NestClutch %in% clutch_values, ]
      
      if (nrow(matching_clutches) > 0) {
        # Get the last matching row based on VisitDate
        last_entry <- matching_clutches[which.max(matching_clutches$VisitDate), ]
        
        # Assign the Last_E value to all rows for this NestNumber and Year
        df[df$NestNumber == nest & df$Year == year, "Last_C"] <- last_entry$VisitDate
      }
    }
  }
  
  return(df)
}


find_last_visit <- function(df) {
  # Create new columns initialized with NA
  df$Last_Visit <- as.Date(NA)
  df$LastStatus_All <- NA
  
  # Iterate over unique NestNumbers and Years
  for (nest in unique(df$NestNumber)) {
    for (year in unique(df$Year)) {
      # Subset the data for the current NestNumber and Year
      subset_df <- df[df$NestNumber == nest & df$Year == year, ]
      
      # Find the last visit based on VisitDate
      if (nrow(subset_df) > 0) {
        last_entry <- subset_df[which.max(subset_df$VisitDate), ]
        
        # Assign the Last_Visit and LastStatus_All values to all rows for this NestNumber and Year
        df[df$NestNumber == nest & df$Year == year, "LastOfVisitDate"] <- last_entry$VisitDate
        df[df$NestNumber == nest & df$Year == year, "LastStatus_All"] <- last_entry$NestContentText
      }
    }
  }
  
  return(df)
}


phenology <- df_selected_year %>%
  find_first_E() %>%
  find_last_E() %>%
  find_first_C() %>%
  find_last_C() %>%
  find_last_visit()

# View the result
head(phenology)


# Initialize new columns for hatching date, its accuracy, and hatching success
phenology <- phenology %>%
  mutate(
    # Calculate Hatching_date as the mean of Last_E and First_C
    Hatching_date = if_else(
      !is.na(Last_E) & !is.na(First_C),
      as.Date((as.numeric(Last_E) + as.numeric(First_C)) / 2, origin = "1970-01-01"),
      NA_Date_
    ),
    
    # Calculate Hatching_date_accuracy as half the difference in days
    Hatching_date_accuracy = if_else(
      !is.na(Last_E) & !is.na(First_C),
      (as.numeric(First_C - Last_E) / 2),
      NA_real_
    ),
    
    # Set June 1st of the given year as Julian Date 1
    JulianHatchingDate = if_else(
      !is.na(Hatching_date),
      {
        # Extract the year from the Hatching_date
        year_start <- as.Date(paste0(format(Hatching_date, "%Y"), "-06-01"))
        as.numeric(Hatching_date - year_start + 1) # Julian Date calculation
      },
      NA_real_
    ),
    
    # Assign HatchingSuccess as 1 if Hatching_date is not NA, otherwise NA
    HatchingSuccess = if_else(
      !is.na(Hatching_date),
      1,
      NA_real_
    )
  )

################################################################################

# Alternative 1

################################################################################

#phenology$First_C <- as.Date(phenology$First_C)
#phenology$Last_C <- as.Date(phenology$Last_C) 

# Assuming that First_C and Last_C are dates if not run the # above
NbChickPresence <- function(df) {
  # Create new column for the number of days between First_C/Hatching_date and Last_C
  df$NbChickPresence <- NA
  df$NbDays_ChickPresence_FromHD <- NA
  # Difference between last visit with chick and last visit
  df$Diff_LastVisitWithChick_LastVisit <- NA
  
  # Iterate over unique NestNumbers and Years
  for (nest in unique(df$NestNumber)) {
    for (year in unique(df$Year)) {
      # Subset the data for the current NestNumber and Year
      subset_df <- df[df$NestNumber == nest & df$Year == year, ]
      
      # Extract First_C and Last_C (assumed to be consistent within the subset)
      first_c <- subset_df$First_C[1]  # Take the first occurrence
      last_c <- subset_df$Last_C[1]   
      hatching_date <- subset_df$Hatching_date[1] 
      last_visit <- subset_df$LastOfVisitDate[1]
      
      # Calculate the number of days if both dates are not NA
      if (!is.na(first_c) && !is.na(last_c)) {
        # Calculate the difference in days (ensure the columns are Date type)
        days_with_chicks <- as.numeric(as.Date(last_c) - as.Date(first_c) + 1)  # Include both days
        
        # Assign the calculated value to all rows of the current subset
        df[df$NestNumber == nest & df$Year == year, "NbChickPresence"] <- days_with_chicks
      } else {
        # If any date is NA, leave NbChickPresence as NA
        df[df$NestNumber == nest & df$Year == year, "NbChickPresence"] <- NA
      }
      
      # Calculate the number of days if both dates are not NA
      if (!is.na(hatching_date) && !is.na(last_c)) {
        # Calculate the difference in days (ensure the columns are Date type)
        days_with_chicks_FromHB <- as.numeric(as.Date(last_c) - as.Date(hatching_date) + 1)  # Include both days
        
        # Assign the calculated value to all rows of the current subset
        df[df$NestNumber == nest & df$Year == year, "NbDays_ChickPresence_FromHD"] <- days_with_chicks_FromHB
      } else {
        # If any date is NA, leave NbDays_ChickPresence_FromHD as NA
        df[df$NestNumber == nest & df$Year == year, "NbDays_ChickPresence_FromHD"] <- NA
      }
      # Calculate the number of days if both dates are not NA
      if (!is.na(last_c) && !is.na(last_visit)) {
        # Calculate the difference in days (ensure the columns are Date type)
        diff_days <- as.numeric(as.Date(last_visit) - as.Date(last_c) + 1)  # Include both days
        
        # Assign the calculated value to all rows of the current subset
        df[df$NestNumber == nest & df$Year == year, "Diff_LastVisitWithChick_LastVisit"] <- diff_days
      } else {
        # If any date is NA, leave Diff_LastVisitWithChick_LastVisit as NA
        df[df$NestNumber == nest & df$Year == year, "Diff_LastVisitWithChick_LastVisit"] <- NA
      }
    }
  }
  
  return(df)
}

################################################################################

# Alternative 2

################################################################################

NbChickPresence <- function(df) {
  # Ensure First_C, Last_C, Hatching_date, and LastOfVisitDate are Date objects
  df <- df %>%
    rowwise() %>%
    mutate(
      First_C = as.Date(First_C),
      Last_C = as.Date(Last_C),
      Hatching_date = as.Date(Hatching_date),
      LastOfVisitDate = as.Date(LastOfVisitDate)
    )
  
  # Group by NestNumber and Year and calculate required metrics
  df <- df %>%
    group_by(NestNumber, Year) %>%
    mutate(
      NbChickPresence = ifelse(!is.na(First_C) & !is.na(Last_C), as.numeric(Last_C - First_C + 1), NA),
      NbDays_ChickPresence_FromHD = ifelse(!is.na(Hatching_date) & !is.na(Last_C), as.numeric(Last_C - Hatching_date + 1), NA),
      Diff_LastVisitWithChick_LastVisit = ifelse(!is.na(Last_C) & !is.na(LastOfVisitDate), as.numeric(LastOfVisitDate - Last_C + 1), NA)
    ) %>%
    ungroup() # Ungroup to finalize the operation
  
  return(df)
}

# Apply the function
phenology <- NbChickPresence(phenology)

################################################################################

# Chick survival
phenology <- phenology %>%
  rowwise() %>%
  mutate(
    Note = if_else(Hatching_date_accuracy > 5, "Hatching_date_accuracy > 5", Note),
    Note = if_else(NbDays_ChickPresence_FromHD < 15, "Entries for NbDays_ChickPresence_FromHD < 15  ", Note),
    Chick_survival = if_else(Hatching_date_accuracy < 5 & any(NbDays_ChickPresence_FromHD > 15), 1, 0),
    Breeding_success = Chick_survival
  )

# Select the columns of interest
phenology <- phenology %>%
  select(spcENG, Locality, Area, NestNumber, Year, Note, Last_E, First_C, Last_C, NbChickPresence, LastOfVisitDate, LastStatus_All, Hatching_date, JulianHatchingDate, Hatching_date_accuracy, NbDays_ChickPresence_FromHD, Diff_LastVisitWithChick_LastVisit, Chick_survival, Breeding_success)

view(phenology)

################################################################################

################################################################################

# False E
egg_clutch_values <- c("1E", "2E", "3E", "1e", "2e", "3e", "1e,1E", "2e,2E", "3e,3E")
chick_clutch_values <- c("1C", "2C", "3C", "1c", "2c", "3c", "1c,1C", "2c,2C", "3c,3C")

df <- df_selected_year %>%
  group_by(NestNumber, Year) %>%
  mutate(
    flag = if_else(
      (NestClutch %in% egg_clutch_values) & 
        (lag(NestClutch) %in% chick_clutch_values) & 
        (lag(NestClutch, 2) %in% chick_clutch_values) &
        (lag(VisitDate) < VisitDate) &
        (lag(VisitDate, 2) < VisitDate), 
      FALSE,
      TRUE
    )
  ) %>%
  filter(flag) %>%
  ungroup()
view(df)