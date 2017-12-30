# UBER Assignment Solution

# 1. Source Data into R from Uber data set (.csv file)

Uber_Request_Data <- read_csv("~/01 PGDDA ML DP/04 Assignments/02 UBER Case Study/Inputs/Uber Request Data.csv", na = "NA")

Uber_Request_Data_Duplicate <- Uber_Request_Data

# 2. Problem Statement

  # Business Understanding

    # UBER loses revenue for the cabs (to and fro from city to airport and viceversa) when
    # either cab driver cancels request or no availabilty of cabs upon customers request

    # Business Objectives of data analysis
      # 1. Identification of root cause through data analysis for the problem stated above
      # 2. Recommend ways to reduce the cancellation or non-availability of cabs scenario


  # Data Understanding

    # 1. Request id: A unique identifier of the request
    # 2. Time of request: The date and time at which the customer made the trip request
    # 3. Drop-off time: The drop-off date and time, in case the trip was completed
    # 4. Pick-up point: The point from which the request was made
    # 5. Driver id: The unique identification number of the driver
    # 6. Status of the request: The final status of the trip, that can be either completed,           #  cancelled by the driver or no cars available



# 3. Data Cleansing Activity
  # Step 1: Fixing Rows and Columns & Derive additional information
    # No Incorrect rows, Summary Rows, Extra Rows, Missing Column names, Inconsistent col names,     # No Unnecessary column names
    # No Misaligned columns
      str(Uber_Request_Data)
      View(Uber_Request_Data)  # Enabled to come to above stated conclusion

    # Fixing "Data Types" of each column
      Uber_Request_Data$`Pickup point` = as.factor(Uber_Request_Data$`Pickup point`)
      levels(Uber_Request_Data$`Pickup point`)
      Uber_Request_Data$Status = as.factor(Uber_Request_Data$Status)
      levels(Uber_Request_Data$Status)

      str(Uber_Request_Data)

    # Columns containing multiple data values & Converstion to Date & Time stamp formats
      library(lubridate)
      # Conversion of date and time into POSIXct format
      Uber_Request_Data$`Request timestamp` <- parse_date_time(Uber_Request_Data$`Request timestamp`, c("dmy_HMS", "dmy_HM"))
      Uber_Request_Data$`Drop timestamp` <- parse_date_time(Uber_Request_Data$`Drop timestamp`, c("dmy_HMS", "dmy_HM"))
      Uber_Request_Data$Travel_time_mins <- difftime(Uber_Request_Data$`Drop timestamp`,Uber_Request_Data$`Request timestamp`, units = "mins")
      Uber_Request_Data$Travel_time_mins <- round(Uber_Request_Data$Travel_time_mins, 0)
      Uber_Request_Data$Travel_time_secs = difftime(Uber_Request_Data$`Drop timestamp`,Uber_Request_Data$`Request timestamp`, units = "secs")

      # Extracting Date and Time into different columns and formatting date into right date format
      Uber_Request_Data$Request_Date <- format(Uber_Request_Data$`Request timestamp`, "%d-%m-%Y")
      Uber_Request_Data$Request_Time <- format(Uber_Request_Data$`Request timestamp`, "%H:%M:%S")
      Uber_Request_Data$Request_Date <- as.Date(Uber_Request_Data$Request_Date, "%d-%m-%Y")
      Uber_Request_Data$Request_Day  <- as.factor(weekdays(Uber_Request_Data$Request_Date))
        # Month and Year has only one value and hence, dont add any value for this data analysis
        # Uber_Request_Data$Request_Mon  <- as.factor(months(Uber_Request_Data$Request_Date))
        # Uber_Request_Data$Request_Yr   <- as.factor(format(Uber_Request_Data$Request_Date, "%Y"))
      Uber_Request_Data$Request_Hour <- as.numeric(format(Uber_Request_Data$`Request timestamp`, "%H"))
      Uber_Request_Data$Request_Mins <- as.numeric(format(Uber_Request_Data$`Request timestamp`, "%M"))
      Uber_Request_Data$Request_Sec <- as.numeric(format(Uber_Request_Data$`Request timestamp`, "%S"))

      Uber_Request_Data$Drop_Date <- format(Uber_Request_Data$`Drop timestamp`, "%d-%m-%Y")
      Uber_Request_Data$Drop_Time <- format(Uber_Request_Data$`Drop timestamp`, "%H:%M:%S")
      Uber_Request_Data$Drop_Date <- as.Date(Uber_Request_Data$Drop_Date, "%d-%m-%Y")
      Uber_Request_Data$Drop_Day  <- as.factor(weekdays(Uber_Request_Data$Drop_Date))
        # Month and Year has only one value and hence, dont add any value for this data analysis
        # Uber_Request_Data$Drop_Mon  <- as.factor(months(Uber_Request_Data$Drop_Date))
        # Uber_Request_Data$Drop_Yr   <- as.factor(format(Uber_Request_Data$Drop_Date, "%Y"))
      Uber_Request_Data$Drop_Hour <- as.numeric(format(Uber_Request_Data$`Drop timestamp`, "%H"))
      Uber_Request_Data$Drop_Mins <- as.numeric(format(Uber_Request_Data$`Drop timestamp`, "%M"))
      Uber_Request_Data$Drop_Sec <- as.numeric(format(Uber_Request_Data$`Drop timestamp`, "%S"))


    # Additional Derived Columns
      Uber_Request_Data$Rev_Lost_Trips <- as.factor(ifelse(Uber_Request_Data$Status == "Cancelled" | Uber_Request_Data$Status == "No Cars Available", "Y", "N"))

     # Uber_Request_Data$Driver_Availability <- ifelse((Uber_Request_Data$Drop_Hour - Uber_Request_Data$Request_Hour) == 0, "Y", "N") # If Driver completing trip within same hour, then he is available for next trip in the same hour

    # Remove Duplicates
      Uber_Request_Data <- unique(Uber_Request_Data)

      str(Uber_Request_Data)
      View(Uber_Request_Data)

# 4. Create a Dataset with Completed Trips, Cancelled Trips, No Cars Available

      Uber_Completed_Trips = subset(Uber_Request_Data, Uber_Request_Data$Status == "Trip Completed")
      Uber_Rev_Lost_Trips  = subset(Uber_Request_Data, Uber_Request_Data$Rev_Lost_Trips == "Y")
      Uber_Cancelled_Trips = subset(Uber_Request_Data, Uber_Request_Data$Status == "Cancelled")
      Uber_No_Cars_Available = subset(Uber_Request_Data, Uber_Request_Data$Status == "No Cars Available")

      library(xlsx)
      write.xlsx(Uber_Request_Data, "Uber_Cleansed_Requests.xlsx", sheetName="Requests")
      write.xlsx(Uber_Completed_Trips, "Uber_Completed_Trips.xlsx", sheetName="Completed Trips")
      write.xlsx(Uber_Rev_Lost_Trips, "Uber_Rev_Lost_Trips.xlsx", sheetName="Rev Lost Trips")
      write.xlsx(Uber_No_Cars_Available, "Uber_No_Cars.xlsx", sheetName="No Cars Trips")

# 5. Data Analysis

      library(ggplot2)
      summary(Uber_Request_Data)       # Enabled me to confirm that there aren't outliers and there are                                             # missing values but they are for trips cancelled and trips cars not                                         # available (Summation of Status - Cancelled + No cares is equivalent                                        # to NA's in Drop Timestamp and Reqeuest Timestamp)

      # No. of Trips by Pickup Point
      summary(Uber_Request_Data$Status)
      ggplot(Uber_Request_Data, aes(x = Status)) + geom_bar()
        # Outcome - No Cars Available is much bigger problem than Cancelled. However, Cancelled cars value is         # large and hence, need to address both issues

      # Analysis through plots are performed through Tableau - Refer Tableau Worksheet in ZIP file

