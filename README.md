# Relief_Tracker
This Github Repo stores the code and data behind ‘Relief_Tracker’, a Twitter account that Tweets out daily reminders about disasters affecting communities across the world. As of writing this, there are 15 disasters that the account tweets out every day.

# Hurricane Harvey 
# Hurricane Irma
# Hurricane Maria
# The Thomas Wildfire
# The Sierra Leone Mudslides
# The Mexico City Earthquake
# The Iran/Iraq Earthquake
# The Flint Water Crisis
# The Columbia Floods
# The Philippines Mudslides
# The Kadovar Island Volcano
# The On-going Mongolia Dzud

Code: 
There is one R code that pulls in, creates, and send out the Tweets for the account, which is “HandCurated_Tweets_Code_Git_Upload.R”. 
It is run once a day and sends Tweets out in 60 second increments.

Data:
There are five csv tables which encompass the data to include in the Tweet:
Metadata_Chart (CSV): this table includes the high level data about each disaster (Date of disaster, is it on-going, what hashtag should be used)
Started_Type (CSV): this table gives the correct preface when referencing the disaster start date.  If it’s an ongoing disaster, this allows the tweet to say that the disaster “Started” on it’s origin data, where as if it were a singular event, it will use the phrase “Occurred”.
Awareness_ (TXT): This table includes Tweets that are meant to raise awareness for a disaster as it has garnered little attention in the U.S (example: "the Sierra Leone mudslides".
Act_ (TXT): This table includes Tweets that are meant to mobilize action to support on-going relief efforts (example: "Hurricane Maria")
Focus_ (TXT): This table includes Tweets that are meant to remind us of existing disasters that have simply fallen from the news cycle (example: "Flint’s Water Crisis)”.
