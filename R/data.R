#' epilepsy Data Set
#'
#' This data set contains information on the number of seizures, which 
#' treatment, and the age of 59 patients with epilepsy.
#'
#' @format A tibble with 59 rows and 4 variables: 
#' \describe{
#'   \item{id}{The patient ID.}
#'   \item{numseiz}{The number of seizures the patient had.}
#'   \item{age}{The age of the patient}
#' }
#'
"epilepsy"

#' cars99 Data Set
#'
#' This data set contains information on 109 vechicles from 1999.
#'
#' @format A tibble with 109 rows and 11 variables: 
#' \describe{
#'   \item{Model}{The vechicle model name.}
#'   \item{CityMPG}{The miles per gallon (MPG) for the vehicle in the city.}
#'   \item{HwyMPG}{The miles per gallon (MPG) for the vehicle on the highway}
#'   \item{FuelCap}{The fuel capacity of the vehicle in gallons.}
#'   \item{Weight}{The weight of the vehicle in lbs.}
#'   \item{FrontWt}{The front weight of the vehicle.}
#'   \item{Accel0_30}{The time it takes, in seconds, for the vehicle to 
#'   accelerate from 0 to 30 mph.}
#'   \item{Accel0_60}{The time it takes, in seconds, for the vehicle to 
#'   accelerate from 0 to 60 mph.}
#'   \item{QtrMile}{The time it takes, in seconds, for the vehicle to travel a 
#'   quarter of a mile.}
#' }
#'
"cars99"

#' class_data_f2019 Data Set
#'
#' This data set contains information on 42 students from Fall of 2019.
#'
#' @format A tibble with 42 rows and 7 variables: 
#' \describe{
#'   \item{level}{A factor indicating the class level the student is: Freshman, 
#'   Sophomore, Junior, Senior, or Graduate.}
#'   \item{major}{A character indicating the major of the student.}
#'   \item{sex}{A factor indicating the sex of the student: F for female or M
#'   for male.}
#'   \item{ski}{A factor indicating downhill preference: Ski, Snowboard, or 
#'   Neither.}
#'   \item{penny}{A factor indicating preference regarding the penny: Abolish,
#'   Retain or No Answer.}
#'   \item{speed}{An integer indicating the fastest speed the student had driven 
#'   in a vehicle (in mph).}
#'   \item{sleep}{A numeric variable indicating how long the student slept the 
#'   night before.}
#' }
#'
"class_data_f2019"

#' mlbsalaries Data Set
#'
#' This data set contains information on 877 MLB players from 2018.
#'
#' @format A tibble with 877 rows and 5 variables: 
#' \describe{
#'   \item{Rank}{The ranking of the player's salary with 1 being the most money 
#'   earned.}
#'   \item{Name}{A character vector containing the name of the player.}
#'   \item{Team}{A character vector containing the team the player played for.}
#'   \item{Position}{A factor indicating what position the player played: SP for
#'   starting pitcher, 1B for first base, 2B for second base, 3B for third base,
#'   SS for shortstop, OF for outfield, RP for relief pitcher, and DH for 
#'   designated hitter.}
#'   \item{Salary}{A numeric vector containing the salary the player made for 
#'   the 2018 season.}
#' }
#'
"mlbsalaries"

#' nitrogen Data Set
#'
#' Nitrogen content of trees in an orchard, the growing tips of 150 leaves are 
#' clipped from trees throughout the orchard. These leaves are ground to form 
#' one composite sample, which the researcher assays for percentage of nitrogen. 
#' Composite samples obtained from a random sample of 36 orchards throughout the
#' state gave the nitrogen contents.
#'
#' @format A data frame with 36 rows 1 variable: 
#' \describe{
#'   \item{nitrogen}{The nitrogen content for the trees.}
#' }
#'
"nitrogen"

#' births Data Set
#'
#' Data from 1995-1997 for a study hat examined pregnancies that resulted in the
#' birth of twins. Births were classified as preterm with intervention (induced
#' labor or cesarean), preterm without procedures, or term/post-term. 
#' Researchers also classified the pregnancies by the level of prenatal medical 
#' care the mother received (inadequate, adequate, or intensive). The data set 
#' consists of 278 cases (rows) with two columns indicating the level of 
#' prenatal care and type of birth for each set of twins.
#'
#' @format A tibble with 278 rows and 2 variables: 
#' \describe{
#'   \item{prenatal}{A factor indicating the prenatal care the mother received:
#'   Adequate, Inadequate, or Intensive.}
#'   \item{type}{A factor indicating the classification of the birth: "Preterm 
#'   (induced or cesarean)", "Preterm (without procedures)", and 
#'   "Term or post-term"}
#' }
#'
"births"

#' idealwt Data Set
#'
#' This data set contains weight information on 182 people (119 females and 63 
#' males). Actual weights, ideal weights, and the difference between them are 
#' recorded.
#'
#' @format A tibble with 182 rows and 4 variables: 
#' \describe{
#'   \item{sex}{A factor indicating the sex of the person: Female or Male.}
#'   \item{actual}{The person's actual weight.}
#'   \item{ideal}{The person's ideal weight.}
#'   \item{diff}{The difference between the person's actual weight and their 
#'   ideal weight. Negative values indicate that the person weighs less than
#'   what they consider ideal.}
#' }
#'
"idealwt"

#' diseases Data Set
#'
#' This data set contains information for each state and Washington, D.C. about
#' the number of reported cases of AIDS, syphilis, and tuberculosis.
#'
#' @format A tibble with 51 rows and 4 variables: 
#' \describe{
#'   \item{State}{A charcter vector indicating the state.}
#'   \item{AIDS}{The number of reporeted AIDS cases.}
#'   \item{Syphilis}{The number of reporeted syphilis cases.}
#'   \item{Tuberculosis}{The number of reporeted tuberculosis cases.}
#' }
#'
"diseases"

#' baseball Data Set
#'
#' This data set contains information for 337 baseball players.
#'
#' @format A tibble with 337 rows and 28 variables: 
#' \describe{
#'   \item{salary}{The salary in $1000s.}
#'   \item{average}{Batting average of the player.}
#'   \item{obp}{On base percentage of the player}
#'   \item{runs}{Number of runs scored.}
#'   \item{hits}{Number of hits in total.}
#'   \item{doubles}{Number of doubles hit.}
#'   \item{triples}{Number of triples hit.}
#'   \item{homeruns}{Number of homeruns hit.}
#'   \item{rbis}{Number of runs batted in.}
#'   \item{walks}{Number of times walked.}
#'   \item{sos}{Number of strikeouts.}
#'   \item{sbs}{Number of stolen bases.}
#'   \item{errors}{Number of errors committed.}
#'   \item{freeagent}{Factor indicating whether the player is a free agent or 
#'   is eligible for free agency.}
#'   \item{arbitration}{Factor indicating whether the player has arbitration or 
#'   is eligible for arbitration.}
#'   \item{runsperso}{Number of runs per strikeout (runs/sos).}
#'   \item{hitsperso}{Number of hits per strikeout (hits/sos).}
#'   \item{hrsperso}{Number of homeruns per strikeout (homeruns/sos).}
#'   \item{rbisperso}{Number of rbis per strikeout (rbis/sos).}
#'   \item{walksperso}{Number of walks per strikeout (walks/sos).}
#'   \item{obppererror}{On base percentage per error (obp/errors).}
#'   \item{runspererror}{Number of runs scored per error (runs/errors).}
#'   \item{hitspererror}{Number of hits per error (hits/errors).}
#'   \item{hrspererror}{Number of homeruns per error (homeruns/errors).}
#'   \item{sospererror}{Number of strikeouts per error (sos/errors).}
#'   \item{sbsobp}{Number of stolen bases times on base percentage (sbs*obp).}
#'   \item{sbsruns}{Number of stolen bases times number of runs scored
#'   (sbs*runs).}
#'   \item{sbshits}{Number of stolen bases times number of hits (sbs*hits).}
#' }
#'
"baseball"

# Left off after chapter 2 data files