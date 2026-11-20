allowedLandingsVariable <- c(
  "CLoffWeight",
  "CLsciWeight"
)
usethis::use_data(allowedLandingsVariable, overwrite = TRUE)

allowedEffortVariable <- c(
  "CEnumFracTrips",
  "CEnumDomTrip",
  "CEoffDaySea",
  "CESciDaySea",
  "CEoffFishDay",
  "CEsciFishDay",
  "CEoffNumHaulSet",
  "CEsciNumHaulSet",
  "CEoffVesFishHour",
  "CEsciVesFishHour",
  "CEoffSoakMeterHour",
  "CEsciSoakMeterHour",
  "CEoffkWDaySea",
  "CEscikWDaySea",
  "CEoffkWFishDay",
  "CEscikWFishDay",
  "CEoffkWFishHour",
  "CEscikWFishHour",
  "CEgTDaySea",
  "CEgTFishDay",
  "CEgTFishHour"
)
usethis::use_data(allowedEffortVariable, overwrite = TRUE)

allowedSamplingVariable <- c(
  "SAsampWtLive",
  "SAnumSamp",
  "SAsampWtMes"
)
usethis::use_data(allowedSamplingVariable, overwrite = TRUE)

allowedCatchCat <- c(
  "Lan",
  "Dis",
  "Catch"
)
usethis::use_data(allowedCatchCat, overwrite = TRUE)


