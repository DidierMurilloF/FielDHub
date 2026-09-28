validateTreatments <- function(data) {
  # Group the data by LOCATION, REP, and then TREATMENT, and count the occurrences
  treatment_counts <- aggregate(ID ~ LOCATION + REP + ENTRY, data=data, FUN=length)

  # Identify any treatment counts greater than 1, indicating duplicates within a LOCATION and REP
  duplicates <- treatment_counts[treatment_counts$ID > 1, ]

  if (nrow(duplicates) > 0) {
    fieldhub_abort("There are duplicates within REP for some LOCATIONs:\n")
  }
}
