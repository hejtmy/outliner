COLORS <- c(les = "#04724D", VR = "#540C97", neutral = "#FFC300")

condition_labeller <- c("les" = "Real forest", "VR" = "Virtual forest")

measures_labeller <- c(
  "sys" = "Systolic blood pressure", 
  "dia" = "Diastolic blood pressure",
  "pulse" = "Heart rate",
  "ros" = "Restoration outcome",
  "anger" = "POMS Anger",
  "vitality" = "POMS Vitality",
  "fatigue"= "POMS Fatigue",
  "confusion" = "POMS Confusion",
  "tension" = "POMS Tension",
  "depression" = "POMS Depression"
)

measures_forest_labeller <- c(
  "blood_sys" = "Systolic blood pressure", 
  "blood_dia" = "Diastolic blood pressure",
  "blood_pulse" = "Heart rate",
  "relax_ros" = "Restoration outcome",
  "POMS_anger" = "POMS Anger",
  "POMS_vitality" = "POMS Vitality",
  "POMS_fatigue" = "POMS Fatigue",
  "POMS_confusion" = "POMS Confusion",
  "POMS_tension" = "POMS Tension",
  "POMS_depression" = "POMS Depression"
)

when_labeller <- c(
  "pre" = "Before session score",
  "post" = "After session score",
  "diff" = "Difference between after and before session scores"
)

relabel <- function(x, labels) {
  labels[match(x, names(labels))]
}
