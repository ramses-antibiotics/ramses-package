## code to prepare `data/reference_loinc.rda dataset
library(dplyr)
library(magrittr)
loinc_text <- unz(grep("Loinc.*Text.*.zip$", 
                       list.files("data-raw", full.names = "T"), 
                       value = T), 
                  "Loinc.csv")
loinc_text <- utils::read.csv(loinc_text, stringsAsFactors = F)
reference_loinc <- filter(
  loinc_text,
  LOINC_NUM %in% c(
    "9279-1", 	# Respiratory rate
    "8867-4", 	# Heart rate
    "2708-6", 	# Oxygen saturation in Arterial blood
    "8310-5", 	# Body temperature
    "8302-2", 	# Body height
    "29463-7", 	# Body weight
    "39156-5", 	# Body mass index (BMI) [Ratio]
    "8480-6", 	# Systolic blood pressure
    "8462-4", 	# Diastolic blood pressure
    "8478-0", 	# Mean blood pressure 
    "6690-2", 	# Leukocytes [#/volume] in Blood by Automated count
    "789-8",    # Erythrocytes [#/volume] in Blood by Automated count
    "718-7",    # Hemoglobin [Mass/volume] in Blood
    "4544-3", 	# Hematocrit [Volume Fraction] of Blood by Automated count
    "787-2", 	 	# MCV [Entitic volume] by Automated count
    "785-6", 	 	# MCH [Entitic mass] by Automated count
    "786-4", 	 	#	MCHC [Mass/volume] by Automated count
    "21000-5", 	#	Erythrocyte distribution width [Entitic volume] by Automated count
    "788-0", 	 	#	Erythrocyte distribution width [Ratio] by Automated count
    "777-3", 	 	#	Platelets [#/volume] in Blood by Automated count
    "32207-3", 	#	Platelet distribution width [Entitic volume] in Blood by Automated count
    "32623-1", 	#	Platelet mean volume [Entitic volume] in Blood by Automated count
    "751-8", 		# Neutrophils [#/volume] in Blood by Automated count
    "30229-9", 	# Band form neutrophils [#/volume] in Blood by Automated count
    "731-0", 		# Lymphocytes [#/volume] in Blood by Automated count
    "43743-4", 	# Variant lymphocytes [#/volume] in Blood by Automated count
    "742-7", 		# Monocytes [#/volume] in Blood by Automated count
    "711-2", 		# Eosinophils [#/volume] in Blood by Automated count
    "704-7", 		# Basophils [#/volume] in Blood by Automated count
    "51383-8", 	# Leukocytes other [#/volume] in Blood by Automated count
    "58443-3", 	# Other cells [#/volume] in Blood by Automated count
    "1988-5", 	# C reactive protein [Mass/volume] in Serum or Plasma
    "30341-2", 	# Erythrocyte sedimentation rate
    "77144-4",  # Alanine aminotransferase [Enzymatic activity/volume] in Serum, Plasma or Blood
    "77141-0",  # Alkaline phosphatase [Enzymatic activity/volume] in Serum, Plasma or Blood
    "77137-8",  # Bilirubin.total [Moles/volume] in Serum, Plasma or Blood
    "35088-4",  # Glasgow coma scale
    "9267-6",   # Glasgow coma score eye opening
    "9268-4",   # Glasgow coma score motor
    "9269-2",   # Glasgow coma score total
    "9270-0",   # Glasgow coma score verbal
    "77140-2",  # Creatinine [Moles/volume] in Serum, Plasma or Blood
    "9187-6",   # Urine output
    "3167-4",   # Volume of 24 hour Urine
    "9192-6",   # Urine output 24 hour
    "9195-9",   # Urine output by in and out urethral catheter
    "1751-7",   # Albumin [Mass/volume] in Serum or Plasma
    "9108-2",   #	Fluid intake total 24 hour
    "9262-7",   # Fluid output total 24 hour
    "2823-3",   # Potassium [Moles/volume] in Serum or Plasma
    "2345-7", 	# Glucose [Mass/volume] in Serum or Plasma
    "3094-0", 	# Urea nitrogen [Mass/volume] in Serum or Plasma
    "2160-0", 	# Creatinine [Mass/volume] in Serum or Plasma
    "3097-3", 	# Urea nitrogen/Creatinine [Mass Ratio] in Serum or Plasma
    "33914-3", 	# Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum or Plasma by Creatinine-based formula (MDRD)
    "50044-7", 	# Glomerular filtration rate/1.73 sq M.predicted among females [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)
    "48642-3", 	# Glomerular filtration rate/1.73 sq M.predicted among non-blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)
    "48643-1", 	# Glomerular filtration rate/1.73 sq M.predicted among blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)
    "62238-1", 	# Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI)
    "88293-6", 	# Glomerular filtration rate/1.73 sq M.predicted among blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI)
    "88294-4", 	# Glomerular filtration rate/1.73 sq M.predicted among non-blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI)
    "17863-2", 	# Calcium.ionized [Mass/volume] in Serum or Plasma
    "11558-4", 	# pH of Blood
    "33254-4", 	# pH of Arterial blood adjusted to patient's actual temperature
    "11557-6", 	# Carbon dioxide [Partial pressure] in Blood
    "34705-4", 	# Carbon dioxide [Partial pressure] adjusted to patient's actual temperature in Blood
    "11556-8", 	# Oxygen [Partial pressure] in Blood
    "19254-2", 	# Oxygen [Partial pressure] adjusted to patient's actual temperature in Blood
    "57800-5", 	# Oxygen content in Blood by calculation
    "1959-6", 	# Bicarbonate [Moles/volume] in Blood
    "20565-8", 	# Carbon dioxide, total [Moles/volume] in Blood
    "11555-0", 	# Base excess in Blood by calculation
    "30318-0", 	# Base deficit in Blood
    "20564-1", 	# Oxygen saturation in Blood
    "2713-6", 	# Oxygen saturation Calculated from oxygen partial pressure in Blood
    "718-7", 	  # Hemoglobin [Mass/volume] in Blood
    "3150-0", 	# Inhaled oxygen concentration
    "3151-8" 	# Inhaled oxygen flow rate
  )) %>% 
  arrange(CLASS, SYSTEM, LOINC_NUM)

usethis::use_data(reference_loinc, overwrite = T)
 