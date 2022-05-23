# Define colors
exposureCol <- "#CDDC55"
outcomeCol <- "#4FBAE4"
adjustedCol <- "seagreen"
unadjustedCol <- "pink"
naCol <- 'grey80'

g1 <- 'dag {
    X [exposure,pos="0.000,0.000"]
    Y [outcome,pos="2.000,0.000"]
    Z [pos="1.000,1.000"]
    Z -> X
    Z -> Y
    X -> Y
}'

label1 <- c("X" = "Ice-cream sales", "Y" = "Sunburn", "Z" = "Sunshine")
sol1 <- c("Z")

g2 <- 'dag {
    X [exposure,pos="0.000,0.000"]
    Y [outcome,pos="2.000,0.000"]
    Z [pos="1.000,1.000"]
    X -> Z
    Z -> Y
    X -> Y
}'

label2 <- c("X" = "Exercise", "Y" = "Mood", "Z" = "Sleep quality")
sol2 <- NULL

# Example from https://rss.onlinelibrary.wiley.com/doi/10.1111/1740-9713.01413
g3 <- 'dag {
    X [exposure,pos="0.000,2.000"]
    Y [outcome,pos="0.000,0.000"]
    Z [pos="1.000,1.000"]
    X -> Z
    Y -> Z
    X -> Y
}'

label3 <- c("X" = "Smoking", "Y" = "COVID-19 Infection", "Z" = "Hospitalisation")
sol3 <- NULL

# From https://academic.oup.com/aje/article/176/10/938/92975
g4 <- 'dag {
Z1 [pos="0.000,1.000"]
Z2 [pos="2.000,1.000"]
Z3 [pos="1.000,0.500"]
X [exposure,pos="0.000,0.000"]
Y [outcome,pos="2.000,0.000"]
Z1 -> Z3
Z1 -> X
Z2 -> Z3
Z2 -> Y
X -> Y
}'

label4 <- c("X" = "SSRI use", "Y" = "Lung cancer", "Z1" = "Depression", "Z2" = "Ever smoker", "Z3" = "Coronary disease")
sol4 <- NULL

# From https://www.nature.com/articles/s41390-018-0071-3
g5 <- 'dag {
X [exposure,pos="1.000,1.000"]
Y [outcome,pos="3.000,1.000"]
Z1 [pos="0.000,3.000"]
Z2 [pos="2.000,0.000"]
Z3 [pos="2.000,-1.000"]
Z1 -> X
Z1 -> Y
X -> Z2
X -> Z3
Z2 -> Y
X -> Y
Y -> Z3
}'

label5 <- c("X" = "Screen time", "Y" = "Obesity", "Z1" = "Parental education", "Z2" = "Physical activity", "Z3" = "Self harm")
sol5 <- c("Z1")


# From https://doi.org/10.1016/j.chiabu.2019.02.011
g6 <- 'dag {
Z1 [pos="1.000,2.000"]
Z2 [pos="2.000,0.000"]
X [exposure,pos="0.000,3.000"]
Y [outcome,pos="2.000,3.000"]
X -> Z1
Z2 -> Z1
Z2 -> Y
Z1 -> Y
X -> Y
}'

label6 <- c("X" = "Childhood\nphysical abuse", "Y" = "Opioid\ndependency", "Z1" = "Chronic pain", "Z2" = "Unintentional\ninjury")
sol6 <- c("Z1", "Z2")

nExamples <- 6
