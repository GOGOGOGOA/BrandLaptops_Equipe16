dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$Price~as.factor(dataset$brand))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$Price~as.factor(dataset$primary_storage_type))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1


dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$Price~as.factor(dataset$OS))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$Price~as.factor(dataset$processor_tier))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$ram_memory~as.factor(dataset$brand))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$ram_memory~as.factor(dataset$primary_storage_type))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$ram_memory~as.factor(dataset$OS))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$ram_memory~as.factor(dataset$processor_tier))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$resolution_width~as.factor(dataset$brand))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$resolution_width~as.factor(dataset$primary_storage_type))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$resolution_width~as.factor(dataset$OS))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$resolution_width~as.factor(dataset$processor_tier))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$resolution_height~as.factor(dataset$brand))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$resolution_height~as.factor(dataset$primary_storage_type))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$resolution_height~as.factor(dataset$OS))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$resolution_height~as.factor(dataset$processor_tier))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$display_size~as.factor(dataset$brand))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$display_size~as.factor(dataset$primary_storage_type))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$display_size~as.factor(dataset$OS))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$display_size~as.factor(dataset$processor_tier))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$primary_storage_capacity~as.factor(dataset$brand))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$primary_storage_capacity~as.factor(dataset$primary_storage_type))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$primary_storage_capacity~as.factor(dataset$OS))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1

dataset <- read.csv("Laptop2024.csv")


model1 <- lm(dataset$primary_storage_capacity~as.factor(dataset$processor_tier))
summary(model1)
ANOVA1 <- anova(model1) 
print(ANOVA1)

#Variance Inter
var.inter1=ANOVA1$`Sum Sq`[1]/992 # inter
var.inter1

#Variance Intra
var.intra1=ANOVA1$`Sum Sq`[2]/992 #intra
var.intra1

#Variance total
var.total1 <- var.inter1+var.intra1
rapport1 = var.inter1/var.total1
rapport1
