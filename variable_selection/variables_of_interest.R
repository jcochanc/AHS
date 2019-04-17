# Libraries.
pacman::p_load(dplyr, visdat, tidyr, ggplot2)

# Load the sample data.
df <- readRDS("./Data/sample_df.RDS")

#remove _h and _p from names
colnames(df)
colnames(df) <- sub("_h$|_p", "", colnames(df))

# Remove variables with more than 25% missing
df.nona <- df[, -which(colMeans(is.na(df)) > 0.25)]

# keep record of variables with more than 25% missingness
removed.vars <- names(which(colMeans(is.na(df)) > 0.25))

# Confirm that this did what we think it did.
length(which(colMeans(is.na(df)) > 0.25)) # 45 variables were removed, confirmed with percentage.plot
#percentage.plot(df.nona)


# Explore variables that we think are relevant 
# to housing quality. We should discuss this further


# Variables used to contruct zadeq variable, separated by categories
## According to paper "Housing Adequacy and Quality as Measured by the AHS
vars.zadeq.bathrooms <- c("PLUMB", "HOTPIP", "TUB", "TOILET", "SHARPF")
vars.zadeq.heating <- c("HEQUIP", "FREEZE", "IFCOLD", "NUMCOLD", 
                        "OTHCLD")
vars.zadeq.elect.wiring <- c("BUYE", "NOWIRE", "PLUGS", "IFBLOW", "NUMBLOW")
vars.zadeq.outside.leak <- c("LEAK", "RLEAK", "WLEAK", "BLEAK", "OTLEAK")
vars.zadeq.inside.leak <- c("ILEAK", "PILEAK", "PLEAK", "NLEAK1", 
                            "NLEAK2")
vars.zadeq.walls.floors.ceilings <- c("HOLES", "CRACKS", "BIGP")
vars.zadeq.rodents.toilet <- c("EVROD", "RATS", "IFTLT", "NUMTLT")
vars.zadeq.kitchens <- c("KITCHEN", "COOK", "OVEN", "BURNER", "REFR", "SINK", 
                         "KEXCLU")
vars.zadeq.structure <- c("EBROKE", "EBOARD", "ECRUMB", "EHOLER", 
                          "EMISSR", "EMISSW", "ESAGR", "ESLOPW", "EGOOD")
vars.zadeq.stair <- c("ELEV")
vars.zadeq.problems.outside <- c("IFDRY", "NUMDRY", "IFSEW", "NUMSEW", "EBAR")
vars.zadeq.overall.quality <- c("HOWH")


# Potential aditional variables of interest
vars.additional <- c("FPLWK", "AFUR", "COKST", "ELECT", "FLIN", "FPINS", "FRPL", 
                     "FRPLI", "GASPIP", "HOTH", "HPMP", "NONE", "PLF", "PORTH", 
                     "STEAM", "STOVE", "AFUEL", "AIR", "AIRSYS", "OARSYS", 
                     "WATER", "SEWDIS", "WFUEL", "CFUEL", "PHONE", "PUBSEW")

# Put all variables of interest into one vector
vars.quality.index <- tolower(c(vars.zadeq.bathrooms, vars.zadeq.heating, vars.zadeq.elect.wiring, 
                                vars.zadeq.outside.leak, vars.zadeq.inside.leak, 
                                vars.zadeq.walls.floors.ceilings, vars.zadeq.rodents.toilet, 
                                vars.zadeq.kitchens, vars.zadeq.structure, vars.zadeq.stair, 
                                vars.zadeq.problems.outside, vars.zadeq.overall.quality, vars.additional))

# Select variables of interest
df.quality.vars <- df.nona %>% select(vars.quality.index)

# Confirm that this did what we think it did.
sum(duplicated(vars.quality.index))
length(vars.quality.index)

