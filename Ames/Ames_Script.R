## Libraries
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(RColorBrewer)
library(ggrepel)

## Data
crash_data = read.csv('crash.csv')
crash_vehicle_data = read.csv('crash_vehicle.csv')

# Rename columns
crash_data = crash_data %>% rename('X' = 'ï..X')
crash_vehicle_data = crash_vehicle_data %>% rename('X' = 'ï..X')

## Filter Data to Ames
crash_data = crash_data %>% filter(X > -93.75 & X < -93.55 & Y < 42.08 & Y > 41.98)
crash_vehicle_data = crash_vehicle_data %>% filter(X > -93.75 & X < -93.55 & Y < 42.08 & Y > 41.98 & VEH_CRASH_KEY %in% unique(crash_data$CRASH_KEY))


## Data Cleaning

# Fix datatypes
crash_data = crash_data %>% 
  mutate(CRASH_DATE = as_datetime(as.character(CRASH_DATE)),
         LECASENUM = as.numeric(LECASENUM),
         LITERAL = as.character(LITERAL),
         DRUGALC = factor(DRUGALC, levels=rev(c('None Indicated', 'Refused', 
                                                'Alcohol (<Statutory)', 'Drug/Alcohol (< Statutory)', 
                                                'Drug', 
                                                'Alcohol (Statutory)', 'Drug/Alcohol (Statutory)', 
                                                'Under Influence of Alcohol/Drugs/Medications')))) %>% 
  select(-X.1, -OBJECTID,
         -CITY_NUMBER, -DISTRICT, -COUNTY_NUMBER, -XCOORD, -YCOORD, 
         -CITY_NAME, -COUNTY_NAME, 
         -CRASH_DATETIME, -CRASH_DATETIME_UTC, -CRASH_DATETIME_UTC_OFFSET, -REST_UPDATED, -REST_UPDATE_UTC_OFFSET, 
         -CARDINAL, -GLOBALID)

crash_vehicle_data = crash_vehicle_data %>%
  mutate(CRASH_DATETIME = as_datetime(as.character(CRASH_DATETIME))) %>%
  select(-CRASH_DATETIME_UTC, CRASH_DATETIME_UTC_OFFSET, -REST_UPDATED, -REST_UPDATED_UTC_OFFSET, -GLOBALID)

# Fix dates
crash_data = crash_data %>% 
  separate(CRASH_DATE, c('year', 'month', 'day')) %>% select(-month) %>% mutate(year = as.numeric(year), day = as.numeric(day)) %>%
  separate(TIMESTR, c('hour', 'minute')) %>% mutate(hour = as.numeric(hour), minute = as.numeric(minute))

crash_vehicle_data = crash_vehicle_data %>% mutate(year = year(CRASH_DATETIME), month = month(CRASH_DATETIME), day = day(CRASH_DATETIME), 
                                                   hour = hour(CRASH_DATETIME), minute = minute(CRASH_DATETIME)) %>% select(-CRASH_DATETIME)

# Fixing car make names
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'ACAT'] = 'CAT' 

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Acur' | 
                                  levels(crash_vehicle_data$MAKE) == 'ACUR'] = 'Acura'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'AUDI'] = 'Audi'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'BIGD'] = 'BIG'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'BLUE'] = 'Blue'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'BUIC' | 
                                  levels(crash_vehicle_data$MAKE) == 'Buic'] = 'Buick'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Cadi' | 
                                  levels(crash_vehicle_data$MAKE) == 'Cagi' |
                                  levels(crash_vehicle_data$MAKE) == 'CADI'] = 'Cadillac'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'CAPA'] = 'Capa'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Chev' | 
                                  levels(crash_vehicle_data$MAKE) == 'CHEV'] = 'Chevrolet'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Chry' | 
                                  levels(crash_vehicle_data$MAKE) == 'CHRY'] = 'Chrysler'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Dodg' | 
                                  levels(crash_vehicle_data$MAKE) == 'DODG' |
                                  levels(crash_vehicle_data$MAKE) == 'RAM' |
                                  levels(crash_vehicle_data$MAKE) == 'Ram'] = 'Dodge'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'DUCA'] = 'Duca'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Eagl' | 
                                  levels(crash_vehicle_data$MAKE) == 'EGIL'] = 'Eagle'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'ELDO'] = 'Eldo'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'FIAT'] = 'Fiat'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'FORD' |
                                  levels(crash_vehicle_data$MAKE) == 'FRD' |
                                  levels(crash_vehicle_data$MAKE) == 'VICT'] = 'Ford'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'FRE' | 
                                  levels(crash_vehicle_data$MAKE) == 'Frei' | 
                                  levels(crash_vehicle_data$MAKE) == 'FREI' | 
                                  levels(crash_vehicle_data$MAKE) == 'FRET' | 
                                  levels(crash_vehicle_data$MAKE) == 'FRG' |
                                  levels(crash_vehicle_data$MAKE) == 'FRHG' | 
                                  levels(crash_vehicle_data$MAKE) == 'FRHT' | 
                                  levels(crash_vehicle_data$MAKE) == 'FRT' | 
                                  levels(crash_vehicle_data$MAKE) == 'FRTL' |
                                  levels(crash_vehicle_data$MAKE) == 'FTL'] = 'Freightliner'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'GENU'] = 'Genu'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'GEO'] = 'GMC'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'GIL' | 
                                  levels(crash_vehicle_data$MAKE) == 'GILG' | 
                                  levels(crash_vehicle_data$MAKE) == 'GILL' | 
                                  levels(crash_vehicle_data$MAKE) == 'GLLG'] = 'Gill'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Harl' | 
                                  levels(crash_vehicle_data$MAKE) == 'HARL' |
                                  levels(crash_vehicle_data$MAKE) == 'HD' |
                                  levels(crash_vehicle_data$MAKE) == 'HDME' |
                                  levels(crash_vehicle_data$MAKE) == 'Harley Murra'] = 'Harley David'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'HAUL' | 
                                  levels(crash_vehicle_data$MAKE) == 'NEWH' |
                                  levels(crash_vehicle_data$MAKE) == 'VNHL'] = 'Newhaul'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Hond' | 
                                  levels(crash_vehicle_data$MAKE) == 'HOND'] = 'Honda'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Humm' | 
                                  levels(crash_vehicle_data$MAKE) == 'HUMM' | 
                                  levels(crash_vehicle_data$MAKE) == 'HUMV'] = 'Hummer'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Hyun' | 
                                  levels(crash_vehicle_data$MAKE) == 'HYUN'] = 'Hyundai'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Ic' | 
                                  levels(crash_vehicle_data$MAKE) == 'IC' | 
                                  levels(crash_vehicle_data$MAKE) == 'IC B' | 
                                  levels(crash_vehicle_data$MAKE) == 'Ic C'] = 'IC'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Infi' | 
                                  levels(crash_vehicle_data$MAKE) == 'INFI'] = 'Infiniti'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'INT' | 
                                  levels(crash_vehicle_data$MAKE) == 'Inte' | 
                                  levels(crash_vehicle_data$MAKE) == 'INTE' | 
                                  levels(crash_vehicle_data$MAKE) == 'Internationa' | 
                                  levels(crash_vehicle_data$MAKE) == 'INTL'] = 'International Harvester'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'ISU' | 
                                  levels(crash_vehicle_data$MAKE) == 'Isuz' | 
                                  levels(crash_vehicle_data$MAKE) == 'ISUZ'] = 'Isuzu'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Jagu' | 
                                  levels(crash_vehicle_data$MAKE) == 'JAGU'] = 'Jaguar'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'JEEP'] = 'Jeep'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Kawa' | 
                                  levels(crash_vehicle_data$MAKE) == 'KAWK'] = 'Kawasaki'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'KEN' | 
                                  levels(crash_vehicle_data$MAKE) == 'KENT' | 
                                  levels(crash_vehicle_data$MAKE) == 'Kenw' | 
                                  levels(crash_vehicle_data$MAKE) == 'KENW' |
                                  levels(crash_vehicle_data$MAKE) == 'KW'] = 'Kenworth'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'LANC' |
                                  levels(crash_vehicle_data$MAKE) == 'LC'] = 'Lanc'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'LAND' |
                                  levels(crash_vehicle_data$MAKE) == 'Land' |
                                  levels(crash_vehicle_data$MAKE) == 'LNDR'] = 'Land Rover'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'LEXS' | 
                                  levels(crash_vehicle_data$MAKE) == 'Lexu' | 
                                  levels(crash_vehicle_data$MAKE) == 'LEXU'] = 'Lexus'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'LINC' |
                                  levels(crash_vehicle_data$MAKE) == 'Linc'] = 'Lincoln'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'MACK'] = 'Mack'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Mazd' |
                                  levels(crash_vehicle_data$MAKE) == 'MAZD'] = 'Lincoln'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'MINI' |
                                  levels(crash_vehicle_data$MAKE) == 'MNNI'] = 'Mini'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'MILL'] = 'Mill'
# this is ambiguous between mercury and mercedes benz
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Merc' |
                                  levels(crash_vehicle_data$MAKE) == 'MERC'] = 'Mercury'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'MERZ'] = 'Mercedes-ben'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'MITS' |
                                  levels(crash_vehicle_data$MAKE) == 'Mits'] = 'Mitsubishi'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'MON'] = 'MONS'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'N/A' |
                                  levels(crash_vehicle_data$MAKE) == 'NR' |
                                  levels(crash_vehicle_data$MAKE) == 'UNKNO' |
                                  levels(crash_vehicle_data$MAKE) == 'UNK' |
                                  levels(crash_vehicle_data$MAKE) == 'Unkn' |
                                  levels(crash_vehicle_data$MAKE) == 'UNKN' |
                                  levels(crash_vehicle_data$MAKE) == 'UKNO' |
                                  levels(crash_vehicle_data$MAKE) == 'VAN' |
                                  levels(crash_vehicle_data$MAKE) == 'Util' |
                                  levels(crash_vehicle_data$MAKE) == 'UTIM' |
                                  levels(crash_vehicle_data$MAKE) == 'VANG'] = NA
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'NISS' |
                                  levels(crash_vehicle_data$MAKE) == 'Niss'] = 'Nissan'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'OLDS' |
                                  levels(crash_vehicle_data$MAKE) == 'Olds'] = 'Oldsmobile'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'OPTI' |
                                  levels(crash_vehicle_data$MAKE) == 'OPT'] = 'Opti'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'PETE' |
                                  levels(crash_vehicle_data$MAKE) == 'Pete' |
                                  levels(crash_vehicle_data$MAKE) == 'PTRB'] = 'Peterbilt'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'PLYM' |
                                  levels(crash_vehicle_data$MAKE) == 'Plym'] = 'Plymouth'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'PONT' |
                                  levels(crash_vehicle_data$MAKE) == 'Pont'] = 'Pontiac'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'PORS' |
                                  levels(crash_vehicle_data$MAKE) == 'Pors' |
                                  levels(crash_vehicle_data$MAKE) == 'PRO'] = 'Porsche'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'SAA'] = 'Saab'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'SATR' |
                                  levels(crash_vehicle_data$MAKE) == 'Satu' |
                                  levels(crash_vehicle_data$MAKE) == 'SATU' |
                                  levels(crash_vehicle_data$MAKE) == 'STRN'] = 'Saturn'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'SMRT'] = 'Smar'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'STER' | 
                                  levels(crash_vehicle_data$MAKE) == 'Ster' | 
                                  levels(crash_vehicle_data$MAKE) == 'STLG' | 
                                  levels(crash_vehicle_data$MAKE) == 'STRG'] = 'Sterling'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Suba' |
                                  levels(crash_vehicle_data$MAKE) == 'SUBA'] = 'Subaru'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'SUPR'] = 'Supr'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'SUZI' |
                                  levels(crash_vehicle_data$MAKE) == 'Suzu' |
                                  levels(crash_vehicle_data$MAKE) == 'SUZU'] = 'Suzuki'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'SYM'] = 'SYMG'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'THMS' |
                                  levels(crash_vehicle_data$MAKE) == 'THOM'] = 'Thom'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Toyo' |
                                  levels(crash_vehicle_data$MAKE) == 'TOYO' |
                                  levels(crash_vehicle_data$MAKE) == 'TOYT'] = 'Toyota'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'VESP'] = 'Vesp'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'VOLV' | 
                                  levels(crash_vehicle_data$MAKE) == 'VOLO' | 
                                  levels(crash_vehicle_data$MAKE) == 'VOLV' | 
                                  levels(crash_vehicle_data$MAKE) == 'Volv' |
                                  levels(crash_vehicle_data$MAKE) == 'VLV'] = 'Volvo'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'Volk' |
                                  levels(crash_vehicle_data$MAKE) == 'VOLK' |
                                  levels(crash_vehicle_data$MAKE) == 'VOL'] = 'Volkswagen'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'WEST'] = 'West'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'WHIT'] = 'Whit'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'WINN'] = 'Winn'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'WRKH' |
                                  levels(crash_vehicle_data$MAKE) == 'WORK'] = 'Work'

levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'YAMA'] = 'Yama'
levels(crash_vehicle_data$MAKE)[levels(crash_vehicle_data$MAKE) == 'TRIU'] = 'Triu'
valid_makes = c('CAT', 'Acura', 'ALFA', 'Audi', 
                'BMW', 'Buick', 
                'Cadillac', 'Chevrolet', 'Chrysler',
                'Dats', 'DEER', 'DELT', 'Dodge',
                'Eagle',
                'Fiat', 'Ford', 'Freightliner',
                'GMC', 'Gill', 
                'Harley David', 'Honda', 'Hummer', 'Hyundai',
                'IC', 'Infiniti', 'International Harvester', 'Isuzu',
                'Jaquar', 'Jeep',
                'Kawasaki', 'Kenworth', 'KIA', 
                'Lanc', 'Land Rover', 'Lexus', 'Lincoln', 
                'Mack', 'Mazda', 'Mercury', 'Mercedes-ben', 'Mini', 'Mill', 'Mitsubishi',
                'Newhaul', 'Nissan',
                'Oldsmobile', 
                'Peterbilt', 'Piag', 'Plymouth', 'Pontiac', 'Porsche',
                'Saab', 'Saturn', 'SCIO', 'Smar', 'Spec', 'Sterling', 'Subaru', 'Suzuki',
                'Thom', 'Timpte', 'Toyota', 
                'Volkswagen', 'Volvo',
                'West', 'Whit', 'Winn',
                'Yama')

levels(crash_vehicle_data$MAKE)[!levels(crash_vehicle_data$MAKE) %in% valid_makes] = NA

crash_data$DRUGALC = ordered(crash_data$DRUGALC, levels=c('None Indicated', 'Alcohol (Statutory)', 'Refused', 'Under Influence of Alcohol/Drugs/Medications', 'Alcohol (< Statutory)', 'Drug', 'Drug/Alcohol (Statutory)'))

## Analysis

# Dangerous roads/intersections
png('./ames_worse_streets.png', width = 1920, height = 1080)
streets = crash_data %>% mutate(total=X*X+Y*Y) 
scnt = streets %>% 
  group_by(total) %>% 
  summarize(count = n())

streets = streets %>% 
  left_join(scnt, by='total') %>% 
  arrange(desc(count))
str(streets %>% distinct(LITERAL, .keep_all=TRUE))
head(unique(streets$LITERAL), 10)
topstreets = streets %>% 
  distinct(total, .keep_all=TRUE) %>% 
  distinct(LITERAL, .keep_all=TRUE) %>% 
  filter(LITERAL %in% head(LITERAL, 10))
topstreets %>% ggplot(aes(x=X, y=Y, label=LITERAL)) + 
  geom_point(data=crash_data, aes(x=X, y=Y), show.legend=FALSE) + 
  geom_point(aes(color=count), size=10) + geom_text_repel(aes(color=count, fill='white', fontface='bold'), size=8, box.padding=2, show.legend=FALSE) +
  scale_color_gradient(low='blue', high='red') + ggtitle('Dangerous Roads/Intersections') + xlab('Latitude') + ylab('Longitude')
dev.off()

# Drugs/Alcohol
png('./drug_alcohol.png', width = 960, height = 540)
crash_data %>% ggplot(aes(x=DRUGALC)) + geom_bar() + coord_flip() + ggtitle('Drugs & Alchol')
dev.off()
table(crash_data$DRUGALC)

# Drivers Age
png('./age.png', width = 960, height = 540)
crash_vehicle_data %>% ggplot(aes(DRIVERAGE)) + geom_histogram(breaks=seq(0, 98, by=1), col="red", aes(fill=..count..)) + scale_fill_gradient("Count", low = "green", high = "red") + ggtitle('Accidents By Age') + ylab('Number of Accidents') + xlab('Driver\'s Age')
dev.off()

# Driver Skill over Time
png('./skill_over_time.png', width = 960, height = 540)
pop = data.frame("year" = 2009:2019, "pop" = c(52271, 59100, 60432, 61426, 63046, 64765, 65662, 65881, 66285, 67154, 67154))
crash_data %>% filter(year < 2020) %>% left_join(pop) %>% group_by(year) %>% mutate(acc_rate = (n() / pop) * 100) %>% ggplot(aes(x=year, y=acc_rate)) + geom_point() + geom_smooth(color='red') + scale_x_continuous(breaks=c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)) + ylim(0, 2.5) + ggtitle('Driver Skill Over Time') + ylab('Accident Rate (%)') + xlab('Year')
dev.off()

# Speed Limits
png('./surface_conditions.png', width = 960, height = 540)
crash_vehicle_data$SPEEDLIMIT = as.numeric(gsub("[^0-9.-]", "", crash_vehicle_data$SPEEDLIMIT))
crash_vehicle_data$SPEEDLIMIT = ordered(crash_vehicle_data$SPEEDLIMIT, levels=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70))
crash_vehicle_data = crash_vehicle_data %>% rename('Surface Conditions' = CSURFCOND)
crash_vehicle_data %>% ggplot(aes(x=SPEEDLIMIT, fill=`Surface Conditions`, position='fill')) + geom_bar() + ggtitle('Speed Limit for Conditions') + xlab('Speed Limit (mph)') + ylab('Number of Accidents')
dev.off()
