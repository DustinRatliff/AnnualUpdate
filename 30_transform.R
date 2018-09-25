#Mutate to rename variables----

target_county <- target_county %>%
  mutate(
    GINI = B19083_001E,
    Poverty = B17001_002E,
    Under5 = B01001_003E + B01001_027E,
    Older64Male = B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E,
    Older64Female = B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E,
    Older64 = Older64Male + Older64Female,
    NoVehicle = B08141_002E / B08141_001E,
    MedianIncome = B19013_001E,
    White = B03002_003E,
    Black = B03002_004E,
    AIAN = B03002_005E,
    Asian = B03002_006E,
    NHPI = B03002_007E,
    OtherRace = B03002_008E,
    MultipleOrOtherRace = B03002_010E,
    Hispanic = B03002_012E,
    NonHispanic = (B03002_001E-B03002_012E),
    LessThanNinthGrade = (
      B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + 
        B15003_009E + B15003_010E + B15003_011E + B15003_012E
    ),
    NinthToTwelthNoDiploma = (B15003_013E + B15003_014E + B15003_015E + B15003_016E),
    LessThanHighSchool = (B15003_013E + B15003_014E + B15003_015E + B15003_016E +B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + 
                            B15003_009E + B15003_010E + B15003_011E + B15003_012E),
    HighSchool = (B15003_017E + B15003_018E),
    SomeCollegeNoDegree = (B15003_019E + B15003_020E),
    AssociatesDegree = B15003_021E,
    BachelorsDegree = B15003_022E,
    MastersDegree = B15003_023E,
    ProfessionalSchoolDegree = B15003_024E,
    DoctorateDegree = B15003_025E,
    GraduateOrProfessional = (B15003_023E + B15003_024E + B15003_025E) ,
    HouseholdIncomeTowardsHousing = (B25105_001E * 12) / B19013_001E *
      100,
    HousingCosts = B25105_001E,
    Population = B01003_001E,
    MaleNoInsurance = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + 
                         B27001_023E + B27001_026E + B27001_029E),
    FemaleNoInsurance = (B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E + B27001_048E + 
                           B27001_051E + B27001_054E + B27001_057E),
    NoInsurance = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + 
                     B27001_023E + B27001_026E + B27001_029E +B27001_033E + B27001_036E + B27001_039E + 
                     B27001_042E + B27001_045E + B27001_048E + B27001_051E + B27001_054E + B27001_057E),
    OwnHouse = B25106_002E
  )
