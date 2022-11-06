-- In this lesson, we'll create our own custom types.

-- Type Synonyms
type FirstName = String
type LastName = String
type Age = Int
type Height = Int
type Weight = Int

-- type synonyms make functions much more readable
patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
  where name = lname ++ " " ++ fname
        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

-- Accessors
type PatientName = (FirstName, LastName)

firstName :: PatientName -> FirstName
firstName patient = fst patient

lastName :: PatientName -> LastName
lastName patient = snd patient

patientName = ("John", "Doe")
patientFirstName = firstName patientName


-- Creating New Types
data Sex = Male | Female
-- Sex is a type constructor. Male and Female are data constructors that create
-- a concrete instance of the type.

showSex :: Sex -> String
showSex Male   = "Male"
showSex Female = "Female"

-- we can use this new type to create safe and meaningful functions, e.g.
-- if we instead used a Bool to model Sex, then we'd have to use True and False
-- to represent the cases below
sexInitial :: Sex -> Char
sexInitial Male   = 'M'
sexInitial Female = 'F'

-- modelling blood type
data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

-- even though there are 8 combinations of ABOType and RhType, we simply need
-- to define a BloodType data constructor that takes a pair of ABOType and RhType
-- We read the data constructor like "A BloodType is an ABOType with an RhType"
patientBloodType :: BloodType
patientBloodType = BloodType AB Pos

-- writing show functions for BloodType
showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

-- modelling the rules of whom can donate to whom (ignoring RhType)
canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _                = True  -- universal donor
canDonateTo _               (BloodType AB _) = True  -- universal receiver
canDonateTo (BloodType A _) (BloodType A _)  = True
canDonateTo (BloodType B _) (BloodType B _)  = True
canDonateTo _               _                = False

patientBTO  = BloodType O Pos
patientBTAB = BloodType AB Neg

canDonateToResult1 = canDonateTo patientBTO patientBTAB
canDonateToResult2 = canDonateTo patientBTAB patientBTO

-- Creating a more robust name type
type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l)             = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

johnsName = NameWithMiddle "John" "David" "Doe"
namePrint = showName johnsName

-- Using Record Syntax
-- lets define a new data type Patient to collect all the args passed to the
-- patientInfo function at the start of the lesson
data Patient = Patient Name Sex Age Height Weight BloodType

patient1 = Patient (Name "John" "Doe") Male 59 74 200 (BloodType AB Pos)
patient2 = Patient (NameWithMiddle "Mary" "Elizabeth" "Smith") Female 30 65 140 (BloodType A Neg)

-- this new data type is great, we've been able to add Sex and BloodType, but it
-- is getting quite unwieldy with so many properties. Since the ordering of each
-- property is strict, it is very easy to make a mistake when creating patients.
-- Additionally, another problem is that if we want to access individual
-- properties on the patients, we'd need to write accessors for each one!
-- To solve these problems, Haskell provides the Record syntax. It allows us to
-- create patients without worrying about the property ordering and it also
-- generates boilerplate functions, such as accessors for us!

data PatientRecord = PatientRecord { name      :: Name
                                   , sex       :: Sex
                                   , age       :: Age
                                   , height    :: Height
                                   , weight    :: Weight
                                   , bloodType :: BloodType }

patientRecord1 :: PatientRecord
patientRecord1 = PatientRecord { name = Name "John" "Doe"
                               , sex = Male
                               , age = 59
                               , height = 74
                               , weight = 200
                               , bloodType = BloodType AB Pos }

patientRecord2 :: PatientRecord
patientRecord2 = PatientRecord { name = NameWithMiddle "Mary" "Elizabeth" "Smith"
                               , sex = Female
                               , age = 30
                               , height = 65
                               , weight = 140
                               , bloodType = BloodType A Neg }

-- we can use the provided accessors for each property
johnsHeight = height patientRecord1
johnsBloodType = showBloodType (bloodType patientRecord1)

-- Setting data
patientRecord1Updated = patientRecord1 { age = 60 }


-- Questions
-- Q12.1: Write a function similar to canDonateTo but takes a PatientRecord type
canDonateTo2 :: PatientRecord -> PatientRecord -> Bool
canDonateTo2 PatientRecord{bloodType=BloodType O _} _                                       = True  -- universal donor
canDonateTo2 _                                      PatientRecord{bloodType=BloodType AB _} = True  -- universal receiver
canDonateTo2 PatientRecord{bloodType=BloodType A _} PatientRecord{bloodType=BloodType A _}  = True
canDonateTo2 PatientRecord{bloodType=BloodType B _} PatientRecord{bloodType=BloodType B _}  = True
canDonateTo2 _                                      _                                       = False

-- Note we could make this much simpler by reusing canDonateTo
canDonateTo3 :: PatientRecord -> PatientRecord -> Bool
canDonateTo3 donor receiver = canDonateTo (bloodType donor) (bloodType receiver)


-- Q12.2: Implement patientSummary function
patientSummary :: PatientRecord -> String
patientSummary patient =  "***************\n"
                       ++ "Patient Name: "    ++ showName (name patient)            ++ "\n"
                       ++ "Sex: "             ++ showSex (sex patient)              ++ "\n"
                       ++ "Age: "             ++ show (age patient)                 ++ "\n"
                       ++ "Height: "          ++ show (height patient)              ++ " ins.\n"
                       ++ "Weight: "          ++ show (weight patient)              ++ " lbs.\n"
                       ++ "Blood Type: "      ++ showBloodType (bloodType patient)  ++ "\n"
                       ++ "***************\n"


--patientInfo2 :: Patient -> String
--patientInfo2 (Patient fname lname age height) = name ++ " " ++ ageHeight
--  where name = lname ++ " " ++ fname
--        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

main :: IO()
main = print namePrint
