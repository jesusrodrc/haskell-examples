-- auxiliary data
data Sex = Male | Female
data RHType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RHType
data Name = Name String String
data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False --otherwise
-- q 12.1

donorFor :: Patient -> Patient -> Bool
donorFor donor acceptor = canDonateTo (bloodType donor) (bloodType acceptor)