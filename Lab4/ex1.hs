polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r,phi) = (r*cos phi,r*sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r*cos phi,r*sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a,a) deriving (Show)
newtype PolarCoord'' a = MkPolarCoord'' (a,a) deriving (Show)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r*cos phi,r*sin phi)

--Zadania

personalInfoString :: (String,String,String) -> String
personalInfoString (nm,snm,addr) = "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personalInfoString' :: PersonInfoToStringType'
personalInfoString' (nm,snm,addr) = "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

newtype PersonInfo'' = MkPersonInfo (Name',Surname',Address') deriving (Show)
personalInfoString'' :: PersonInfo'' -> String
personalInfoString'' (MkPersonInfo(nm,snm,addr)) = "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

