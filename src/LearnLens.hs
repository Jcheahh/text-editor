{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module LearnLens where

import Control.Lens.Combinators (each, over, _1, _2, _Just, _Left)
import Data.Monoid (Endo)
import qualified Data.Text as Text
import qualified Lens.Micro as LM
import Lens.Micro.Platform (makeLenses)
import qualified Lens.Micro.Platform as LMP
import Test.Hspec (hspec, it, shouldBe)

data Address = Address
  {addressCity :: !Text.Text, addressStreet :: !Text.Text}

data Person = Person
  {personAddress :: !Address, personName :: !Text.Text}

getPersonCity :: Person -> Text.Text
getPersonCity = addressCity . personAddress

setPersonCity :: Text.Text -> Person -> Person
setPersonCity city person =
  person
    { personAddress =
        (personAddress person)
          { addressCity = city
          }
    }

modifyAddressCity :: (Text.Text -> Text.Text) -> Address -> Address
modifyAddressCity f address =
  address
    { addressCity = f (addressCity address)
    }

modifyPersonAddress :: (Address -> Address) -> Person -> Person
modifyPersonAddress f person =
  person
    { personAddress = f (personAddress person)
    }

modifyPersonCity :: (Text.Text -> Text.Text) -> Person -> Person
modifyPersonCity = modifyPersonAddress . modifyAddressCity

setPersonCity' :: Text.Text -> Person -> Person
setPersonCity' city = modifyPersonCity (const city)

data Lenso s a = Lenso
  {lensGetter :: s -> a, lensModify :: (a -> a) -> s -> s}

composeLenso :: Lenso a b -> Lenso b c -> Lenso a c
composeLenso (Lenso getter1 modify1) (Lenso getter2 modify2) =
  Lenso
    { lensGetter = getter2 . getter1,
      lensModify = modify1 . modify2
    }

personAddressL :: Lenso Person Address
personAddressL =
  Lenso
    { lensGetter = personAddress,
      lensModify = \f person -> person {personAddress = f (personAddress person)}
    }

addressCityL :: Lenso Address Text.Text
addressCityL =
  Lenso
    { lensGetter = addressCity,
      lensModify = \f address -> address {addressCity = f (addressCity address)}
    }

personCityL :: Lenso Person Text.Text
personCityL = personAddressL `composeLenso` addressCityL

setPersonCityM :: Text.Text -> Person -> Person
setPersonCityM city = lensModify personCityL (const city)

type LensModify s a = (a -> Identity a) -> (s -> Identity s)

-- over :: LensModify s a -> (a -> a) -> s -> s
-- over lens f s = runIdentity (lens (Identity . f) s)

personAddressM :: LensModify Person Address
personAddressM f person =
  Identity $
    person
      { personAddress = runIdentity $ f $ personAddress person
      }

personAddressM' :: LensModify Person Address
personAddressM' f person =
  (\address -> person {personAddress = address}) <$> f (personAddress person)

type LensGetter s a = (a -> Const a a) -> (s -> Const a s)

view :: LensGetter s a -> s -> a
view lens s = getConst (lens Const s)

personAddressG :: LensGetter Person Address
personAddressG f person = Const $ getConst $ f (personAddress person)

personAddressG' :: LensGetter Person Address
personAddressG' f person =
  (\address -> person {personAddress = address}) <$> f (personAddress person)

type Lensi s a = forall f. Functor f => (a -> f a) -> (s -> f s)

newtype Identity a = Identity {runIdentity :: a}
  deriving (Functor)

newtype Const a b = Const {getConst :: a} deriving (Functor)

over' :: Lensi s a -> (a -> a) -> s -> s
over' lens f s = runIdentity (lens (Identity . f) s)

view' :: Lensi s a -> s -> a
view' lens s = getConst (lens Const s)

personAddressL' :: Lensi Person Address
personAddressL' f person =
  (\address -> person {personAddress = address})
    <$> f (personAddress person)

getPersonAddress :: Person -> Address
getPersonAddress = view' personAddressL'

modifyPersonAddress' :: (Address -> Address) -> Person -> Person
modifyPersonAddress' = over' personAddressL'

setPersonAddress :: Address -> Person -> Person
setPersonAddress address = modifyPersonAddress' (const address)

lensi :: (s -> a) -> (s -> a -> s) -> Lensi s a
lensi getter setter = \f s -> (setter s) <$> f (getter s)

-- lens :: (s -> a) -> (s -> a -> s) -> Lensi s a
-- lens getter setter = \f s -> (setter s) <$> f (getter s)

personAddressL'' :: Lensi Person Address
personAddressL'' = lensi personAddress (\x y -> x {personAddress = y})

addressCityL' :: Lensi Address Text.Text
addressCityL' = lensi addressCity (\x y -> x {addressCity = y})

personCityL' :: Lensi Person Text.Text
personCityL' = personAddressL'' . addressCityL'

(^.) :: s -> Lensi s a -> a
s ^. lens = view lens s

infixl 8 ^.

(%~) :: Lensi s a -> (a -> a) -> s -> s
(%~) = over'

infixr 4 %~

reverseCity :: Person -> Person
reverseCity = personAddressL'' . addressCityL' %~ Text.reverse

getCity :: Person -> Text.Text
getCity person = person ^. personAddressL'' . addressCityL'

set :: Lensi s a -> a -> s -> s
set lens a s = runIdentity $ lens (\_olda -> Identity a) s

setCity :: Text.Text -> Person -> Person
setCity = set (personAddressL'' . addressCityL')

data PersonA age = PersonA
  { personNameA :: !Text.Text,
    personAge :: !age
  }

aliceInt :: PersonA Int
aliceInt = PersonA "Alice" 30

personAgeL :: Lensi (PersonA age) age
personAgeL = lensi personAge (\x y -> x {personAge = y})

setAge :: age -> PersonA oldAge -> PersonA age
setAge age person = person {personAge = age}

aliceDouble :: PersonA Double
aliceDouble = setAge 30.5 aliceInt

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

personAgeL' :: Lens (PersonA age1) (PersonA age2) age1 age2
personAgeL' = LM.lens personAge (\x y -> x {personAge = y})

setAge' :: age -> PersonA oldAge -> PersonA age
setAge' = LM.set personAgeL'

-------------------------------------------------------------------------------------

data Address' = Address'
  { _street :: !Text.Text,
    _city :: !Text.Text
  }

makeLenses ''Address'

-- address :: LMP.Lens' Address' Text.Text
-- address f addresss = (\s -> addresss {_street = s}) <$> f (_street addresss)

-- street' :: LMP.Lens' Address' Text.Text
-- street' = lens _street (\x y -> x {_street = y})

-- address' :: LMP.Lens' Person' Address'
-- address' = lens _address (\x y -> x {_address = y})

-- age' :: LMP.Lens' Address' Text.Text
-- age' = lens _street (\x y -> x {_street = y})

-- -------------------------------------------------------------------------------

-- street'' :: LMP.Lens' Address' Text.Text
-- street'' f addresss = (\x -> addresss {_street = x}) <$> f (_street addresss)

-- address'' :: LMP.Lens' Person' Address'
-- address'' f person = (\x -> person {_address = x}) <$> f (_address person)

-- age'' :: LMP.Lens' Person' Int
-- age'' f person = (\x -> person {_age = x}) <$> f (_age person)

data Person' = Person'
  { _name :: !Text.Text,
    _address :: !Address',
    _age :: !Int
  }

makeLenses ''Person'

lensPerson :: LMP.Lens' Person' Int
lensPerson f person = (\a -> person {_age = a}) <$> f (_age person)

hollywood :: Text.Text
hollywood = "Hollywood Blvd"

alice :: Person'
alice =
  Person'
    { _name = "Alice",
      _address =
        Address'
          { _street = hollywood,
            _city = "Los Angeles"
          },
      _age = 30
    }

wilshire :: Text.Text
wilshire = "Wilshire Blvd"

aliceWilshire :: Person'
aliceWilshire = set (address . street) wilshire alice

getStreet :: Person' -> Text.Text
getStreet = view (address . street)

birthday :: Person' -> Person'
birthday = over age (+ 1)

getAge :: Person' -> Int
getAge = view age

mapLens :: LMP.ASetter s t a b -> (a -> b) -> s -> t
mapLens l f = over l f

-- | toList
toListLens :: LMP.Getting (Endo [a]) s a -> s -> [a]
toListLens l s = s LMP.^.. l

-- | catMaybes
catMaybesLens :: [Maybe a] -> [a]
catMaybesLens list = list LMP.^.. each . _Just

main :: IO ()
main = hspec $ do
  it "lives on Wilshire" $
    _street (_address aliceWilshire) `shouldBe` wilshire
  it "getStreet works" $
    getStreet alice `shouldBe` hollywood
  it "birthday" $
    getAge (birthday alice) `shouldBe` _age alice + 1
  it "fun with tuples" $
    let tupleLens = (_2 . _1)
        tuple :: ((Int, Double), (Bool, Char, String))
        tuple = ((1, 2), (True, 'x', "Hello World"))
     in over tupleLens not tuple
          `shouldBe` ((1, 2), (False, 'x', "Hello World"))
  it "over left on left" $
    let val :: Either Int Double
        val = Left 5
     in over _Left (+ 1) val `shouldBe` Left 6
  it "over left on right" $
    let val :: Either Int Double
        val = Right 5
     in over _Left (+ 1) val `shouldBe` Right 5
  it "set left on left" $
    let val :: Either Int Double
        val = Left 5
     in LM.set _Left 6 val `shouldBe` Left 6
  it "set left on right" $
    let val :: Either Int Double
        val = Right 5
     in LM.set _Left 6 val `shouldBe` Right 5
  it "mapLens" $
    mapLens _2 not ((), True) `shouldBe` ((), False)
  it "toListLens" $
    toListLens LMP.both ('x', 'y') `shouldBe` "xy"
  it "catMaybesLens" $
    catMaybesLens [Just 'x', Nothing, Just 'y'] `shouldBe` "xy"