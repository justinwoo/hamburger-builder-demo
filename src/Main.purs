module Main where

import Prelude

import Control.IxMonad (class IxMonad, (:>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List, (:))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse_)

-- WHAT IS EVEN IN A HAMBURGER
newtype Ingredient = Ingredient String
derive instance newtypeIngredient :: Newtype Ingredient _

-- TYPE ALIAS BECAUSE YEAH
type BurgerSpec = List Ingredient

-- HOW DO WE BUILD A BURGER WITHOUT MESSING UP THE ORDERING?
-- WHY NOT INDEXED MONADS
newtype IxBurgerBuilder i o spec = IxBurgerBuilder spec

-- BASIC EXTRACTING OPERATION LIKE WE SHOULD HAVE
runIxBurgerBuilder :: forall prev next spec. IxBurgerBuilder prev next spec -> spec
runIxBurgerBuilder (IxBurgerBuilder spec) = spec

-- INSTANCE FOR INDEXED MONAD FOR OUR BURGER BUILDER
instance ixMonadIxBurgerBuilder :: IxMonad IxBurgerBuilder where
  ipure = IxBurgerBuilder
  ibind (IxBurgerBuilder spec) f = IxBurgerBuilder <<< runIxBurgerBuilder $ f spec

-- STAGES OF HAMBURGER BUILDING
data Ready
data EmptyPlate
data BottomBunOn
data PattyOn
data CheeseOn
data OnionOn
data LettuceOn
data TomatoOn
data PicklesOn
data TopBunOn

-- STARTING POINT
getEmptyPlate :: IxBurgerBuilder Ready EmptyPlate BurgerSpec
getEmptyPlate = IxBurgerBuilder mempty

addIngredient :: forall i o. String -> BurgerSpec -> IxBurgerBuilder i o (BurgerSpec)
addIngredient x xs = IxBurgerBuilder $ Ingredient x : xs

-- ADDING THE BUN
placeEmptyBun :: BurgerSpec -> IxBurgerBuilder EmptyPlate BottomBunOn BurgerSpec
placeEmptyBun = addIngredient "Bottom Bun"

-- ADD SOME SAUCES DIRECTLY ON THE BOTTOM BUN
addKetchup :: BurgerSpec -> IxBurgerBuilder BottomBunOn BottomBunOn BurgerSpec
addKetchup = addIngredient "Ketchup"

addMayo :: BurgerSpec -> IxBurgerBuilder BottomBunOn BottomBunOn BurgerSpec
addMayo = addIngredient "Mayo"

addMustard :: BurgerSpec -> IxBurgerBuilder BottomBunOn BottomBunOn BurgerSpec
addMustard = addIngredient "Mustard"

-- PUT THE PATTY ON, NO MORE SAUCES
addPatty :: BurgerSpec -> IxBurgerBuilder BottomBunOn PattyOn BurgerSpec
addPatty = addIngredient "Patty"

-- NEXT IS THE CHEESE, OR NONE
addCheese :: BurgerSpec -> IxBurgerBuilder PattyOn CheeseOn BurgerSpec
addCheese = addIngredient "Cheese"

noCheese :: BurgerSpec -> IxBurgerBuilder PattyOn CheeseOn BurgerSpec
noCheese = IxBurgerBuilder

-- ONIONS GO ON THE CHEESE, OR NONE
addOnions :: BurgerSpec -> IxBurgerBuilder CheeseOn OnionOn BurgerSpec
addOnions = addIngredient "Onion"

noOnions :: BurgerSpec -> IxBurgerBuilder CheeseOn OnionOn BurgerSpec
noOnions = IxBurgerBuilder

-- THEN LETTUCE, OR NONE
addLettuce :: BurgerSpec -> IxBurgerBuilder OnionOn LettuceOn BurgerSpec
addLettuce = addIngredient "Lettuce"

noLettuce :: BurgerSpec -> IxBurgerBuilder OnionOn LettuceOn BurgerSpec
noLettuce = IxBurgerBuilder

-- THEN TOMATO, OR NONE
addTomato :: BurgerSpec -> IxBurgerBuilder LettuceOn TomatoOn BurgerSpec
addTomato = addIngredient "Tomato"

noTomato :: BurgerSpec -> IxBurgerBuilder LettuceOn TomatoOn BurgerSpec
noTomato = IxBurgerBuilder

-- THEN FINISH THE BURGER WITH THE TOP BUN
addTopBun :: BurgerSpec -> IxBurgerBuilder TomatoOn TopBunOn BurgerSpec
addTopBun = addIngredient "TopBun"

-- OUR AMAZING INDEXED BURGER SPEC FROM READY TO TOP BUN ON
burgerSpec :: IxBurgerBuilder Ready TopBunOn BurgerSpec
burgerSpec = getEmptyPlate
  :>>= placeEmptyBun
  :>>= addKetchup
  :>>= addPatty
  :>>= addCheese
  :>>= addOnions
  :>>= noLettuce
  :>>= addTomato
  :>>= addTopBun

-- EXAMPLE THAT WOULD BE WRONG
-- wrongBurgerSpec :: IxBurgerBuilder Ready TopBunOn BurgerSpec
-- wrongBurgerSpec = getEmptyPlate
--   :>>= placeEmptyBun
--   :>>= addKetchup
--   :>>= addCheese -- Can't match PattyOn with BottomBunOn, since we haven't put on the patty, the most important part!!!
--   :>>= addOnions
--   :>>= noLettuce
--   :>>= addTomato
--   :>>= addTopBun

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ "My burger consists of:"
  traverse_ (log <<< append "  " <<< unwrap) $ runIxBurgerBuilder burgerSpec