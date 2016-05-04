module Main where

import Control.Alt (alt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Plus (empty, class Plus)
import Data.Array (last)
import Data.Foldable (class Foldable, foldr)
import Data.Generic (GenericSpine(SProd), fromSpine, GenericSignature(SigProd), toSignature, class Generic, gShow)
import Data.Maybe (Maybe(Nothing))
import Data.String (split)
import Prelude (map, show, bind, Unit, class Show, (==), (<<<), ($))
import Type.Proxy (Proxy(Proxy))

genericRead :: forall a. Generic a => String -> Proxy a -> Maybe a
genericRead s p =
  case (toSignature p) of
    (SigProd _ cs) -> oneOf (map step cs)
    _ -> Nothing
 where
  step constructor = do
    -- leave only last part of constructor ie. Main.Foo -> Foo
    let fullConstructorName = constructor.sigConstructor
    constructorName <- last <<< split "." $ fullConstructorName
    if constructorName == s
      -- this example only works for construcotrs of kind `*`
      then fromSpine $ SProd fullConstructorName []
      else Nothing

-- this helper will be available in purescript-foldable-traversable 1.0
oneOf :: forall f g a. (Foldable f, Plus g) => f (g a) -> g a
oneOf = foldr alt empty


data Something = Foo | Bar
derive instance genericFoo :: Generic Something
instance showSomething :: Show Something where
  show = gShow

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let p = Proxy :: (Proxy Something)
  log <<< show <<< genericRead "Foo" $ p
  log <<< show <<< genericRead "Bar" $ p
  log <<< show <<< genericRead "Baz" $ p
