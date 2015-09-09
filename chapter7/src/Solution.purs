module Solution where

import Prelude
import Data.Maybe
import Data.Either


--class Functor f where
--  map :: forall a b. (a -> b) ->
--                     f a -> f b
--
--class (Functor f) <= Apply f where
--  apply :: forall a b. f (a -> b) ->
--                       f a -> f b

--instance functorMaybe :: Functor Maybe where
--  map f (Just a) = Just (f a)
--  map f Nothing  = Nothing
--
--instance applyMaybe :: Apply Maybe where
--  apply (Just f) (Just x) = Just (f x)
--  apply _        _        = Nothing

lift1 :: forall a b f. (Prelude.Apply f) =>
                           (a -> b) ->
                           f a -> f b
--lift1 f x y z = f <$> x
lift1 f x = map f x


lift2 :: forall a b c f. (Prelude.Apply f) =>
                           (a -> (b -> c)) ->
                           f a -> f b -> f c
--lift2 f x y z = f <$> x <*> y
--lift2 f x y = apply (map  f x) y
lift2 fun a b = apply liftedfx b
  where
  liftedfx :: f (b -> c)
  liftedfx = map fun a


lift3 :: forall a b c d f. (Prelude.Apply f) =>
                           (a -> b -> c -> d) ->
                           f a -> f b -> f c -> f d
--lift3 f x y z = f <$> x <*> y <*> z
lift3 f x y z = apply (apply (map  f x) y) z


--class (Apply f) <= Applicative f where
--  pure :: forall a. a -> f a

--instance applicativeMaybe :: Applicative Maybe where
--  pure x = Just x

fullName first middle last = last ++ ", " ++ first ++ " " ++ middle

fullNameMaybe :: Maybe String -> Maybe String -> Maybe String -> Maybe String
fullNameMaybe first middle last = fullName <$> first <*> middle <*> last


(<?>) :: forall a e. Maybe a -> e  -> Either e a
(<?>) Nothing  err = Left err
(<?>) (Just a) _ = Right a


fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either String String
--fullNameEither first middle last =
--    fullName <$> (first  <?> "First name was missing")
--             <*> (middle <?> "Middle name was missing")
--             <*> (last   <?> "Last name was missing")
fullNameEither first middle last =
    lift3 fullName (first  <?> "First name was missing") (middle <?> "Middle name was missing") (last   <?> "Last name was missing")

