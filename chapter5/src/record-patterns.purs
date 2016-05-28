module RecordPatterns where

import Data.Foldable
import Prelude (otherwise, (==), (+), (-), (*), class Eq)

type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false

marco :: Person
marco = {name: "Marco", address: {city: "Berlin", street: "OurStreet"}}

nora :: Person
nora = {name: "Nora", address: {city: "Berlin", street: "OurStreet"}}

lars :: Person
lars = {name: "Lars", address: {city: "Leipzig", street: "AnimalStreet"}}

-- 1. Exercise
-- Write a function sameCity which uses record patterns to test whether two Person records belong to the same city.

sameCity :: Person -> Person -> Boolean
sameCity {address: {city: c1}} {address: {city: c2}} = c1 == c2

-- 2. Exercise
-- What is the most general type of the sameCity function, taking into account row polymorphism?

sameCity' ::  forall p1 p2 a1 a2 ct. (Eq ct) => {address :: {city :: ct | a1} |p1} -> {address :: {city :: ct | a2} | p2}-> Boolean
sameCity' {address: {city: c1}} {address: {city: c2}} = c1 == c2

-- What about the livesInLA function defined above?
livesInLA' :: forall p a. {address :: {city :: String | a} | p} -> Boolean
livesInLA' { address: { city: "Los Angeles" } } = true
livesInLA' _ = false


-- 3. Exercise
-- Write a function fromSingleton which uses an array literal pattern to extract
-- the sole member of a singleton array.
-- If the array is not a singleton, your function should return a provided default value.

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [a] = a
fromSingleton defaultValue _ = defaultValue
