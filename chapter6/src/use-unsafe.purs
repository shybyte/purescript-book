module UseUnsafe where

import Unsafe (class Unsafe)

result :: Unsafe => Int
result = Unsafe.last [1, 2, 3]

-- Error!
-- result :: Int
-- result = Unsafe.last [1, 2, 3]
