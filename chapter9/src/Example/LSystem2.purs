module Example.LSystem2 where

import Prelude (class Monad, bind, ($), return, (*), (+), (/), (-))
import Data.Maybe.Unsafe (fromJust)
import Data.Array (concatMap, foldM)

import Control.Monad.Eff (Eff)
import Graphics.Canvas hiding (translate)

lsystem :: forall a m s. (Monad m) =>
                         Array a ->
                         (a -> Array a) ->
                         (s -> a -> m s) ->
                         Int ->
                         s ->
                         m s
lsystem init prod interpret n state =
  interpretSentences (buildSentences init prod n) interpret state


interpretSentences :: forall a m s. (Monad m) =>
                         Array a ->
                         (s -> a -> m s) ->
                         s ->
                         m s
interpretSentences sentences interpret initialState = foldM interpret initialState sentences


buildSentences :: forall a.
                         Array a ->
                         (a -> Array a) ->
                         Int ->
                         Array a
buildSentences init prod n = go init n
  where
  go s 0 = s
  go s n = go (concatMap prod s) (n - 1)


data Alphabet = L | R | F Boolean

type Sentence = Array Alphabet

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }



main :: Eff( canvas :: Canvas) State
main = do
  canvasMaybe <- getCanvasElementById "canvas"
  let canvas = fromJust canvasMaybe
  ctx <- getContext2D canvas

  let
    initial :: Sentence
    initial = [F false]

    productions :: Alphabet -> Sentence
    productions L = [L]
    productions R = [R]
    productions (F true)  = [F true, L, F false, L, F true, R, F false, R, F true, R, F false, R, F true, L, F false, L, F true]
    productions (F false) = [F false, R, F true, R, F false, L, F true, L, F false, L, F true, L, F false, R, F true, R, F false]

    interpret :: State -> Alphabet -> Eff (canvas :: Canvas) State
    interpret state L = return $ state { theta = state.theta - Math.pi / 3.0 }
    interpret state R = return $ state { theta = state.theta + Math.pi / 3.0 }
    interpret state _ = do
      let x' = state.x + Math.cos state.theta * 1.5
          y' = state.y + Math.sin state.theta * 1.5
      lineTo ctx x' y'
      return { x: x', y: y', theta: state.theta }

    initialState :: State
    initialState = { x: 120.0, y: 200.0, theta: 0.0 }

  setStrokeStyle "#000000" ctx
  setFillStyle "#ff8800" ctx

  fillPath ctx $ do
    setShadowOffsetX 10.0 ctx
    setShadowOffsetY 10.0 ctx
    setShadowBlur 5.0 ctx
    setShadowColor "#aa4400" ctx
    moveTo ctx initialState.x initialState.y
    lsystem initial productions interpret 4 initialState
    closePath ctx

  strokePath ctx $ do
    moveTo ctx initialState.x initialState.y
    lsystem initial productions interpret 4 initialState
