module History 
    (History, singleton, append, back, forward, current, hasPast, hasFuture)
where

import Prelude

import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))

data History a = 
    History
        { past :: List a
        , current :: a
        , future :: List a
        }

derive instance eqHistory :: Eq a => Eq (History a)
derive instance ordHistory :: Ord a => Ord (History a)
instance showHistory :: Show a => Show (History a) where
    show (History history) = 
        "{ past : " 
            <> show history.past 
            <> ", current : " 
            <> show history.current 
            <> ", future : " 
            <> show history.future
            <> " }"

back :: forall a. History a -> History a
back (History h) =
    case List.uncons h.past of 
        Just { head, tail } -> 
            History $ h { past = tail, current = head, future = head : h.future }
        Nothing -> 
            History h

forward :: forall a. History a -> History a
forward (History h) =
    case List.uncons h.future of 
        Just { head, tail } -> 
            History $ h { past = head : h.past, current = head, future = tail }
        Nothing -> 
            History h

current :: forall a. History a -> a
current (History { current }) = current

hasPast :: forall a. History a -> Boolean
hasPast (History { past }) = not $ List.null past

hasFuture :: forall a. History a -> Boolean
hasFuture (History { future }) = not $ List.null future

singleton :: forall a. a -> History a
singleton current = 
    History 
        { past : Nil
        , current
        , future : Nil
        }

append :: forall a. a -> History a -> History a
append current (History h) =
    History $ h { past = h.current : h.past, current = current, future = Nil }