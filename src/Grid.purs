module Grid where

import Prelude

import CSS (class Val, CSS, Value, fromString, key)
import Data.Generic.Rep (class Generic)

newtype GridRow = GridRow Value

derive instance eqGridRow :: Eq GridRow
derive instance ordGridRow:: Ord GridRow
derive instance genericGridRow :: Generic GridRow _
instance valGridRow :: Val GridRow where
  value (GridRow v) = v

gridRowN :: Int -> GridRow
gridRowN n = GridRow $ fromString $ show n

gridRow :: GridRow -> CSS
gridRow = key $ fromString "grid-row"


newtype GridColumn = GridColumn Value

derive instance eqGridColumn :: Eq GridColumn
derive instance ordGridColumn:: Ord GridColumn
derive instance genericGridColumn :: Generic GridColumn _
instance valGridColumn :: Val GridColumn where
  value (GridColumn v) = v

gridColumnN :: Int -> GridColumn
gridColumnN n = GridColumn $ fromString $ show n

gridColumn :: GridColumn -> CSS
gridColumn = key $ fromString "grid-column"


newtype JustifySelf = JustifySelf Value

derive instance eqJustifySelf :: Eq JustifySelf
derive instance ordJustifySelf:: Ord JustifySelf
derive instance genericJustifySelf :: Generic JustifySelf _
instance valJustifySelf :: Val JustifySelf where
  value (JustifySelf v) = v

center :: JustifySelf
center = JustifySelf $ fromString "center"

justifySelf :: JustifySelf -> CSS
justifySelf = key $ fromString "justify-self"