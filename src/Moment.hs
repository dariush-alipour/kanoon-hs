module Moment
( Moment.Moment(..)
) where

data Moment a = 
  Moment { calendarSystemId :: String
         , year :: a
         , month :: a
         , day :: a
         , hour :: a
         , minute :: a
         , second :: a
         } deriving (Eq, Show)
