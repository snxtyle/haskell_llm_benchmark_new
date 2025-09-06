module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, dayOfWeek, addDays, addGregorianMonthsClip)
import Data.Time (DayOfWeek)

toDayOfWeek :: Weekday -> DayOfWeek
toDayOfWeek = toEnum . fromEnum

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  let firstDay = fromGregorian year month 1
      targetDOW = toDayOfWeek weekday
      firstDOW = dayOfWeek firstDay
      offset = (fromEnum targetDOW - fromEnum firstDOW) `mod` 7
      candidate = addDays (toInteger offset) firstDay
  in case schedule of
       First -> candidate
       Second -> addDays 7 candidate
       Third -> addDays 14 candidate
       Fourth -> addDays 21 candidate
       Last ->
         let nextMonth = addGregorianMonthsClip 1 firstDay
             lastDay = addDays (-1) nextMonth
             lastDOW = dayOfWeek lastDay
             backOffset = (fromEnum lastDOW - fromEnum targetDOW) `mod` 7
         in addDays (- (toInteger backOffset)) lastDay
       Teenth ->
         let teenthStart = fromGregorian year month 13
             teenthDOW = dayOfWeek teenthStart
             offsetT = (fromEnum targetDOW - fromEnum teenthDOW) `mod` 7
         in addDays (toInteger offsetT) teenthStart
