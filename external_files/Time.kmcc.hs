{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

import Prelude ((*))
import qualified Prelude as P
import qualified Data.Time.Clock.POSIX as D
import qualified Data.Time.Clock as D
import qualified Data.Time.LocalTime as D
import qualified Data.Time.Calendar as D
import BasicDefinitions

instance ForeignType ClockTime_Det where
  type Foreign ClockTime_Det = D.POSIXTime
  toForeign (CTime_Det t) = P.fromInteger t
  fromForeign t = CTime_Det (P.floor t)

timedotgetClockTime_Det# = D.getPOSIXTime P.>>= \x -> P.return (CTime_Det (P.floor x))
timedotgetClockTime_ND# = P.return (P.fmap from timedotgetClockTime_Det#)

timedotprimuscoretoCalendarTime_Det# (CTime_Det t) = do 
  let utcTime = D.posixSecondsToUTCTime (P.fromInteger t)
  zoneTime <- D.utcToLocalZonedTime utcTime
  let timeZone = P.toInteger (D.timeZoneMinutes (D.zonedTimeZone zoneTime))
  let locTime = D.zonedTimeToLocalTime zoneTime
  let (yr, mnth, day) = D.toGregorian (D.localDay locTime)
  let tod = D.localTimeOfDay locTime
  P.return (CalendarTime_Det yr (P.toInteger mnth) (P.toInteger day) (P.toInteger (D.todHour tod)) (P.toInteger (D.todMin tod)) (P.toInteger (P.floor (D.todSec tod))) (timeZone * 60))
timedotprimuscoretoCalendarTime_ND# = liftConvertIO1 timedotprimuscoretoCalendarTime_Det#

timedotprimuscoretoUTCTime_Det# (CTime_Det t) =
  CalendarTime_Det yr (P.toInteger mnth) (P.toInteger day) (P.toInteger (D.todHour tod)) (P.toInteger (D.todMin tod)) (P.toInteger (P.floor (D.todSec tod))) timeZone
 where
  utcTime = D.posixSecondsToUTCTime (P.fromInteger t)
  locTime = D.utcToLocalTime D.utc utcTime
  timeZone = P.toInteger (D.timeZoneMinutes D.utc)
  tod = D.localTimeOfDay locTime
  (yr, mnth, day) = D.toGregorian (D.localDay locTime)
timedotprimuscoretoUTCTime_ND# = liftConvert1 timedotprimuscoretoUTCTime_Det#

timedotprimuscoretoClockTime_Det# (CalendarTime_Det yr mnth day hr min sec zone) =
  CTime_Det time
 where
  date = D.fromGregorian yr (P.fromInteger mnth) (P.fromInteger day)
  tod = D.TimeOfDay (P.fromInteger hr) (P.fromInteger min) (P.fromInteger sec)
  locTime = D.LocalTime date tod
  timeZone = D.TimeZone (P.fromInteger (zone `P.quot` 60)) P.False ""
  utcTime = D.localTimeToUTC timeZone locTime
  posixTime = D.utcTimeToPOSIXSeconds utcTime
  time = P.floor posixTime
timedotprimuscoretoClockTime_ND# = liftConvert1 timedotprimuscoretoClockTime_Det#