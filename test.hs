import Data.UnixTime as Unix
import System.Time
getUTC x = toUTCTime (Unix.toClockTime (fromEpochTime x))
