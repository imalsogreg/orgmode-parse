-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Time
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode active and inactive timestamps.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.OrgMode.Parse.Attoparsec.Time
( parseTimestampLine
, parseTimestamp
, scheduleType
)
where

import           Control.Applicative        ((*>), (<$>), (<*), (<|>))
import qualified Data.Attoparsec.ByteString as AB
import           Data.Attoparsec.Text       as T
import           Data.Attoparsec.Types      as TP (Parser)
import           Data.Maybe                 (isJust)
import           Data.Text                  as Text (Text, isPrefixOf, unwords,
                                                     words)
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Thyme.Format          (buildTime, timeParser)
import           Prelude                    hiding (concat, null, takeWhile,
                                             unwords, words)
import           System.Locale              (defaultTimeLocale)

import           Data.OrgMode.Parse.Types

-- | Parse an org-mode timestamp (eg "[2015-03-21 Sat 09:45]") with
-- user-supplied opening and ending brackets
parseTimestamp :: Open -> Close -> TP.Parser Text Timestamp
parseTimestamp (Open s) (Close e) = do
  timeString <- (char s *>
                 T.takeTill (== e)
                 <* T.many' (skipSpace *> parseRecur)
                 <* char e)
  case AB.parseOnly (timeParser1 <|> timeParser2) (encodeUtf8 timeString) of
    Left err -> fail err
    Right t  -> return (activeState $ buildTime t)
  where
    activeState = if s == '<' && e == '>'
                  then Active else Inactive
    timeParser1 = timeParser defaultTimeLocale "%Y-%m-%d %a %H:%M"
    timeParser2 = timeParser defaultTimeLocale "%Y-%m-%d %a"
    parseRecur  = T.char '+' *> T.many' T.digit *> T.takeWhile (T.inClass "ymdwhm") -- TODO


-- | Parse an org-mode timestamp line with user-supplied opening and
-- ending brackets (for either active or inactive stamps).
parseTimestampLine :: Open -> Close -> TP.Parser Text (Maybe Schedule)
parseTimestampLine (Open s) (Close e) = do
    s'    <- skipSpace *> scheduleType
    stamp <- skipSpace *> char s *> takeTill (== e) <* char e

    let parts  = words stamp
        r      = if recur parts then Just $ last parts else Nothing
        parsed = parseStamp parts
        stamp' = if s == '<' then Active <$> parsed else Inactive <$> parsed
        sched  = Schedule s' stamp' r

    if isJust $ timestamp sched
    then return $ Just sched
    else return Nothing

  where
    recur p = isPrefixOf "+" $ last p

    stitch p | recur p   = unwords $ init p
             | otherwise = unwords p

    timeParser1 = timeParser defaultTimeLocale "%Y-%m-%d %a %H:%M"
    timeParser2 = timeParser defaultTimeLocale "%Y-%m-%d %a"

    parseStamp p = either (return Nothing) (fmap buildTime) $
      AB.parseOnly
        (option Nothing (Just <$> (timeParser1 <|> timeParser2)))
        (encodeUtf8 $ stitch p)

-- | Parse the type of schedule.
scheduleType :: TP.Parser Text ScheduleType
scheduleType = do
    sd <- option Nothing sched <* skipWhile (== ':')
    return $ case sd of
        Just "DEADLINE"  -> DEADLINE
        Just "SCHEDULED" -> SCHEDULED
        Just _           -> APPOINTMENT
        Nothing          -> APPOINTMENT
  where
    sched = Just <$> (string "DEADLINE" <|> string "SCHEDULED")
