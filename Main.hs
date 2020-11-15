{-# LANGUAGE OverloadedStrings #-}

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import           Data.Text                (Text, isPrefixOf, words, unwords, pack, unpack, unlines)
import           Data.Time
import           Control.Monad.IO.Class
import           Control.Concurrent

token = Token "" --You can get token from @botfarther in Telegram

fmtTime :: ZonedTime -> String --Time formatting
fmtTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
prsTime :: TimeZone -> String -> Maybe ZonedTime -- Time parsing
prsTime tz s = do
    let t = (parseTime defaultTimeLocale "%Y-%m-%d %H:%M" s) :: Maybe LocalTime
    case t of
        Just t -> Just (ZonedTime t tz)
        otherwise -> Nothing

delayUntil :: ZonedTime -> IO ()
delayUntil time = do
    now <- getZonedTime -- Current time
    let diff = toRational (diffUTCTime (zonedTimeToUTC time) (zonedTimeToUTC now)) -- Difference between current and listed as Rational
    if diff > 0
        then do -- Если нужно ждать
            let a = (1000 * 1e6)  -- 1000 sec to microseconds
            let b = (diff * 1e6) -- Time to specified in microseconds
            threadDelay (floor (min a b) :: Int) -- We are waiting for the minimum of them (because too many microseconds will not fit into Int)
            delayUntil time -- And we are waiting again, in case you need to wait more than 1000 seconds
        else do
            return ()

reminder :: Maybe ZonedTime -> ChatId -> Text -> IO ()
reminder (Just t) cid text = do
    delayUntil t -- Ждем указанного времени
    manager <- newManager tlsManagerSettings
    result <- runTelegramClient token manager $ do -- Once again, we create the TelegramClient monad (since we switched from it to IO and there is no longer access to it)
        sendMessageM (sendMessageRequest cid text) -- We send the specified message
    return ()
reminder _ _ _ = return ()

startMessage :: Text
startMessage = Data.Text.unlines
 [ "Hello! My name is Kazuki! And my profession is to remind you to do smth :)"
 , ""
 , "- Just type what you need to do an I'll do it!"
 , "- Use /time  to see current time"
 , "- Use /reminder YYYY-MM-DD hh:mm action - and I will remind you to do this action at this time"
 , "where:"
 , "           YYYY - year"
 , "           MM - month"
 , "           DD - day"
 , "           hh - hours"
 , "           mm - minutes"
 , "           action - smth that I will print to remind you" 
 , ""
 , "For example:"
 , "[user] (Message from 14 March 2020 at 12:14): /reminder 2020-03-17 17:20 wash the car, go to the bank, buy products: milk, bread, oranges and apples"
 , "[KazukiBot](Message from 14 March 2020 at 12:14): Ok, I will remind you"
 , "[KazukiBot](Message from 17 March 2020 at 17:20):wash the car, go to the bank, buy products: milk, bread, oranges and apples "
 , ""
 , "So I'm ready for your commands, my friend :)"
 ]



onPrivateMessage :: Message -> TelegramClient ()
onPrivateMessage Message {text = Nothing} = return () -- If there is no text, exit
onPrivateMessage Message {text = Just text, chat = Chat {chat_id = chat_id}}
    | isPrefixOf "/reminder " text = do -- If it starts with a "/reminder"
        let parts = tail $ Data.Text.words text --- Divide into words and remove reminder (words divide into words)
        tz <- liftIO getCurrentTimeZone
        let t = prsTime tz $ unpack $ Data.Text.unwords (take 2 parts) --- Parse time (unwords concatenate words separated by space) (unpack turns Text into String)
        let text = Data.Text.unwords (drop 2 parts) -- Reminder text
        sendMessageM (sendMessageRequest (ChatId chat_id) "OK, I will remind you")
        liftIO $ forkIO (reminder t (ChatId chat_id) text) -- We start a thread with the reminder function in the IO monad
        return ()
    | text == "/time" = do
        now <- liftIO getZonedTime -- Get the current time
        let time = pack $ fmtTime now -- Ooutput it to the line
        sendMessageM (sendMessageRequest (ChatId chat_id) time) -- Send back
        return ()
    | text == "/start" = do
        sendMessageM (sendMessageRequest (ChatId chat_id) startMessage)
        return ()
    | otherwise = return ()

processUpdate :: Update -> TelegramClient ()
processUpdate Update {message = Just msg} = case msg of -- Processing updates of the message type
    Message {chat = Chat {chat_type = Private}} -> onPrivateMessage msg -- If it is from a private chat, then we pass it to the function
    otherwise -> return ()
processUpdate _ = return ()

forUpdates :: [Update] -> TelegramClient () --- Just a cycle of updates
forUpdates [] = return ()
forUpdates (u:us) = do
    processUpdate u
    forUpdates us

getUpdateId Update {update_id = uid} = uid -- Mapping update to its id

loop :: Maybe Int -> TelegramClient ()
loop offset = do
    result <- getUpdatesM (GetUpdatesRequest offset Nothing (Just 15) Nothing) -- Receive updates with a wait of 15 seconds if they are not.
    let Response {result = updates} = result
    forUpdates updates -- Processing updates
    case updates of
        []        -> loop Nothing -- Receive all updates (if there is nothing superfluous and so there is nothing)
        otherwise -> loop newOffset -- Receive updates only those updates that have not yet been received
            where
                maxId = maximum (map getUpdateId updates) -- Find the maximum update id
                newOffset = Just (1 + maxId) -- New offset to receive updates

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    result <- runTelegramClient token manager $ do -- Calling a function in the TelegramClient monad
        Response {result = me} <- getMeM -- Receive information about ourselves
        liftIO $ print me -- And take it out
        loop Nothing -- Starting the event loop
    print result
    print "done!"
