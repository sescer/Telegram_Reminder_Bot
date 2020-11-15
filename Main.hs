{-# LANGUAGE OverloadedStrings #-}

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import           Data.Text                (Text, isPrefixOf, words, unwords, pack, unpack, unlines)
import           Data.Time
import           Control.Monad.IO.Class
import           Control.Concurrent

token = Token "bot1056873657:AAF6cxffiubyd6IhGtYkr1832y74E6dIBQE"

fmtTime :: ZonedTime -> String -- Форматирование времени
fmtTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
prsTime :: TimeZone -> String -> Maybe ZonedTime -- Парсинг времени
prsTime tz s = do
    let t = (parseTime defaultTimeLocale "%Y-%m-%d %H:%M" s) :: Maybe LocalTime
    case t of
        Just t -> Just (ZonedTime t tz)
        otherwise -> Nothing

delayUntil :: ZonedTime -> IO ()
delayUntil time = do
    now <- getZonedTime -- Текущее время
    let diff = toRational (diffUTCTime (zonedTimeToUTC time) (zonedTimeToUTC now)) -- Разница между текущим и указанным как Rational
    if diff > 0
        then do -- Если нужно ждать
            let a = (1000 * 1e6)  -- 1000 сек в микросекундах
            let b = (diff * 1e6) -- Время до указанного в микросекундах
            threadDelay (floor (min a b) :: Int) --- Ждем минимальное из них (т.к. слишком много микросекунд в Int не влезет)
            delayUntil time -- И ждем еще раз, на случай если ждать нужно больше 1000 сек
        else do
            return ()

reminder :: Maybe ZonedTime -> ChatId -> Text -> IO ()
reminder (Just t) cid text = do
    delayUntil t -- Ждем указанного времени
    manager <- newManager tlsManagerSettings
    result <- runTelegramClient token manager $ do -- Еще раз создаем монаду TelegramClient (т.к. мы из нее перешли в IO и к ней доступа уже нет)
        sendMessageM (sendMessageRequest cid text) -- Отправляем указанное сообщение
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
onPrivateMessage Message {text = Nothing} = return () -- Если текста нет выходим
onPrivateMessage Message {text = Just text, chat = Chat {chat_id = chat_id}}
    | isPrefixOf "/reminder " text = do -- Если начинается с reminder
        let parts = tail $ Data.Text.words text --- Разделяем на слова и убираем reminder (words разделяет на слова)
        tz <- liftIO getCurrentTimeZone
        let t = prsTime tz $ unpack $ Data.Text.unwords (take 2 parts) --- Парсим время (unwords соединяет слова через пробел) (unpack превращает Text в String)
        let text = Data.Text.unwords (drop 2 parts) -- Текст напоминания
        sendMessageM (sendMessageRequest (ChatId chat_id) "OK, I will remind you")
        liftIO $ forkIO (reminder t (ChatId chat_id) text) -- Запускаем поток с функцией reminder в монаде IO
        return ()
    | text == "/time" = do
        now <- liftIO getZonedTime -- Получаем текущее время
        let time = pack $ fmtTime now -- Выводим его в строку
        sendMessageM (sendMessageRequest (ChatId chat_id) time) -- Отправляем в ответ
        return ()
    | text == "/start" = do
        sendMessageM (sendMessageRequest (ChatId chat_id) startMessage)
        return ()
    | otherwise = return ()

processUpdate :: Update -> TelegramClient ()
processUpdate Update {message = Just msg} = case msg of -- Обрабатываем обновления типа message
    Message {chat = Chat {chat_type = Private}} -> onPrivateMessage msg -- Если оно из приватного чата то передаем в функцию
    otherwise -> return ()
processUpdate _ = return ()

forUpdates :: [Update] -> TelegramClient () --- Просто цикл по обновлениям
forUpdates [] = return ()
forUpdates (u:us) = do
    processUpdate u
    forUpdates us

getUpdateId Update {update_id = uid} = uid -- Маппинг обновления в его id

loop :: Maybe Int -> TelegramClient ()
loop offset = do
    result <- getUpdatesM (GetUpdatesRequest offset Nothing (Just 15) Nothing) -- Получаем обновления с ожиданием 15 сек если их нет
    let Response {result = updates} = result
    forUpdates updates -- Обрабатываем обновления
    case updates of
        []        -> loop Nothing -- Получаем все обновления (если ничего лишнего и так нет)
        otherwise -> loop newOffset -- Получаем обновления только те обновления, которых еще не получили
            where
                maxId = maximum (map getUpdateId updates) -- Находим максимальный id обновления
                newOffset = Just (1 + maxId) -- Новый offset для получения обновлений

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    result <- runTelegramClient token manager $ do -- Вызываем функцию в монаде TelegramClient
        Response {result = me} <- getMeM -- Получаем информацию о себе
        liftIO $ print me -- И выводим её
        loop Nothing -- Запускаем цикл обработки событий
    print result
    print "done!"
