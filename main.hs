{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Maybe (fromMaybe)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import System.Console.Readline (readline)

data ItemState
  = TODO
  | INPROG
  | DONE
  deriving (Show, Read, Eq)

data ItemField = ItemField
  { dbId :: Int
  , dbState :: String
  , dbData :: String
  } deriving (Show)

instance FromRow ItemField where
  fromRow = ItemField <$> field <*> field <*> field

data Item = Item
  { itemData :: String
  , itemState :: ItemState
  , itemId :: Int
  }

instance Show Item where
  show (Item itD itS _) = "<" ++ show itS ++ "> " ++ show itD

type Items = [Item]

data Command
  = Quit
  | DisplayItems
  | AddItem Item
  | InProg Int
  | Done Int
  | Undo Int
  | Delete Int
  | DeleteAllDone
  | Help

helpMsg :: String
helpMsg =
  "Commands: help, quit, items, add - <item to add>,\n" ++
  "          inProg <item to mark in progress>,\n" ++
  "          done <item to mark as done>, undo <item to mark as todo>,\n" ++
  "          delete <item to delete>, delAllDone."

indexErr :: String
indexErr = "Index out of bounds."

parseCommand :: String -> Either String Command
parseCommand line =
  case words line of
    ["quit"] -> Right Quit
    ["items"] -> Right DisplayItems
    ["inProg", i] -> resp [i] InProg
    ["done", i] -> resp [i] Done
    ["undo", i] -> resp [i] Undo
    ["delete", i] -> resp [i] Delete
    ["delAllDone"] -> Right DeleteAllDone
    ["help"] -> Right Help
    -- for add, itemId is set after the db operation
    "add":"-":itD -> Right (AddItem (Item (unwords itD) TODO 0))
    _ -> Left "Unknown command."
  where
    resp i c =
      if all (\l -> elem l [show i | i <- [0 .. 9]]) i
        then Right (c (read $ head i))
        else Left "Invalid index."

addItem :: Item -> Items -> Items
addItem item items = item : items

displayItems :: Items -> String
displayItems items =
  let displayItem index item = show index ++ " - " ++ show item
      reversedList = reverse items
      displayItemsList = zipWith displayItem [1 ..] reversedList
   in unlines displayItemsList

markItemState :: Int -> ItemState -> Items -> Either String (Items, Int)
markItemState index ist items =
  case result of
    Left msg -> Left msg
    Right (f, i, s) ->
      Right (f ++ [Item (itemData i) ist (itemId i)] ++ s, itemId i)
  where
    result = splitAtItemIndex ((length items + 1) - index) items

splitAtItemIndex :: Int -> Items -> Either String ([Item], Item, [Item])
splitAtItemIndex revIndex items
  | revIndex <= 0 || revIndex > (length items) = Left indexErr
  | otherwise = Right (init first, last first, second)
  where
    (first, second) = splitAt revIndex items

deleteAllDone :: Items -> (Items, [Int])
deleteAllDone items = ([e | e <- items, itemState e /= DONE], delItemIds)
  where
    delItemIds = [itemId i | i <- items, itemState i == DONE]

deleteItem :: Int -> Items -> Either String (Items, Int)
deleteItem reverseIndex allItems =
  impl (length allItems - reverseIndex) allItems
  where
    impl index items =
      case (index, items) of
        (0, i:rest)    -> Right (rest, itemId i)
        (_, [])        -> Left indexErr
        (n, item:rest) ->
          case impl (n - 1) rest of
            Right (newItems, iid) -> Right (item : newItems, iid)

newDBItem :: Item -> Int -> Item
newDBItem item iid = Item (itemData item) (itemState item) iid

updateDBItem :: Int -> ItemState -> IO ()
updateDBItem iid its = do
  conn <- open "items.db"
  execute conn "UPDATE items SET itemState=? WHERE id=?" [show its, show iid]
  close conn

delMultipleFromDB :: [Int] -> IO ()
delMultipleFromDB iids =
  case iids of
    [i] -> delFromDB i
    (i:xs) -> do
      delFromDB i
      delMultipleFromDB xs

delFromDB :: Int -> IO ()
delFromDB iid = do
  conn <- open "items.db"
  execute conn "DELETE FROM items WHERE id=?" (Only iid)
  close conn

loadItems :: [ItemField] -> Items
loadItems ifs = [Item (dbData i) (read $ dbState i) (dbId i) | i <- ifs]

addDBItem :: Item -> IO Int
addDBItem item = do
  conn <- open "items.db"
  executeMany
    conn
    "INSERT INTO items (itemState, itemData) VALUES (?, ?)"
    ([(show (itemState item), itemData item)] :: [(String, String)])
  iid <- lastInsertRowId conn
  close conn
  return (fromIntegral iid)

-- Takes a list of items
-- Interact with the user
-- Return an updated list of items
interactWithUser :: Items -> IO ()
interactWithUser items = do
  maybeLine <- readline ": "
  let line = fromMaybe "" maybeLine
  case parseCommand line of
    Right DisplayItems -> do
      putStrLn "The list of items is:"
      putStrLn (displayItems items)
      interactWithUser items
    Right (AddItem item) -> do
      iid <- addDBItem item
      let newItems = addItem (newDBItem item iid) items
      putStrLn "Item added."
      interactWithUser newItems
    Right Quit -> do
      putStrLn "Bye"
      pure ()
    Right (Delete index) -> do
      let result = deleteItem index items
      case result of
        Left errMsg -> do
          putStrLn ("Error: " ++ errMsg)
          interactWithUser items
        Right (newItems, iid) -> do
          delFromDB iid
          putStrLn "Item deleted."
          interactWithUser newItems
    Right (InProg index) -> do
      let result = markItemState index INPROG items
      case result of
        Left errMsg -> do
          putStrLn ("Error: " ++ errMsg)
          interactWithUser items
        Right (newItems, iid) -> do
          putStrLn "Item marked in progress."
          updateDBItem iid INPROG
          interactWithUser newItems
    Right (Done index) -> do
      let result = markItemState index DONE items
      case result of
        Left errMsg -> do
          putStrLn ("Error: " ++ errMsg)
          interactWithUser items
        Right (newItems, iid) -> do
          putStrLn "Item marked done."
          updateDBItem iid DONE
          interactWithUser newItems
    Right (Undo index) -> do
      let result = markItemState index TODO items
      case result of
        Left errMsg -> do
          putStrLn ("Error: " ++ errMsg)
          interactWithUser items
        Right (newItems, iid) -> do
          updateDBItem iid TODO
          putStrLn "Item marked todo."
          interactWithUser newItems
    Right DeleteAllDone -> do
      let (result, delItemIds) = deleteAllDone items
      delMultipleFromDB delItemIds
      putStrLn "Deleted all done items."
      interactWithUser result
    Right Help -> do
      putStrLn helpMsg
      interactWithUser items
    Left errMsg -> do
      putStrLn ("Error: " ++ errMsg)
      interactWithUser items

main :: IO ()
main = do
  putStrLn "TODO app"
  putStrLn helpMsg
  conn <- open "items.db"
  r <- query_ conn "SELECT * FROM items" :: IO [ItemField]
  close conn
  let initialList = loadItems r
  interactWithUser initialList
  putStrLn "Thank you."
