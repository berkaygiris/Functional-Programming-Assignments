import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie} deriving (Show)
type Word = String

empty :: Trie
empty = Trie { end = False, children = M.empty }

insert :: Word -> Trie -> Trie
insert []     t = t { end = True }
insert (x:xs) t = let ts = children t
                  in case M.lookup x ts of
                         Nothing -> t { children = M.insert x (insert xs empty) ts }
                         Just t' -> t { children = M.insert x (insert xs t') ts }

insertList :: [Word] -> Trie 
insertList list = foldr insert empty list

search :: Word -> Trie -> Bool 
search []       t = end t
search (x:xs)   t = case M.lookup x (children t) of
                    Nothing -> False
                    Just t' -> search xs t'

getWords :: Trie -> [Word] 
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined

menuItems :: [String]
menuItems = ["a) Add Word",
             "s) Search Word",
             "f) Find words with prefix",
             "p) Print all words",
             "e) Exit",
             "Enter the action:"]

main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let line = lines content
    let t = insertList line
    mapM_ print menuItems