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
insert = undefined

insertList :: [Word] -> Trie 
insertList = undefined

search :: Word -> Trie -> Bool 
search []       t = end t
search (x:xs)   t = case M.lookup x (children t) of
                    Nothing -> False
                    Just t' -> search xs t'

getWords :: Trie -> [Word] 
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined