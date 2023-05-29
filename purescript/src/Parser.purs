module Parser
  ( ParserT
  , Error (..)
  , ParserState
  , runParser
  , satisfy
  , char,
  string,
    oneOf,
    many1,
    letter,
    manyTill,
    whitespace,
    skipMany,
    endOfLine,
    peek,
    skipWhile,
    takeWhile,
    takeTill,
    newline,
    anyChar
  )
where

import Prelude
  ( class Eq
  , class Show
  , class Functor
  , class Apply
  , class Applicative
  , class Bind
  , class Monad
  , Unit
  , Void
  , (==)
  , ($)
  , (>>=)
  , (<<<)
  , (/=)
  , (<>)
  , (+)
  , (||)
  , (*>)
  , (&&)
  , ($>)
  , (<$>)
  , (-)
  , pure
  , unit
  , map
  , bind
  , discard
  , const
  , mempty
  , not
  , void
  )
import Control.Apply (lift2)
import Control.Lazy (class Lazy, fix)
import Control.Alt (class Alt, (<|>))
import Control.Plus (class Plus)
import Control.Alternative (class Alternative)
import Control.Monad.Rec.Class (class MonadRec, Step(Loop, Done))
import Control.Monad.Trans.Class (lift)
import Control.Monad.State
  ( class MonadState
  , StateT(StateT)
  , runStateT
  , get
  , put
  , modify
  , modify_
  )
import Data.List
  ( List(Nil)
  , (:)
  , head
  , filter
  , manyRec
  )
import Data.Array (fromFoldable)
import Data.Maybe (Maybe (Nothing, Just))
import Data.Either (Either (Left, Right))
import Data.Tuple (Tuple(Tuple))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.CodePoint.Unicode (isLetter, isSpace)
import Data.String.CodeUnits (fromCharArray)
import Data.String.CodePoints (codePointFromChar)
import Data.Traversable (traverse)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Foldable (fold, elem)
import Data.Function.Uncurried (mkFn2, mkFn3, mkFn5, runFn2, runFn3, runFn5)

data Error i e
  = EndOfInput
  | Illegal i
  | CustomError e
  | Empty

derive instance (Eq i, Eq e) => Eq (Error i e)
--derive instance Generic (Error i e) _
--instance Show (Error i e) where
--  show = genericShow

newtype ParserT i e a = ParserT (StateT (ParserState i) (Either (List (Error i e))) a)


derive instance Newtype (ParserT i e a) _

instance MonadRec (ParserT i e) where
  tailRecM next initArg = ParserT $ StateT ?q 

type ParserState i = 
  { input :: List i,
    position :: Int,
    ch :: Maybe i
  }

--derive instance Eq (ParserState i)
--derive instance Generic (ParserState i) _
--instance Show (ParserState i) where
--  show s = genericShow s

defaultParserState :: List i -> ParserState i
defaultParserState i =
    { input:  i,
      position:  0,
      ch:  head i
    }

runParser ::
  forall i e a.
  ParserT i e a ->
  List i ->
  Either (List Error i e) (Tuple a (ParserState i))
runParser { runParserT } i = runStateT runParserT (defaultParserState i)

instance Functor (ParserT i e) where
  map f parser = wrap $ map f $ unwrap parser

instance Apply (ParserT i e) where
  apply (ParserT fn) (ParserT parser) = ParserT $ StateT $ \state -> do
    (Tuple atob state') <- runStateT fn state
    (Tuple a state'') <- runStateT parser state'
    pure (Tuple (atob a) state'')

instance Applicative (ParserT i e) where
  pure a = ParserT $ pure a

instance Bind (ParserT i e) where
  bind (ParserT x) y = ParserT ( x >>= \a -> unwrap (y a) )

instance Monad (ParserT i e)

instance (Eq i, Eq e) => Alt (ParserT i e) where
  alt (ParserT leftParser) (ParserT rightParser) = ParserT $ StateT $ \state ->
    case runStateT leftParser state of
      Left err ->
        case runStateT rightParser state of
          Left err' -> Left $ uniq $ err <> err'
          Right (Tuple output state') -> Right (Tuple output state')
      Right (Tuple output state') -> Right (Tuple output state')
    where
      uniq Nil = Nil
      uniq (x : xs) = x : uniq ((filter ((/=) x) xs))

instance (Eq i, Eq e) => Plus (ParserT i e) where
  empty = wrap $ StateT $ \_state -> Left (Empty:Nil)

instance (Eq i, Eq e) => Alternative (ParserT i e)

instance MonadState (ParserState i) (ParserT i e) where
  state fn = wrap $ StateT \s -> Right (fn s)
  --state fn = ?f 
  --get = ParserT get
  --put = ParserT <<< put

satisfy :: forall i e. (i -> Boolean) -> ParserT i e i
satisfy pred = ParserT $ do
  s <- get
  case s.input of
    Nil              -> lift $ Left (EndOfInput:Nil)
    c : cs | pred c -> do
                        modify_
                          ( \ss ->
                               ss 
                                  { input = cs,
                                    position = ss.position + 1,
                                    ch = head ss.input
                                  }
                          )
                        lift $ Right c
    c : _          -> lift $ Left (Illegal c:Nil)

char :: Eq i => i -> ParserT i e i
char i = satisfy ((==) i)

string :: Eq i => List i -> ParserT i e (List i)
string characters = traverse char characters

oneOf :: forall i e. Eq i => List i -> ParserT i e i
oneOf str = satisfy (\char' -> char' `elem` str)

many1 :: forall a f. Alternative f => MonadRec f => f a -> f (List a)
many1 parser = lift2 (:) parser (manyRec parser)

letter :: forall e. ParserT Char e Char
letter = satisfy (\c -> isLetter (codePointFromChar c) || c == '_')

manyTill :: forall a b f. Alternative f => Lazy (f (List a)) => f a -> f b -> f (List a)
manyTill parser end = fix scan
  where
    scan self =
      (end *> pure Nil) <|> lift2 (:) parser self

skipMany :: forall f a. Alternative f => Lazy (f Unit) => f a -> f Unit 
skipMany parser = fix scan
  where
    scan self = (parser *> self) <|> pure unit 

peek :: forall i e. ParserT i e (Maybe i)
peek = ParserT $ do
  parserState <- get
  case parserState.input of
    Nil -> do
      modify_ (\state -> state {input = Nil})
      pure Nothing
    a : b : rest -> do
      modify_ (\state -> state {input = a : b : rest})
      pure $ Just b
    rest -> do
      modify_ (\state -> state {input = rest})
      pure Nothing

whitespace :: forall e. Eq e => ParserT Char e String
whitespace = fromListCharToString
  <$> many1 (satisfy $ isSpace <<< codePointFromChar)

endOfLine :: ParserT Char Void Unit
endOfLine = (char '\n' *> pure unit) <|> (string "\r\n" *> pure unit)

newline :: ParserT Char Void Char
newline = char '\n'

skipWhile :: forall e. (Char -> Boolean) -> ParserT Char e Unit
skipWhile pred = do
  parserState <- get
  go parserState.input
  where
    go (c : cs) | pred c && cs /= mempty= do
        modify_ (\state -> state {input = cs})
        go cs
    go _ = pure unit

fromListCharToString :: List Char -> String
fromListCharToString = fromCharArray <<< fromFoldable

takeWhile :: forall e. (Char -> Boolean) -> ParserT Char e String
takeWhile pred = do
  s <- get
  fromListCharToString <$> go s.input
  where
    go s@Nil = pure s
    go (c : cs) =
      if pred c
        then do
          --modify (\state -> state {input = [c]})
          go cs
        else do
          --modify (\state -> state {input = cs})
          pure (c:Nil)

takeTill :: forall e. (Char -> Boolean) -> ParserT Char e String
takeTill pred = takeWhile (not <<< pred)

anyChar :: ParserT i e i
anyChar = satisfy (const true)
