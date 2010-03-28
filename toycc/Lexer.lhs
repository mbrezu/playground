
 Toy C Compiler - a learning project that should eventually do what
 its name says.
 Copyright (C) 2008 Miron Brezuleanu

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.

 See the COPYING file for the full text of the license.

 -----------------------------------------------------------------------------

 Lexer module for Toycc.

> module Lexer (Ctoken(..),
>               CtokenWithPos(..),
>               FilePos(..),
>               lexer,
>               token2String,
>               printTokens)
> where

 Some necessary imports:

> import Data.Char
> import Data.List
> import Control.Monad.State
> import Control.Monad.Error
> import Control.Monad.Error.Class
> import Control.Monad.Identity
> import ToyccUtils

 First, we need a lexer (although rudimentary) because operating
 directly on characters can be tedious. The possible tokens are
 described below:

> data Ctoken = Identifier String
>             | KeywordInt
>             | KeywordVoid
>             | OpenParenthesis
>             | CloseParenthesis
>             | Star
>             | OpenSqParenthesis
>             | CloseSqParenthesis
>             | Semicolon
>             | Comma
>             | Number Int
>             | Plus
>             | PlusPlus
>             | Minus
>             | MinusMinus
>             | OpenBrace
>             | CloseBrace
>             | Equal
>             | EqualEqual
>             | KeywordIf
>             | KeywordFor
>             | KeywordStruct
>             | KeywordTypedef
>               deriving (Show, Eq)

 The rudimentary lexer that turns a list of characters (a string) into
 a list of tokens:

> data FilePos = FilePos { line :: Int, column :: Int }
>                deriving Show
> data CtokenWithPos = CtokenWithPos { token :: Ctoken, position :: FilePos }
>                      deriving Show

> data LexerState = LexerState { pos :: FilePos,
>                                rest :: String }

> type LexerMonad a = ErrorState String LexerState a

> lexer :: String -> Either String [CtokenWithPos]
> lexer input = fst $ runErrorState lexerImpl (LexerState (FilePos 1 1) input)

> lexerImpl :: LexerMonad [CtokenWithPos]
> lexerImpl = do ls <- get
>                case rest ls of
>                         [] -> return []
>                         ('*':_) -> lexChars Star 1
>                         (';':_) -> lexChars Semicolon 1
>                         (',':_) -> lexChars Comma 1
>                         ('(':_) -> lexChars OpenParenthesis 1
>                         (')':_) -> lexChars CloseParenthesis 1
>                         ('[':_) -> lexChars OpenSqParenthesis 1
>                         (']':_) -> lexChars CloseSqParenthesis 1
>                         ('{':_) -> lexChars OpenBrace 1
>                         ('}':_) -> lexChars CloseBrace 1
>                         ('+':_) -> lexChars Plus 1
>                         ('+': '+':_) -> lexChars PlusPlus 2
>                         ('-':_) -> lexChars Minus 1
>                         ('-': '-':_) -> lexChars MinusMinus 2
>                         ('=':_) -> lexChars Equal 1
>                         ('=':'=':_) -> lexChars EqualEqual 2
>                         (' ':_) -> lexSkipOne
>                         inp -> if isDigit (head inp)
>                                then lexInteger inp
>                                else if isAlphaNum (head inp) || head inp == '_'
>                                     then lexIdent inp
>                                     else do
>                                       ls <- get
>                                       throwError $ "unknown token " ++ [head inp] ++ " at " ++ (show $ pos ls)

> incLsCol :: Int -> LexerMonad ()
> incLsCol n = do ls <- get
>                 let currentPos = pos ls
>                 put $ ls { pos = currentPos { column = column currentPos + n  } }
> incLine :: LexerMonad ()
> incLine = do ls <- get
>              let currentPos = pos ls
>              put $ ls { pos = currentPos { line = line currentPos + 1,
>                                            column = 1 } }
> consume :: Int -> LexerMonad ()
> consume n = do ls <- get
>                put $ ls { rest = drop n $ rest ls }
> getToken :: Ctoken -> LexerMonad CtokenWithPos
> getToken tokenType = do ls <- get
>                         return $ CtokenWithPos tokenType (pos ls)
> lexChars :: Ctoken -> Int -> LexerMonad [CtokenWithPos]
> lexChars tokenType n = do tk <- getToken tokenType
>                           incLsCol n
>                           consume n
>                           theRest <- lexerImpl
>                           return (tk : theRest)
> lexSkipOne :: LexerMonad [CtokenWithPos]
> lexSkipOne = do incLsCol 1
>                 consume 1
>                 lexerImpl
> lexInteger :: String -> LexerMonad [CtokenWithPos]
> lexInteger inp = do let (num, rest) = span isDigit inp
>                     let numVal = (read num) :: Int
>                     incLsCol $ length num
>                     consume $ length num
>                     ls <- get
>                     let tk = CtokenWithPos (Number numVal) (pos ls)
>                     theRest <- lexerImpl
>                     return (tk : theRest)
> lexIdent :: String -> LexerMonad [CtokenWithPos]
> lexIdent inp = do let isIdentChar c = isAlphaNum c || c == '_'
>                   let (ident, rest) = span isIdentChar inp
>                   let tkType = case ident of
>                                  "int" -> KeywordInt
>                                  "void" -> KeywordVoid
>                                  "if" -> KeywordIf
>                                  "for" -> KeywordFor
>                                  "struct" -> KeywordStruct
>                                  "typedef" -> KeywordTypedef
>                                  _ -> Identifier ident
>                   incLsCol $ length ident
>                   consume $ length ident
>                   ls <- get
>                   let tk = CtokenWithPos tkType (pos ls)
>                   theRest <- lexerImpl
>                   return (tk : theRest)

 A couple of printing helper functions (we need these if we debug the
 lexer) or want to make sure that a list of tokens means what we think
 it means:

> token2String KeywordInt = "int "
> token2String KeywordVoid = "void "
> token2String KeywordStruct = "struct "
> token2String KeywordFor = "for "
> token2String KeywordIf = "if "
> token2String KeywordTypedef = "typedef "
> token2String OpenParenthesis ="("
> token2String CloseParenthesis = ")"
> token2String OpenSqParenthesis = "["
> token2String CloseSqParenthesis = "]"
> token2String Star = "*"
> token2String Semicolon = ";"
> token2String Comma = ","
> token2String (Number n) = show n
> token2String (Identifier i) = i
> token2String OpenBrace = "{"
> token2String CloseBrace = "}"
> token2String Plus = "+"
> token2String PlusPlus = "++"
> token2String Minus = "-"
> token2String MinusMinus = "--"
> token2String Equal = "="
> token2String EqualEqual = "=="

 (I don't instantiate Show for Ctoken because I still want to be able
 to see the token indices)

 The next function renders a string that should be very much like the
 lexer input (the possible differences are caused by different use of
 white space).

> printTokens tokList = concatMap token2String $ map token tokList

