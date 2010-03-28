
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

 This file contains various general purpose functions and types used
 in the parser for Toycc.

> module ParserUtils (ParserState,
>                     ParserMonad,
>                     Ctype(..),
>                     ParseDirection(..),
>                     VarDecl(..),
>                     makeEmptyParserState,
>                     lookupParserState,
>                     showParserState,
>                     addType)
> where

> import Control.Monad.State
> import Control.Monad.Error
> import Control.Monad.Error.Class
> import Control.Monad.Identity
> import qualified Data.Map as Map
> import ToyccUtils
> import Data.List
> import Lexer

 Time to define a representation for C types. A C type (in the
 simplified version that we use) can be either int or void, an array
 with an unspecified size, an array with a specified size, a pointer
 or a function returning a type.

> data Ctype = PrimitiveInt
>            | PrimitiveVoid
>            | VarArray Ctype
>            | Array Int Ctype
>            | Pointer Ctype
>            | Function [Ctype] Ctype
>            | Struct [VarDecl]

 Defining Ctype as a Show instance explicitly gives more readable
 results (and could be used to compare outputs to cdecl results).

> instance Show Ctype where
>     show PrimitiveVoid = "void"
>     show PrimitiveInt = "int"
>     show (VarArray ct) = "array of " ++ show ct
>     show (Array num ct) = "array of " ++ (show num) ++ " " ++ (show ct)
>     show (Pointer ct) = "pointer to " ++ (show ct)
>     show (Function args ret)
>         | null args = "function with no arguments returning " ++ (show ret)
>         | otherwise = "function with arguments " ++ concatWithinParens args ++
>                       " returning " ++ (show ret)
>     show (Struct members) = "struct with members " ++ concatWithinParens members

> concatWithinParens args = "(" ++ intercalate ", " (map show args) ++ ")"

 The VarDecl type is used to tag (identifier, type) pairs.

> data VarDecl = VarDecl CtokenWithPos Ctype

> instance Show VarDecl where
>     show (VarDecl (token -> ident) theType) =
>         case ident of
>          Identifier theIdent -> showVarDecl theIdent theType

>          otherwise -> error "internal error 2"

 The implementation of show for VarDecls

> showVarDecl theIdent theType =
>     let theVar = if theIdent /= ""
>                  then theIdent
>                  else "anonymous variable"
>         typeStr = show theType
>         article (x:xs) | x `elem` "aeiouy" = "an"
>                        | otherwise = "a"
>     in (theVar ++ " is " ++ (article typeStr) ++ " " ++ typeStr)

 Since the parsing of a variable declaration requires parsing both
 forwards and backwards, we define a type to represent the direction
 (to improve readability)

> data ParseDirection = Forward | Backward

 Parser state needs to be abstracted away,

> type ParserStateDictionary = Map.Map String Ctype

> type ParserState = Map.Map String ParserStateDictionary

> showParserState :: ParserState -> String
> showParserState ps =
>     case maybeDicts of
>                     Just (structs, typedefs) ->
>                         let typedefsString = putTypes "Typedefs:\n" typedefs
>                             structsString = putTypes "Structs:\n" structs
>                         in typedefsString ++ structsString
>                     Nothing -> error "invalid parser state"
>         where maybeDicts = do structs <- Map.lookup "structs" ps
>                               typedefs <- Map.lookup "typedefs" ps
>                               return (structs, typedefs)
>               putTypes prompt types = let typeList = Map.toList types
>                                           stringTypedefs = map (\(ident, def) -> showVarDecl ident def) typeList
>                                       in prompt ++ (intercalate "\n" stringTypedefs) ++ "\n"

 used in a parser monad,

> type ParserMonad a = ErrorState String ParserState a

 and accessed using an API.

 Create an empty parser state.

> makeEmptyParserState = Map.fromList [("typedefs", Map.empty),
>                                      ("structs", Map.empty)]

 Lookup a value into one of the dictionaries of the parser state. For
 now, the dictSelector can be one of "typedefs" or "structs"

> lookupParserState :: String -> String -> ParserMonad Ctype
> lookupParserState dictSelector typeName = do
>                                    parserState <- get
>                                    case (Map.lookup dictSelector parserState) of
>                                           Just dict -> case Map.lookup typeName dict of
>                                                             Nothing -> throwError "9"
>                                                             Just theType -> return theType
>                                           Nothing -> error "internal error 1"

 Adds one named type definition to the parser state. dictSelector can
 be either "typedefs" or "structs".

> addType :: String -> String -> Ctype -> ParserMonad ()
> addType dictSelector typeName typeDefinition = modify changeState
>     where changeState ps = case (Map.lookup dictSelector ps) of
>                              Nothing -> error "internal error 1"
>                              Just dict -> let newDict = Map.insert typeName typeDefinition dict
>                                           in Map.insert dictSelector newDict ps


