
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

 Define a parser for simplified C variable declarations.

 View patterns extension required.

 Some necessary imports:

> import Data.Char
> import Data.List
> import Control.Monad.State
> import Control.Monad.Error
> import Control.Monad.Error.Class
> import Lexer
> import ToyccUtils
> import ParserUtils
> import qualified Data.Map as Map

 Simplifications (hmm, maybe there are more, but I'm not aware of the
 rest :-) ):

 a) int and void are the only primitive types;
 b) no const, static, volatile, register, auto;
 c) no error handling on broken input;

 The examples we use for testing at the end of the file should give a
 pretty good idea of what is supported.

 The lexer is defined in module Lexer

 Now the variable declaration parser, which follows the rules from
 http://eli.thegreenplace.net/2008/07/18/reading-c-type-declarations/

 The varDecl function takes a string as input and outputs a tuple of
 the variable name and its type, represented as a Ctype
 instance (which, when 'show'ed, will provide a 'human readable'
 description of the declaration)

> varDecl :: String -> ParserMonad ([VarDecl], [CtokenWithPos])
> varDecl input = do
>               let tokens = lexer input
>               case tokens of
>                 Right tokens' -> parseTypedef tokens'
>                 Left anError -> throwError anError

 'varDeclLoop' and 'varDeclTokens' are the 'workhorse' functions used
 to parse declarations. 'varDeclTokens' is actually only useful when
 parsing function arguments, so... there's only one actual workhorse here

> parseTypedef :: [CtokenWithPos] -> ParserMonad ([VarDecl], [CtokenWithPos])
> parseTypedef tokens = do
>   case (token $ head tokens) of
>     KeywordTypedef -> do
>       (vardecls, after) <- varDeclLoop (tail tokens)
>       forM vardecls addTypedefLocal
>       return ([], after)
>           where addTypedefLocal (VarDecl (token -> (Identifier name)) theType) =
>                              addType "typedefs" name theType
>     _ -> varDeclLoop tokens

> varDeclLoop :: [CtokenWithPos] -> ParserMonad ([VarDecl], [CtokenWithPos])
> varDeclLoop tokens = do
>    (baseType, restOfTokens) <- parseBaseType tokens
>    varDeclLoopImpl restOfTokens baseType []
>        where varDeclLoopImpl input baseType varDecls = do
>                     (varDecl, restOfTokens) <- varDeclTokensWithBaseType input baseType
>                     let varDecls' = varDecl : varDecls
>                     case restOfTokens of
>                              (token -> Comma) : restOfTokens ->
>                                  varDeclLoopImpl restOfTokens baseType varDecls'
>                              _ ->
>                                  return ((reverse varDecls'), restOfTokens)

> varDeclTokens :: [CtokenWithPos] -> ParserMonad (VarDecl, [CtokenWithPos])
> varDeclTokens tokens = do
>    (baseType, restOfTokens) <- parseBaseType tokens
>    varDeclTokensWithBaseType restOfTokens baseType

> parseBaseType :: [CtokenWithPos] -> ParserMonad (Ctype, [CtokenWithPos])
> parseBaseType inputTokens =
>     case (token $ head inputTokens) of
>       KeywordStruct -> do
>             restOfTokens <- consumeToken inputTokens KeywordStruct
>             case token $ head restOfTokens of
>               OpenBrace -> do
>                            restOfTokens' <- consumeToken restOfTokens OpenBrace
>                            (structMembers, restOfTokens'') <- parseStruct restOfTokens' []
>                            return (Struct structMembers, restOfTokens'')
>               Identifier structName  -> do
>                            let restOfTokens' = tail restOfTokens
>                            case token $ head restOfTokens' of
>                              OpenBrace -> do
>                                 let restOfTokens'' = tail restOfTokens'
>                                 (structMembers, restOfTokens''') <- parseStruct restOfTokens'' []
>                                 addType "structs" structName (Struct structMembers)
>                                 return (Struct structMembers, restOfTokens''')
>                              _ -> do
>                                 structType <- lookupParserState "structs" structName
>                                 return (structType, restOfTokens')
>               _ -> throwError "8"
>       KeywordInt ->
>             return (PrimitiveInt, tail inputTokens)
>       KeywordVoid ->
>             return (PrimitiveVoid, tail inputTokens)
>       (Identifier baseType) -> do
>             typedef <- lookupParserState "typedefs" baseType
>             return (typedef, tail inputTokens)
>       _ -> throwError "6"

> varDeclTokensWithBaseType tokens baseType = do
>    (before, ident, after) <- findIdentifier tokens
>    (ctype, after') <- varDeclImpl (baseType, reverse before, after, Forward)
>    return ((VarDecl ident ctype), after')

> parseStruct :: [CtokenWithPos] -> [VarDecl] -> ParserMonad ([VarDecl], [CtokenWithPos])
> parseStruct inputTokens varDeclsSoFar = do
>   (varDecls, restOfTokens) <- varDeclLoop inputTokens
>   let varDeclsSoFar' = (reverse varDecls) ++ varDeclsSoFar
>   if ((token $ head restOfTokens) == CloseBrace)
>     then return (reverse varDeclsSoFar', tail restOfTokens)
>     else parseStruct restOfTokens varDeclsSoFar'

> consumeToken inputTokens tokenToMatch = do
>     when ((token $ head inputTokens) /= tokenToMatch)
>          (throwError $ "expected " ++ (token2String tokenToMatch))
>     return $ tail inputTokens

 there are some helper functions involved; findIdentifier does the initial
 splitting of the input into identifier (the name of the variable
 being declared) and the symbols before and after the identifier:

> findIdentifier :: [CtokenWithPos] -> ParserMonad ([CtokenWithPos],
>                                                   CtokenWithPos,
>                                                   [CtokenWithPos])
> findIdentifier input =
>     let (beforeIdent, afterIncludeIdent) = span (not . isIdent) input
>         isIdent (token -> (Identifier _)) = True
>         isIdent _ = False
>     in if not (null afterIncludeIdent)
>        then return (beforeIdent, head afterIncludeIdent, tail afterIncludeIdent)
>        else return $ splitAbstractDeclarator input

 If there is no variable name in a declaration, we are dealing with an
 abstract declarator. One instance where this can appear is
 int f(int (*)());
 This declares a function that returns an integer and takes a pointer
 to function as argument. Since we're only interested in the type of
 the argument, there is no need for a variable here.

 The rules for parsing were explained on the web page referred
 above. They are used in the splitAbstractDeclarator function. The
 scanToMissingIdentifier implements the search for the abstract
 declarator position in the innermost grouping parens.

> splitAbstractDeclarator :: [CtokenWithPos] -> ([CtokenWithPos], CtokenWithPos, [CtokenWithPos])
> splitAbstractDeclarator input =
>  let (scanStart, useDirectly) = startOfDeepestParenNest (map token input) 0 0 0 0
>  in if useDirectly
>     then let (before, after) = splitAt scanStart input
>          in (before, CtokenWithPos (Identifier "") (position $ head after), after)
>     else let splitPos = scanToMissingIdentifier (drop scanStart input) scanStart
>              (before, after) = splitAt splitPos input
>          in (before, CtokenWithPos (Identifier "") (position $ head after), after)
>      where scanToMissingIdentifier tokens scanStart =
>                case (map token tokens) of
>                  (Star : OpenSqParenthesis : _) -> scanStart + 1
>                  (OpenSqParenthesis : ts) -> scanStart + 1
>                  (CloseParenthesis : ts) -> scanStart
>                  (t:ts) -> scanToMissingIdentifier (tail tokens) (scanStart + 1)
>                  [] -> scanStart

 startOfDeepestParenNest is used to detect the deepest nesting of
 parenthesis. It returns index of the beginning of the deepest nesting
 level. It uses an extra rule to detect cases like
 int *()
 The extra rule is "check for a star before the paren; if you find
 one, you need to split after the star". Not sure if it's correct,
 though.
 The result of the function is a tuple, containing the position and
 whether that's meant to be used directly or the position between a
 '*' and '[' needs to be searched for.

> startOfDeepestParenNest :: [Ctoken] -> Int -> Int -> Int -> Int -> (Int, Bool)
> startOfDeepestParenNest input currentParenLevel maxParenLevel currentPos posOfMax =
>     case input of
>              (Star : OpenParenthesis : ts) -> (currentPos + 1, True)
>              (Star : CloseParenthesis : ts) -> (currentPos + 1, True)
>              (OpenParenthesis : ts) ->
>                if currentParenLevel + 1 > maxParenLevel
>                then startOfDeepestParenNest (tail input)
>                                             (currentParenLevel + 1)
>                                             (currentParenLevel + 1)
>                                             (currentPos + 1)
>                                             (currentPos + 1)
>                else startOfDeepestParenNest (tail input)
>                                             (currentParenLevel + 1)
>                                             maxParenLevel
>                                             (currentPos + 1)
>                                             posOfMax
>              (CloseParenthesis : ts) -> startOfDeepestParenNest (tail input)
>                                                                   (currentParenLevel - 1)
>                                                                   maxParenLevel
>                                                                   (currentPos + 1)
>                                                                   posOfMax
>              (t:ts) -> startOfDeepestParenNest (tail input)
>                                                 currentParenLevel
>                                                 maxParenLevel
>                                                 (currentPos + 1)
>                                                 posOfMax
>              [] -> (posOfMax, False)

 varDeclImpl is the variable declaration parser itself, which
 takes a list of tokens before the identifier (reversed, so we can use
 'head' and 'tail' instead of 'last' and 'init', as it would be the
 case if we kept the initial order), a list of tokens after the
 identifier, and a parse direction (which tells from which token list
 we should consume elements).

 It simply applies the rules listed in the web page referred above, by
 adding elements to the description of the type being parsed and
 switching direction whenever needed.

 It does two recursive descent parses, depending on the current
 parse direction (hence the two equations which do something based on
 the first item in the relevant list of tokens).

> varDeclImpl :: (Ctype, [CtokenWithPos], [CtokenWithPos], ParseDirection)
>                -> ParserMonad (Ctype, [CtokenWithPos])

> varDeclImpl (baseType, before, [], Forward) = varDeclImpl (baseType, before, [], Backward)
> varDeclImpl (baseType, before, after, Forward) =
>     case (token $ head after) of
>          CloseParenthesis -> varDeclImpl (baseType, before, tail after, Backward)
>          OpenParenthesis -> funDecl (before, tail after)
>                       where funDecl (before, (token -> CloseParenthesis) : after) = do
>                                 (ctype, after') <- varDeclImpl (baseType,
>                                                                 before,
>                                                                 after,
>                                                                 Forward)
>                                 return (Function [] ctype, after')
>                             funDecl (before, after) = do
>                                 (args, after') <- parseFunArgs after
>                                 (ctype, after'') <- varDeclImpl (baseType,
>                                                                  before,
>                                                                  after',
>                                                                  Forward)
>                                 return (Function args ctype, after'')
>          OpenSqParenthesis -> arrayDecl (before, tail after)
>                       where arrayDecl (before, (token -> CloseSqParenthesis) : after) = do
>                                 (ctype, after') <- varDeclImpl (baseType,
>                                                                 before,
>                                                                 after,
>                                                                 Forward)
>                                 return (VarArray ctype, after')
>                             arrayDecl (before, (token -> Number dim) :
>                                                  (token -> CloseSqParenthesis) : after) = do
>                                 (ctype, after') <- varDeclImpl (baseType,
>                                                                 before,
>                                                                 after,
>                                                                 Forward)
>                                 return (Array dim ctype, after')
>          Semicolon -> varDeclImpl (baseType, before, tail after, Backward)
>          OpenBrace -> varDeclImpl (baseType, before, after, Backward)
>          Comma -> varDeclImpl (baseType, before, after, Backward)
>          _ -> return (baseType, after)

> varDeclImpl (baseType, [], after, Backward) = return (baseType, after)
> varDeclImpl (baseType, before, after, Backward) =
>     case (token $ head before) of
>          OpenParenthesis -> varDeclImpl (baseType, tail before, after, Forward)
>          Star -> do
>            (ctype, after') <- varDeclImpl (baseType, tail before, after, Backward)
>            return (Pointer ctype, after')
>          KeywordInt -> return (PrimitiveInt, after)
>          KeywordVoid -> return (PrimitiveVoid, after)
>          _ -> return (baseType, after)

 The funDecl function uses parseFunArgs (which consumes the tokens
 representing the list of arguments and returns a list of their types)
 which splits the list of tokens at commas that are not
 parenthesized and finishes parsing at the closing parenthesis that
 corresponds to the parenthesis that started the argument list.

> parseFunArgs :: [CtokenWithPos] -> ParserMonad ([Ctype], [CtokenWithPos])
> parseFunArgs tokens = do
>   (argList, after) <- splitArgList tokens 0 [] []
>   argTypes <- mapM varDeclTokens argList
>   return (map extractType argTypes, after)
>     where
>       extractType (VarDecl _ theType, _) = theType

 The splitArgList helper function takes as arguments the list of
 tokens, a parenthesis nesting level and two accumulators, one for the
 list of argument declarations and one for the current argument
 declaration. It returns a tuple made of the list of argument
 declarations and the list of remaining tokens.

>       splitArgList :: [CtokenWithPos] -> Int -> [[CtokenWithPos]] -> [CtokenWithPos]
>                       -> ParserMonad ([[CtokenWithPos]], [CtokenWithPos])
>       splitArgList ((token -> Comma) : tokens) 0 argsSoFar currentArg =
>           splitArgList tokens 0 ((reverse currentArg) : argsSoFar) []
>       splitArgList ((token -> CloseParenthesis) : tokens) 0 argsSoFar currentArg =
>           if (not $ null currentArg)
>           then return (reverse ((reverse currentArg) : argsSoFar), tokens)
>           else return (reverse argsSoFar, tokens)
>       splitArgList a@((token -> OpenParenthesis) : tokens) level argsSoFar currentArg =
>           splitArgList tokens (level + 1) argsSoFar ((head a) : currentArg)
>       splitArgList a@((token -> CloseParenthesis) : tokens) level argsSoFar currentArg =
>           splitArgList tokens (level - 1) argsSoFar ((head a) : currentArg)
>       splitArgList (t:ts) level argsSoFar currentArg =
>           splitArgList ts level argsSoFar (t : currentArg)
>       splitArgList _ _ _ _ = throwError "4"

 Run this function to see the English translation of the 'inputs'
 declarations (or use varDecl on one of the inputs)

> main = do
>   testFile "test/validDeclarations.txt" printInput (\line -> line /= "Typedefs:")
>   testFile "test/brokenDeclarations.txt"
>            (\brokenInputs -> let separateBrokenInputs = (map (\x -> [x]) brokenInputs)
>                                  separateOutputs = map printInput separateBrokenInputs
>                              in intercalate "\n" separateOutputs )
>            (\line -> True)
>     where printInput inputs =
>             let (result, parserState) = (runErrorState (action inputs) makeEmptyParserState)
>             in case result of
>                            Right toPrint -> toPrint ++ (showParserState parserState) ++ "\n"
>                            Left error -> (intercalate "\n" inputs) ++ "\n"
>                                          ++ "failed with error:" ++ (show error) ++ "\n"
>           action inputs = do
>             outputs <- forM inputs actionOneInput
>             return $ intercalate "\n" outputs
>               where actionOneInput inp = do
>                               (varDecls, after') <- varDecl inp
>                               let stringVarDecls = map show varDecls
>                               return $ intercalate "\n" ([inp] ++ stringVarDecls ++ [""])
>           handler errorMsg = do
>             return $ "error with message " ++ errorMsg
>           testFile fileName outputGenerator isInputLine = do
>               inputText <- readFile fileName
>               let inputs = extractInputLines inputText isInputLine
>               let outputText = outputGenerator inputs
>               putStrLn outputText
>               let success = compareTrimEmptyLines inputText outputText
>               when (success) (putStrLn "Success!")
>               when (not success) (putStrLn "Failure!")
>           extractInputLines inputText isInputLine =
>               let inpLines = takeWhile isInputLine (lines inputText)
>                   groups = groupBy (\line1 line2 -> line1 /= "" && line2 /= "") inpLines
>                   filteredGroups = filter (\el -> el /= [""]) groups
>               in map head filteredGroups
>           compareTrimEmptyLines content1 content2 =
>               (trimEmptyLines content1) == (trimEmptyLines content2)
>               where trimEmptyLines content = filter (\line -> line /= "") (lines content)
