--- This module defines functions for parsing URL Query strings.

module URL where

import Maybe
import DetParse

--- @param q - a URL Query String
--- @return the path components
parseQuery :: String -> Maybe [(String, String)]
parseQuery =
  parse $ many $ (?>) '&' *> (paramName *>= yield . (,)) <*> (DetParse.char '=' *> paramName)

  where
    paramName :: Parser String
    paramName = some paramNameChar

    paramNameChar :: Parser Char
    paramNameChar = check (not . specialChar) anyChar

    specialChar :: Char -> Bool
    specialChar c = c == '&' || c == '='

    (?>) :: Char -> Parser ()
    (?>) c = DetParse.char c <|> empty
