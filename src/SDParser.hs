{-|
Module      : SDParser
Description : Defines a parser which parses text into commands for the program
Copyright   : Anthony Wang, 2021
License     : MIT
Maintainer  : anthony.y.wang.math@gmail.com

@SDParser@ defines a parser which parses text into a list of 'SDCommand's,
which are then handled by the main thread of the program.

The notation for the text parsed is documented in the user's manual for the
@tikzsd@ program.
-}
module SDParser (
    SDCommand (..),
    SDDrawLine (..),
    CompElement (..),
    sd_parser
) where

import Data.Char (isSpace)
import Text.ParserCombinators.Parsec ((<|>), 
                                      GenParser, 
                                      try, 
                                      spaces, 
                                      endBy, 
                                      many, 
                                      char, 
                                      string, 
                                      sepBy,
                                      sepEndBy,
                                      oneOf,
                                      satisfy,
                                      between,
                                      noneOf,
                                      alphaNum)

-- | A 'SDCommand' describes a command parsed from the file.
--
-- We have the following commands:
--
-- - 'DefineCat' is the command for defining a category.
-- 
-- - 'DefineFunc' is the command for defining a basic functor.
--
-- - 'DefineNat' is the command for defining a basic natural transformation.
--
-- - 'DrawNat' is the command for creating a file with the TikZ code for the
-- drawing a string diagram of a natural transformation.
data SDCommand = DefineCat
               { dcat_id :: !String
               , dcat_display_string :: !String
               } |
               DefineFunc
               { dfun_id :: !String
               , dfun_display_string :: !String
               , dfun_source :: !String
               , dfun_target :: !String
               , dfun_opts :: !String
               } |
               DefineNat
               { dnat_id :: !String
               , dnat_display_string :: !String
               , dnat_source :: ![CompElement]
               , dnat_target :: ![CompElement]
               , dnat_opts :: !String
               , dnat_shape :: !String
               } |
               DrawNat 
               { dn_file_name :: !String
               , dn_opts :: !String
               , dn_parts :: ![SDDrawLine]
               }
    deriving Show
data StructureType = Cat | Func | Nat
    deriving Show

-- | 'SDDrawLine' describes a line in a 'DrawNat' command.
-- These lines are used to specify a 'NaturalTransformation' and a 'NatFormatting'
-- used format the 'NaturalTransformation'.
-- See the user's manual for notation.
--
-- - 'SDDrawFun' specifies a line used to specify a 'Functor'
-- 
-- - 'SDDrawNat' specifies a line used to specify a horizontal composition of basic natural
-- transformations.
data SDDrawLine = SDDrawFun [CompElement] | SDDrawNat [CompElement]
    deriving Show
-- | @(CompElement i opts)@ refers to the element with id @i@, modified with options @opts@
data CompElement = Empty |
    CompElement 
    { ce_id :: !String
    , ce_opts :: !String
    } deriving (Show, Eq)

def_or_draw :: GenParser Char st String
def_or_draw = try (string "define") <|> try (string "draw")

def_types :: GenParser Char st String
def_types = try (string "category") <|> try (string "functor") <|> try nat_trans_words

nat_trans_words :: GenParser Char st String
nat_trans_words = (string "natural" >> spaces >> string "transformation")

disp_string_parser :: GenParser Char st String
disp_string_parser = between (char '\"') (char '\"') (many $ noneOf "\"")

options_parser :: GenParser Char st String
options_parser = between (char '[') (char ']') (many $ noneOf "[]")

id_parser:: GenParser Char st String
id_parser = many $ satisfy (\x -> (not $ isSpace x) && x `notElem` "&\\[]")

shape_parser :: GenParser Char st String
shape_parser = many alphaNum

category_parser :: GenParser Char st SDCommand
category_parser = do cid <- spaces >> id_parser
                     ds <- try (spaces >> disp_string_parser) <|> return ""
                     return $ DefineCat cid ds

functor_parser :: GenParser Char st SDCommand
functor_parser = do f_id <- spaces >> id_parser
                    ds <- try (spaces >> disp_string_parser) <|> return ""
                    source_id <- spaces >> string "source" >> spaces >> char ':' >> spaces >> id_parser
                    target_id <- spaces >> string "target" >> spaces >> char ':' >> spaces >> id_parser
                    opts <- try (spaces >> options_parser) <|> return ""
                    return $ DefineFunc f_id ds source_id target_id opts

nat_trans_parser :: GenParser Char st SDCommand
nat_trans_parser = do nt_id <- spaces >> id_parser
                      ds <- try (spaces >> disp_string_parser) <|> return ""
                      source_ids <- spaces >> string "source" >> spaces >> char ':' 
                                        >> sepBy cell_parser (try $ spaces >> char '&')
                      spaces >> string "\\\\"
                      target_ids <- spaces >> string "target" >> spaces >> char ':' 
                                        >> sepBy cell_parser (try $ spaces >> char '&')
                      spaces >> string "\\\\"
                      opts <- try (spaces >> options_parser) <|> return ""
                      shape <- try (spaces >> string "shape" >> spaces >> char ':' >> spaces >> shape_parser) <|> return ""
                      return $ DefineNat nt_id ds source_ids target_ids opts shape

def_parser :: GenParser Char st SDCommand
def_parser = spaces >> def_types >>= def_controller
    where
        def_controller "category"       = category_parser
        def_controller "functor"        = functor_parser
        def_controller "transformation" = nat_trans_parser

entry_parser :: GenParser Char st SDCommand
entry_parser = def_or_draw >>= entry_controller
    where
        entry_controller "define" = def_parser
        entry_controller "draw"   = draw_parser

-- | 'sd_parser' is the parser which parses a @String@
-- into a list of @SDCommand@s.
sd_parser :: GenParser Char st [SDCommand]
sd_parser = spaces >> sepEndBy entry_parser spaces

cell_parser :: GenParser Char st CompElement
cell_parser = do idm <- spaces >> id_parser
                 opt <- try (spaces >> options_parser) <|> return ""
                 case idm of "" -> return Empty
                             cid -> return $ CompElement cid opt

f_or_n :: GenParser Char st StructureType
f_or_n = do fn <- oneOf "fn" <* (spaces >> char ':')
            case fn of 'f' -> return Func
                       'n' -> return Nat

draw_line_parser :: GenParser Char st SDDrawLine
draw_line_parser = do fn <- spaces >> f_or_n
                      parts <- sepBy cell_parser (try $ spaces >> char '&')
                      case fn of Func -> return $ SDDrawFun parts
                                 Nat  -> return $ SDDrawNat parts
 
draw_parser :: GenParser Char st SDCommand
draw_parser = do file_path <- spaces >> (many $ satisfy (not.isSpace))
                 opts <- try (spaces >> options_parser) <|> return ""
                 list <- endBy (try draw_line_parser) (try $ spaces >> string "\\\\")
                 return $ DrawNat file_path opts list
