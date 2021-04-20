{-|
Module      : TikzStringDiagram
Description : Module for creating and printing string diagrams.
Copyright   : Anthony Wang, 2021
License     : MIT
Maintainer  : anthony.y.wang.math@gmail.com

@TikzStringDiagram@ is a module for defining 'TikzStringDiagram', the structure
containing the TikZ objects needed to create a string diagram.
A 'TikzStringDiagram' is an instance of 'ShowLatex', where
'showLatex' of a 'TikzStringDiagram' is the LaTeX code for rendering
the string diagram.
-}
module TikzStringDiagram where

import Prelude hiding (Functor)
import Data.Array
import Data.List
import Data.Maybe
import TikzObjects
import TwoCatOfCats
import Internal.FormattingData

-- | 'TikzStringDiagram' is a data structure containing the Tikz objects
-- needed to draw a string diagram.
data TikzStringDiagram = TikzStringDiagram
    { tikzsd_array_of_coords :: !(Array (Int, Int) (Maybe TikzPathOperation))
        -- ^ the array of TikZ coordinate path operations used when placing functors
    , tikzsd_array_of_tikz_nt_nodes :: !(Array (Int,Int) (Maybe TikzPathOperation))    
        -- ^ the array of TikZ node path operations, used to place the natural transformations
    , tikzsd_functor_strings :: ![TikzPath]
        -- ^ a list of 'TikzPath's which draw the strings in the string diagram
    , tikzsd_options :: !String
        -- ^ LaTeX code for options in the @tikzpicture@ environment
    }

-- | 'make_tikzsd' creates a 'TikzStringDiagram' from a 'NaturalTransformation',
-- a 'NatFormatting' which can be used to format the 'NaturalTransformation',
-- and a @String@ of LaTeX code for options in the @tikzpicture@ environment.
make_tikzsd :: NaturalTransformation -> NatFormatting -> String -> TikzStringDiagram
make_tikzsd nat nf opts = TikzStringDiagram a b c opts
    where
        a = array_of_tikz_coords nf
        b = array_of_tikz_nt_nodes nat nf
        c = map (fsd_to_tikz_path nf) $ nt_to_functor_strings nat

instance ShowLatex TikzStringDiagram where
    showLatex (TikzStringDiagram cs ns fss opts) = concat [ "\\begin{tikzpicture}["
                                                     , "baseline=(current bounding box.center),"
                                                     , opts
                                                     , "]\n"
                                                     , intercalate "\n" $ map (showLatex.singleton) $ catMaybes $ elems $ cs
                                                     , "\n\n"
                                                     , intercalate "\n" $ map (showLatex.singleton) $ catMaybes $ elems $ ns
                                                     , "\n\n"
                                                     , intercalate "\n" $ map showLatex fss
                                                     , "\n"
                                                     , "\\end{tikzpicture}"]
        where singleton = return :: a -> [a]
