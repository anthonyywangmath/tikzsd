{-# LANGUAGE FlexibleInstances#-}
{-|
Module      : TikzObjects
Description : Defines data structures representing TikZ paths
Copyright   : Anthony Wang, 2021
License     : MIT
Maintainer  : anthony.y.wang.math@gmail.com

@TikzObjects@ defines data structures to represent
    objects such as coordinates and paths.
See the tikzpgf manual for more details.
-}

module TikzObjects where

-- | A class for data structures which can be converted into LaTeX code.
class ShowLatex a where
    showLatex :: a -> String

-- | Data structure representing points on a 2 dimensional TikZ canvas.
data TikzCoordinate = 
    -- | Specifies a point by its coordinates
    Canvas 
    { canvas_x :: !Float
    , canvas_y :: !Float
    } |
    -- | Specifies a coordinate by a name.
    -- A node or coordinate path operation can create a new named coordinate.
    NamedCoordinate 
    { coord_name :: !String
    } deriving (Show)

instance ShowLatex TikzCoordinate where
    showLatex (Canvas x y) = "(" ++ show(x) ++","++show(y)++")"
    showLatex (NamedCoordinate s) = "("++ s++")"

-- | A data structure for specifying path operations, which are chained to create paths.
-- See the tikz manual for details on these operations.
data TikzPathOperation = PathOpMoveTo
    { pop_move_to_coord :: !TikzCoordinate
    } |
    PathOpLineTo
    { pop_line_to_coord :: !TikzCoordinate
    } |
    PathOpVertHorz
    { pop_vert_horz_coord :: !TikzCoordinate
    } | 
    PathOpHorzVert
    { pop_horz_vert_coord :: !TikzCoordinate
    } |
    PathOpCurveToOneControl
    { pop_curve_to_1c_coord :: !TikzCoordinate
    , pop_curve_to_1c_control :: !TikzCoordinate
    } |
    PathOpCurveToTwoControls
    { pop_curve_to_2c_coord :: !TikzCoordinate
    , pop_curve_to_2c_control1 :: !TikzCoordinate
    , pop_curve_to_2c_control2 :: !TikzCoordinate
    } |
    PathOpOption
    { pop_option_string :: !String
    } |
    PathOpScopedOption
    { pop_scoped_option_string :: !String
    , pop_scoped_option_scope :: !TikzPath
    } |
    PathOpCycle |
    PathOpRectangle 
    { pop_rectangle_corner :: !TikzCoordinate
    } |
    PathOpNode 
    { pop_node_options :: !String
    , pop_node_name :: !String --cannot contain punctuation like dot, comma or semicolon
    , pop_node_coordinate :: !TikzCoordinate
    , pop_node_text :: !String
    } |
    PathOpCoordinate
    { pop_coord_options :: !String
    , pop_coord_name :: !String
    , pop_coord_coordinate :: !TikzCoordinate
    } |
    PathOpRelativeNode
    { pop_rel_node_options :: !String
    , pop_rel_node_text :: !String
    } deriving (Show)

instance ShowLatex TikzPathOperation where
    showLatex (PathOpMoveTo c) = " "++(showLatex c)
    showLatex (PathOpLineTo c) = " --"++(showLatex c)
    showLatex (PathOpVertHorz c) = " |-"++(showLatex c)
    showLatex (PathOpHorzVert c) = " -|"++(showLatex c)
    showLatex (PathOpCurveToOneControl c d) = " ..controls"++(showLatex d)++".."++(showLatex c)
    showLatex (PathOpCurveToTwoControls c d1 d2) = " ..controls"++(showLatex d1)++"and"++(showLatex d2)++".."++(showLatex c)
    showLatex (PathOpOption s) = " ["++ s ++ "]"
    showLatex (PathOpScopedOption s p) = " {["++s++"] "++(partialShowLatex p)++"}"
    showLatex (PathOpCycle) = " --cycle"
    showLatex (PathOpRectangle c) = " rectangle"++(showLatex c)
    showLatex (PathOpNode o n c t) = " node["++o++"] ("++n++") at "++(showLatex c)++ " {"++t++"}"
    showLatex (PathOpCoordinate o n c) = " coordinate["++o++"] ("++n++") at "++(showLatex c)
    showLatex (PathOpRelativeNode o t) = " node["++o++"] {"++ t++"}"

-- | If a path operation has a name, it defines a @NamedCoordinate@.
-- @toNamedCoord@ of a path operation returns @Just@ this named coordinate for operations with names, otherwise
-- it returns @Nothing@.
toNamedCoord :: TikzPathOperation -> Maybe TikzCoordinate
toNamedCoord (PathOpNode _o n _c _t) = Just $ NamedCoordinate n
toNamedCoord (PathOpCoordinate _o n _c) = Just $ NamedCoordinate n
toNamedCoord _ = Nothing

-- | A @TikzPath@ is made by sequencing @TikzPathOperation@s.
type TikzPath = [TikzPathOperation]

-- | A helper function used to define @showLatex@ of a @TikzPath@.
partialShowLatex :: TikzPath -> String
partialShowLatex p = foldl (++) "" (map showLatex p)

instance ShowLatex TikzPath where
    showLatex x = "\\path"++(partialShowLatex x)++";"
