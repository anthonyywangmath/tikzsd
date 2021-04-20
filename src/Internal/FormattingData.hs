{-|
Module      : Internal.FormattingData
Description : Defines data structures and functions relating to the placement
              of functors on a TikZ canvas
Copyright   : Anthony Wang, 2021
License     : MIT
Maintainer  : anthony.y.wang.math@gmail.com

@Internal.FormattingData@ defines data structures relating to the placement
    of functors on a TikZ canvas when drawing a string diagram.
-}
module Internal.FormattingData where

import Prelude hiding (Functor)
import Data.Array
import Data.List
import Data.Maybe (catMaybes)
import Internal.TwoCatOfCats
import TikzObjects

-- | 'FunctorFormatting' is a data structure that specifies the spacing
-- for a composite of basic functors on a horizontal line.
--
-- The @ff_length@ is the total length of the line.
-- The @ff_positions_list@ is the collection of indices where a functor is placed.
-- The @ff_positions_list@ is indexed starting at 0.
--
-- For example, if we use @(FunctorFormatting 5 [1,2,4])@ to 
--  format a composition F G H (composing from left to right as is our convention),
--  we would get the spacing
--
--      & F & G & & H,
--
--  while using @(FunctorFormatting 4 [0,1,3])@ to format the same list would give the spacing
--
--      F & G & & H.
--
-- The notation for the spacing above is similar to how spacing is defined in LaTeX, i.e. in
-- tables, arrays, etc.
--
-- Assumptions on the data: @ff_positions_list@ is an increasing list of nonnegative integers.
-- The largest element in @ff_positions_list@, if it exists, is at most @ff_length - 1@.
data FunctorFormatting = FunctorFormatting
    { ff_length   :: !Int
    , ff_positions_list :: ![Int]
    } deriving (Eq)
    
-- | 'ff_num_positions' of a @FunctorFormatting@ is equal to the @func_reduced_length@
-- of a functor which it can format, i.e. it is the length of @func_positions_list@.
ff_num_positions :: FunctorFormatting -> Int
ff_num_positions = length . ff_positions_list

-- | Data of type 'FunctorFormatting' naturally form an operad, with each @FunctorFormatting@
-- giving an n-ary operation where n is equal to the number of positions given by
-- @ff_num_positions@.
--
-- If @ff@ is a @FunctorFormatting@ with n=@(ff_num_positions ff)@, and
-- @[ff1, ..., ffn]@ is a list of n @FunctorFormatting@s, then
-- we can get a composite @FunctorFormatting@ by putting @ff1@ at the first position
-- in @ff@,..., and @ffn@ at the nth position in @ff@.
--
-- For example:
--
-- If @ff@ is the formatting @"*&*&*&*"@
-- and @ff1@,...,@ff4@ are all @empty@, then @ff_operad_compose ff [ff1,ff2,ff3,ff4]@
-- is @empty@.
--
-- If @ff@ is the formatting represented by @"*& &*& "@, @ff1@ is the formatting @"*& &*"@ and
-- @ff2@ is the formatting @"*&*&*& "@, then
-- @ff_operad_compose ff [ff1,ff2]@ is @"*& &*& &*&*&*& & "@.
--
-- @(ff_operad_compose ff ffs)@ is equal to @Nothing@ if the length of @ffs@ is not equal to 
-- @(ff_num_positions ff)@.
-- Otherwise, it returns @Just@ the above composition.
ff_operad_compose :: FunctorFormatting -> [FunctorFormatting] -> Maybe FunctorFormatting
ff_operad_compose ff ffs
    | ff_num_positions ff /= length ffs = Nothing
    | otherwise                         = let new_length = (ff_length ff) + (sum $ map ff_length ffs) - (ff_num_positions ff)
                                              positions_builder :: [Int] -> Int -> [Int] -> [FunctorFormatting] -> [Int]
                                              positions_builder current _ [] [] = current
                                              positions_builder current offset (n:ns) (nf:nfs) = 
                                                  positions_builder (current ++ (map (\x -> x+n+offset) (ff_positions_list nf)))
                                                      (offset + (ff_length nf) - 1) ns nfs
                                              positions_builder _ _ _ _ = error $ "positions_builder a b c d should only ever be called"
                                                                            ++ " when the lengths of c and d are the same."
                                              new_positions_list = positions_builder [] 0 (ff_positions_list ff) ffs
                                          in Just (FunctorFormatting new_length new_positions_list)

-- | If @f@ is a @Functor@ with @func_reduced_length f@ equal to n,
-- then @default_ff f@ is the formatting @"*&*&...&*"@  where there are n total positions.
default_ff :: Functor -> FunctorFormatting
default_ff func = let n = func_reduced_length func
                  in FunctorFormatting n [0..(n-1)]

-- | A show function which is easier to understand than the default one.
--
-- If the @FunctorFormatting@ does not satisfy the assumptions above,
-- then it shows @"InvalidFunctorFormatting "++(show ff_length)++" "++(show ff_positions_list)@.
--
-- If the @FunctorFormatting@ has @ff_length@ equal to @0@, then it shows @"empty"@.
-- Otherwise it is a @'&'@ separated list with @ff_length@ number of positions,
--     with a @'*'@ in the positions in @ff_positions_list@ and a @' '@
--     in the other positions.
--
-- For example @show (FunctorFormatting 5 [1,2,4])@ is equal to @" &*&*& &*"@.
instance Show FunctorFormatting where
    show ff 
        | len==0 && positions_check = "empty"
        | len>0 && positions_check  = intercalate "&" $ show_helper (ff_length ff) 0 (ff_positions_list ff)
        | otherwise                 = "InvalidFunctorFormatting "++(show len)++" "++(show pos)
        where 
            len = ff_length ff
            pos = ff_positions_list ff
            in_range a b c = a<b && b<c
            positions_check = foldl (&&) True $ zipWith3 in_range (-1:pos) pos (repeat len)
            show_helper :: Int -> Int  -> [Int] -> [String]
            show_helper l posit [] = replicate (l-posit) " "
            show_helper l posit (r:rs)
                | posit == l  = []
                | posit == r  = "*":(show_helper l (posit+1) rs)
                | otherwise = " ":(show_helper l (posit+1) (r:rs))

-- | A data structure to specify the location of functors when creating a string diagram
-- from a natural transformation.
--
-- We will not attempt to format every possible @NaturalTransformation@, 
--     but will only format a certain subcollection.
-- Roughly speaking, we can format natural transformations that can be thought of as vertical
--     compositions of horizontal compositions of basic natural transformations.
--
-- We will only format the following types of @NaturalTransformation@
--
-- 1. @(NaturalTransformation n d s b o)@ can be formatted with a length 2 list @[ff1, ff2]@
--        of @FunctorFormatting@ where
--     @(ff_num_positions ff1)@ is equal to @func_reduced_length@ of the source functor,
--     @(ff_num_positions ff2)@ is equal to @func_reduced_length@ of the target functor,
--     and at least one of @(ff_num_positions ff1)@ or @(ff_num_positions ff2)@ is nonzero.
--
-- 2. Identity natural transformations of an identity functor of a category @c@,
--      i.e. @(NatTransVerticalComposite (OneGlobelet (CompositeFunctor (ZeroGlobelet c c) []) 
--      (CompositeFunctor (ZeroGlobelet c c) [])) [])@
--      can be formatted with a length 2 list @[ff1,ff2]@ of FunctorFormatting with
--      @(ff_num_positions ff1)@ and @(ff_num_positions ff2)@ both equal to 0
--
-- 3. Identity natural transformations of a functor
--      i.e. @(NatTransVerticalComposite (OneGlobelet (Functor i d b o) (Functor i d b o)) [])@
--      can be formatted with a length 2 list @[ff1,ff2]@ of FunctorFormatting with
--      @(ff_num_positions ff1)@ and @(ff_num_positions ff2)@ both equal to 1
--
-- 4. Horizontal composites of natural transformations of type 1,2,3 above
--      i.e. @(NatTransHorizontalComposite g list)@ where list is a list of natural transformations
--      of types 1,2,3 above, can be formatted with a length 2 list @[ff1, ff2]@
--      of FunctorFormatting, where
--      @ff_num_positions ff1@ is equal to @func_reduced_length@ of the source functor,
--      and @ff_num_positions ff2@ is equal to @func_reduced_length@ of the target functor.
--
-- 5. Vertical composites of natural transformations of type 1,2,3,4,
--      i.e. @(NatTransVerticalComposite g list)@ where list is a nonempty list of natural transformations of
--      types 1,2,3,4 above, can be formatted with a length @l+1@ list @[ff_0, ... , ff_l]@ of 
--      @FunctorFormatting@, where @l@ is the length of @list@,
--      where @ff_num_positions@ of @ff_i@ is equal to 
--      @func_reduced_length@ of the source of @list!!i@ or the @func_reduced_length@
--      of the target of @list!!(i-1)@, whichever is defined
--      (these two are equal when both are defined by our assumptions on @NatTransVerticalComposite@).
type NatFormatting = [FunctorFormatting] 

-- | 'nf_max_horz_position' of a 'NatFormatting' gives the largest @ff_num_positions@
--  among the 'FunctorFormatting' in the list.
nf_max_horz_position :: NatFormatting -> Int
nf_max_horz_position ffs = maximum $ map ff_num_positions ffs

-- | 'nf_pos_to_coord' takes a 'NatFormatting' @nt@ and a pair @(x,y)@ representing
-- the position of a basic natural transformation in a natural transformation
-- which can be formatted by @nt@, and returns @Just@ the associated coordinate on a TikZ canvas,
--  if possible.
-- It returns @Nothing@ if @(x,y)@ does not specify a valid position in a natural transformation
--  which can be formatted by @nt@.
--
-- Here, @x@ represents the row and @y@ represents the position in the row, with indexing starting
-- at @0@.
-- 'nf_pos_to_coord' will return @Nothing@ unless the following are satisfied:
--
-- -@x@ is between @0@ and @(length nf)-1@, inclusive
-- -@y@ is between @0@ and @(ff_num_positions (nf!!x))-1@, inclusive.
nf_pos_to_coord :: NatFormatting -> (Int, Int) -> Maybe (Float, Float)
nf_pos_to_coord nf (x,y)
    | x < 0                           = Nothing
    | x >= length nf                  = Nothing
    | y < 0                           = Nothing
    | y >= ff_num_positions (nf !! x) = Nothing
    | otherwise                       = Just (fromIntegral ((ff_positions_list (nf!!x))!!y) , fromIntegral (-2*x))

-- | 'nf_pos_to_tikz_coord' returns @Just@ the TikZ coordinate path
-- operation at 'nf_pos_to_coord', with the coordinate named by
-- 'pos_to_internal_name'.
-- It returns @Nothing@ if 'nf_pos_to_coord' returns @Nothing@.
nf_pos_to_tikz_coord :: NatFormatting -> (Int,Int) -> Maybe TikzPathOperation
nf_pos_to_tikz_coord nf (x,y) = do (a,b) <- nf_pos_to_coord nf (x,y)
                                   return (PathOpCoordinate "" (pos_to_internal_name (x,y)) (Canvas a b))

-- | 'pos_to_internal_name' converts a position @(x,y)@ described by the documentation
-- for 'nf_pos_to_coord' and converts it into a @String@ for referencing purposes inside a TikZ
-- picture.
pos_to_internal_name :: (Int, Int) -> String
pos_to_internal_name (x,y) = "tikzsd_internal_pos_"++(show x) ++ "_"++(show y)

-- | 'pos_to_named_coord' gives the named @TikzCoordinate@ with name given by
-- 'pos_to_internal_name'.
-- This named coordinate can be used to refer to the @TikzPathOp@
--  coordinate given by 'nf_pos_to_coord'.
pos_to_named_coord :: (Int,Int) -> TikzCoordinate
pos_to_named_coord (x,y) = NamedCoordinate $ pos_to_internal_name (x,y)

-- | 'array_of_tikz_coords' takes a 'NatFormatting' @nf@ and returns the array
-- which maps @(x,y)@ to @nf_pos_to_tikz_coord nf (x,y)@.
-- The bounds of the array are from @(0,0)@ to @((length nf)-1,(nf_max_horz_position nf)-1)@.
array_of_tikz_coords :: NatFormatting -> Array (Int, Int) (Maybe TikzPathOperation)
array_of_tikz_coords ffs = array r [(i, nf_pos_to_tikz_coord ffs i) | i <- inds]
    where
        r = ((0,0), ((length ffs)-1, (nf_max_horz_position ffs)-1))
        inds = range r

-- | @(get_nt_in_pos nt (x,y))@ will return @Just@ the @y@th basic natural transformation 
--  in the @x@th row.
-- It returns @Nothing@ if @x@ does not specify a row of the natural transformation or  @y<0@ or @y>=@ the number of
--  basic natural transformations in the @x@th row.
-- We assume @nt@ is assumed to be one of the 5 types of natural transformations which we can format,
--  described in the documentation for 'NatFormatting'.
-- It makes sense to think of such @nt@ as vertical composites of horizontal composites of natural
-- transformations.
-- @x@ specifies the index in the vertical composite, while @y@ specifies the index in the
-- horizontal composite, with indexing starting at @0@.
get_nt_in_pos :: NaturalTransformation -> (Int, Int) -> Maybe NaturalTransformation
get_nt_in_pos (NaturalTransformation n d s b o)  (x,y)
    | x/=0 || y/=0  = Nothing
    | otherwise     = Just (NaturalTransformation n d s b o)
get_nt_in_pos (NatTransVerticalComposite (OneGlobelet (CompositeFunctor _ []) (CompositeFunctor _ [])) []) _ = Nothing
get_nt_in_pos (NatTransVerticalComposite (OneGlobelet (Functor _ _ _ _) _ ) []) _ = Nothing
get_nt_in_pos (NatTransHorizontalComposite _bg nts) (x,y)
    | x /=0 || y < 0 || y>= length simple_nts = Nothing
    | otherwise                               = Just (simple_nts!!y)
    where
        simple_nts = filter is_basic_nt nts
get_nt_in_pos (NatTransVerticalComposite _bg nts) (x,y)
    | x<0 || x >= length nts = Nothing
    | otherwise              = get_nt_in_pos (nts!!x) (0,y)

-- | @(get_nt_pos_to_coord nt nf (x,y))@ will calculate the TikZ coordinates the basic natural
--  tranformation @(get_nt_in_pos nt (x,y))@ should be placed at.
-- It returns
--      @Just@ the coordinates if @(get_nt_in_pos nt (x,y)@ is not @Nothing@,
--      and @Nothing@ otherwise.
-- @nt@ is assumed to be one of the natural transformations we can format,
--  and @nf@ is assumed to be a 'NatFormatting' which can be used to format @nt@.
-- The TikZ coordinates of the basic natural transformation @(get_nt_in_pos nt (x,y))@
-- is based on the TikZ coordinates of the basic functors in its source and target.
nt_nf_pos_to_coord :: NaturalTransformation -> NatFormatting -> (Int, Int) -> Maybe (Float, Float)
nt_nf_pos_to_coord (NaturalTransformation _n _d _s b _o) nf (x,y)
    | x /= 0 || y /= 0 = Nothing
    | target_len == 0  = do source_first <- nf_pos_to_coord nf (0,0)
                            source_last <- nf_pos_to_coord nf (0, source_len-1)
                            return (0.5*((fst source_first)+(fst source_last)),-1) 
    | source_len == 0  = do target_first <- nf_pos_to_coord nf (1,0)
                            target_last <- nf_pos_to_coord nf (1, target_len-1)
                            return (0.5*((fst target_first)+(fst target_last)),-1)
    | otherwise        = do source_first <- nf_pos_to_coord nf (0,0)
                            source_last <- nf_pos_to_coord nf (0,source_len-1)
                            target_first <- nf_pos_to_coord nf (1,0)
                            target_last <- nf_pos_to_coord nf (1, target_len-1)
                            return (0.25*((fst source_first)+(fst source_last)+(fst target_first)+(fst target_last)),-1)
    where 
        source_len = func_reduced_length $ glob1_source b
        target_len = func_reduced_length $ glob1_target b
nt_nf_pos_to_coord (NatTransVerticalComposite (OneGlobelet (CompositeFunctor _ []) (CompositeFunctor _ [])) []) _nf (_x,_y) = Nothing
nt_nf_pos_to_coord (NatTransVerticalComposite (OneGlobelet (Functor _ _ _ _) _ ) []) _nf (_x,_y) = Nothing
nt_nf_pos_to_coord (NatTransHorizontalComposite _bg nts) nf (x,y)
    | x /= 0 || y < 0 || z > length nts = Nothing
    | target_len == 0                   = do source_first <- nf_pos_to_coord nf (0,source_offset)
                                             source_last <- nf_pos_to_coord nf (0, source_offset+source_len-1)
                                             return (0.5*((fst source_first)+(fst source_last)),-1)
    | source_len == 0                   = do target_first <- nf_pos_to_coord nf (1,target_offset)
                                             target_last <- nf_pos_to_coord nf (1,target_offset+target_len-1)
                                             return (0.5*((fst target_first)+(fst target_last)),-1)
    | otherwise                         = do source_first <- nf_pos_to_coord nf (0,source_offset)
                                             source_last <- nf_pos_to_coord nf (0,source_offset+source_len-1)
                                             target_first <- nf_pos_to_coord nf (1,target_offset)
                                             target_last <- nf_pos_to_coord nf (1,target_offset+target_len-1)
                                             return (0.25*((fst source_first)+(fst source_last)+(fst target_first)+(fst target_last)),-1)
    where 
        num_taken :: [NaturalTransformation] -> Int -> Int
        --num_taken ns m gives the position in ns of the mth basic natural transformation, i.e. of the form
        --  (NaturalTransformation n d s b o). 
        --  Here 1 is the first position and (length ns)+1 means there are less than m natural
        --  transformations of the form (NaturalTransformation n d s b o) in ns
        -- We assume that m>=1
        num_taken [] _ = 1
        num_taken ((NaturalTransformation _ _ _ _ _):_) 1 = 1
        num_taken ((NaturalTransformation _ _ _ _ _):ns) m = 1+(num_taken ns (m-1))
        num_taken (_:ns) m = 1+(num_taken ns m)
        z = num_taken nts (y+1)
        beg_nts = take (z-1) nts --because of how we guarded, this should not be evaluated if z>length nts
        current_nt = nts !! (z-1)
        source_len = nat_source_length current_nt
        target_len = nat_target_length current_nt
        source_offset = sum $ map nat_source_length beg_nts
        target_offset = sum $ map nat_target_length beg_nts
nt_nf_pos_to_coord (NatTransVerticalComposite _bg nts) nf (x,y) 
    | x<0 || x>= length nts = Nothing
    | otherwise             = do (a,_) <- nt_nf_pos_to_coord (nts!!x) (drop x nf) (0,y)
                                 return (a,fromIntegral (-2*x-1))

-- | 'nt_nf_pos_to_nt_node' gives @Just@ the TikZ node path operation with a node created at
-- 'nt_nf_pos_to_coord' with name given by 'nt_pos_to_internal_name'.
-- It returns @Nothing@ if 'nt_nf_pos_to_coord' returns @Nothing@.
nt_nf_pos_to_nt_node :: NaturalTransformation -> NatFormatting -> (Int,Int) -> Maybe TikzPathOperation
nt_nf_pos_to_nt_node nt nf (x,y) = do basic_nt <- get_nt_in_pos nt (x,y)
                                      (a,b) <- nt_nf_pos_to_coord nt nf (x,y)
                                      let opts = nt_options basic_nt
                                      let shape = nt_shapeString basic_nt
                                      let disp = nt_displayString basic_nt
                                      let name = nt_pos_to_internal_name (x,y)
                                      return (PathOpNode (shape++",draw,"++opts) name (Canvas a b) disp)

-- | 'nt_pos_to_internal_name' converts a position @(x,y)@ as described by 'get_nt_in_pos'
-- and creates a @String@ for referencing purposes inside a TikZ picture.
nt_pos_to_internal_name :: (Int, Int) -> String
nt_pos_to_internal_name (x,y) = "tikzsd_internal_nt_node_"++(show x)++"_"++(show y)

-- | 'nt_pos_to_named_coord' gives the named @TikzCoordinate@ with name given by 
-- 'nf_pos_to_internal_name'.
-- This named coordinate can be used to refer to the @TikzPathOp@ node given by 
-- 'nt_nf_pos_to_nt_node'.
nt_pos_to_named_coord :: (Int, Int) -> TikzCoordinate
nt_pos_to_named_coord (x,y)= NamedCoordinate $ nt_pos_to_internal_name (x,y)

-- | 'nt_max_pos_dimensions' takes one of the 5 types of @NaturalTransformation@
-- we can format (described by the documentation for 'NatFormatting') and returns
-- @(x,y)@.
-- If our natural transformation is viewed as a vertical composite of horizontal composites
-- of basic natural transformations, @x@ will equal the number of horizontal composites which
-- are being composed vertically, and @y@ will be the maximum number of basic natural
-- transformations in a horizontal composite.
nt_max_pos_dimensions :: NaturalTransformation -> (Int, Int)
nt_max_pos_dimensions (NaturalTransformation _ _ _ _ _) = (1,1)
nt_max_pos_dimensions (NatTransVerticalComposite (OneGlobelet (CompositeFunctor _ []) (CompositeFunctor _ [])) []) = (0,0)
nt_max_pos_dimensions (NatTransVerticalComposite (OneGlobelet (Functor _ _ _ _) _ ) []) = (0,0)
nt_max_pos_dimensions (NatTransHorizontalComposite _bg nts) = (0, length $ filter is_basic_nt nts)
nt_max_pos_dimensions (NatTransVerticalComposite _bg nts) = (length nts, maximum $ map (snd.nt_max_pos_dimensions) nts)

-- | @(array_of_tikz_nt_nodes nt nf)@ is the array mapping pairs @(x,y)@
-- to the TikZ node path operation @(nt_nf_pos_to_nt_node nt nf (x,y))@.
-- If @(nt_max_pos_dimensions nt)@ is equal to @(a,b)@, then the bounds of the array
-- are @(0,0)@ to @(a-1,b-1)@.
array_of_tikz_nt_nodes :: NaturalTransformation -> NatFormatting -> Array (Int, Int) (Maybe TikzPathOperation)
array_of_tikz_nt_nodes nt nf = array r [(i,nt_nf_pos_to_nt_node nt nf i) | i<- inds]
    where
        (x,y) = nt_max_pos_dimensions nt
        r = ((0,0),(x-1,y-1))
        inds = range r

-- | A 'FunctorStringElement' is either
-- a @(FunctorElement (x,y))@, where @(x,y)@ can be converted to coordinates on a TikZ picture
-- using 'nf_pos_to_coord' (after specifying a 'NatFormatting')
-- or a @(NatElement (x,y))@, where @(x,y)@ can be converted to coordinates in a TikZ picture
-- using 'nt_nf_pos_to_coord' (after specifying a 'NaturalTransformation' and 'NatFormatting'
-- which can be used to format the 'NaturalTransformation').
data FunctorStringElement = FunctorElement (Int, Int) | NatElement (Int, Int) deriving (Show)

-- | A 'FunctorStringData' represents a string in a string diagram.
-- It consists of a list @fsd_list_of_elements@, which is a list of 'FunctorStringElement'
--  which the string goes between, @fsd_display_string@ which is LaTeX code for labeling the string,
--  and @fsd_options@ which is LaTeX code for options when labeling the string.
-- If the first element of the @fsd_list_of_elements@ is of the form @(FunctorElement (x,y))@,
-- the string in the string diagram starts from the top of the diagram, at the position specified by
-- the @FunctorElement@.
-- If the first element of the list is of the form @(NatElement (x,y))@,
-- the string starts at a basic natural transformation at the position
-- specified by the @NatElement@.
-- A similar statement holds for lists ending with @FunctorElement@ or @NatElement@.
-- All other positions on the list (i.e. not the first and not the last)
-- are occupied by a @(FunctorElement (x,y))@ representing positions where the string will pass
-- through.
--
-- The following assumptions on @fsd_list_of_elements@ are assumed to hold:
--
-- 1. All but possibly the first and the last element on the list are of the form
--  @(FunctorElement (x,y))@.
-- 2. The list contains at least one element of the form @(FunctorElement (x,y))@.
-- 3. If @(FunctorElement (x,y))@ and @(FunctorElement (z,w))@ are consecutive elements of the list,
--  then @z@ is equal to @x+1@.
-- 4. If @(NatElement (x,y))@ is the first element of the list and is followed by 
--  @(FunctorElement (z,w))@ then @z@ is equal to @x+1@.
-- 5. If @(NatElement (x,y))@ is the last element of the list and is preceded by 
--  @(FunctorElement (z,w))@ then @x@ is equal to @z@.
data FunctorStringData = FunctorStringData
    { fsd_list_of_elements :: ![FunctorStringElement]
    , fsd_display_string   :: !String
    , fsd_options          :: !String
    } deriving (Show)

-- | 'fse_get_named_coord' gives the named coordinate associated to
-- a 'FunctorStringelement'.
-- It either calls 'pos_to_named_coord' in the case of 'FunctorElement',
-- or it calls 'nt_pos_to_named_coord' in the case of 'NatElement'.
fse_get_named_coord :: FunctorStringElement -> TikzCoordinate
fse_get_named_coord (FunctorElement x) = pos_to_named_coord x
fse_get_named_coord (NatElement x) = nt_pos_to_named_coord x

-- | 'fse_is_nat_elem' returns @True@ if the 'FunctorStringElement'
-- is of the form @(NatElement (x,y))@ and @False@ otherwise.
fse_is_nat_elem :: FunctorStringElement -> Bool
fse_is_nat_elem (NatElement _) = True
fse_is_nat_elem _ = False

-- | 'fsd_head_position' is @Just (x,y)@ when the first element in
-- the @fsd_list_of_elements@ of the given
-- 'FunctorStringData' is @(FunctorElement (x,y))@, otherwise it is 
-- @Nothing@.
fsd_head_position :: FunctorStringData -> Maybe (Int, Int)
fsd_head_position fsd = let f = head $ fsd_list_of_elements fsd
    in case f of (NatElement _) -> Nothing
                 (FunctorElement (x,y)) -> Just (x,y)

-- | 'fsd_tail_position' is @Just (x,y)@ when the last element in
-- the @fsd_list_of_elements@ of the given
-- 'FunctorStringData' is @(FunctorElement (x,y))@, otherwise it is 
-- @Nothing@.
fsd_tail_position :: FunctorStringData -> Maybe (Int, Int)
fsd_tail_position fsd = let l = last $ fsd_list_of_elements fsd
    in case l of (NatElement _) -> Nothing
                 (FunctorElement (x,y)) -> Just (x,y)

-- | 'fsd_combinable' is a Boolean value which tells whether two 
-- 'FunctorStringData'
-- can be combined together to form a longer 'FunctorStringData'.
-- 
-- Explicitly, it returns @True@ if the last element in
-- @fsd_list_of_elements@ of the first 'FunctorStringData' is of the form
-- @(FunctorElement (x,y))@ and is equal to the first
-- elementin @fsd_list_of_elements@ of the second 'FunctorStringData'.
fsd_combinable :: FunctorStringData -> FunctorStringData -> Bool
fsd_combinable fsd1 fsd2 = (middle == fsd_tail_position fsd1) && (middle /= Nothing)
    where middle = fsd_head_position fsd2

-- | If @(fsd_combinable fsd1 fsd2)@ is @True@, @(fsd_combine fsd1 fsd2)@
--  is the 'FunctorStringData' gotten by combining the @fsd_list_of_elements@ of @fsd1@ and @fsd2@,
--  which is done by identifying the common last element of of the first list with the first
--  element of the second.
-- The display string and options are taken from @fsd1@.
fsd_combine :: FunctorStringData -> FunctorStringData -> FunctorStringData
fsd_combine (FunctorStringData l1 ds1 op1) (FunctorStringData l2 _ds2 _op2) = FunctorStringData l3 ds1 op1
    where 
        l3 = l1 ++ (tail l2)

-- | @(fsd_append fsd fse)@ appends the 'FunctorStringElement' @fse@
-- to the end of the @fsd_list_of_elements@ of @fsd@.
--
-- No check is done that the resulting 'fsd_list_of_elements' satisfies the
-- assumptions given in the documentation for 'FunctorStringData'.
fsd_append :: FunctorStringData -> FunctorStringElement -> FunctorStringData
fsd_append (FunctorStringData loe ds op) fe = FunctorStringData (loe ++ [fe]) ds op

-- | @(fsd_prepend fse fsd)@ adds the 'FunctorStringElement' @fse@
-- to the start of the @fsd_list_of_elements@ of @fsd@.
--
-- No check is done that the resulting 'fsd_list_of_elements' satisfies the
-- assumptions given in the documentation for 'FunctorStringData'.
fsd_prepend :: FunctorStringElement -> FunctorStringData -> FunctorStringData
fsd_prepend fe (FunctorStringData loe ds op) = FunctorStringData (fe:loe) ds op

-- | 'basic_func_to_fsd' takes a basic functor along with a position @(x,y)@
-- and gives the associated 'FunctorStringData' whose 
-- @fsd_list_of_elements@ is the singleton list containing @(FunctorElement (x,y))@.
basic_func_to_fsd :: Functor -> (Int, Int) -> FunctorStringData
basic_func_to_fsd (Functor _id ds _bg op) (x,y) = FunctorStringData fel ds op
    where 
        fel = [FunctorElement (x,y)]
basic_func_to_fsd _ _ = error "basic_func_to_fsd is only defined for basic functors."

-- | An 'OrderedFSDList' is a list of 'FunctorStringData'
-- assumed to satisfy the following axioms on the order of its elements:
--
-- 1. @filter (\x-> x/=Nothing) (map fsd_head_position fsds)@ is equal to @[Just (r,0), ..., Just (r,n)]@
--          for some @r@ and @n@
--
-- 2. @filter (\x-> x/=Nothing) (map fsd_tail_position fsds)@ is equal to @[Just (s,0), ..., Just (s,m)]@
--          for some @s@ and @m@
--
-- 3. @r<=s@ when both @r@ and @s@ are uniquely determined (i.e. the above lists are nonempty).
type OrderedFSDList = [FunctorStringData]


-- | 'func_to_fsds' takes a 'Functor',
-- an @Int@ representing a row and an @Int@ representing an offset
-- and returns an 'OrderedFSDList'.
-- 
-- Decomposing the @Functor@ into a composition of basic functors, each
-- basic functor corresponds to one element of the 'OrderedFSDList'.
-- The 'FunctorStringData' in this list are created with
-- 'basic_func_to_fsd', with the basic functors placed at
-- @(r,o),(r,o+1),...,(r,o+n-1)@ where @r@ is the given row,
-- @o@ is the given offset, and @n@ is equal to 'func_reduced_length'
-- of the given functor.
func_to_fsds :: Functor -> Int -> Int -> OrderedFSDList
func_to_fsds fun row offset= zipWith basic_func_to_fsd functors coords
    where 
        functors = func_to_single_list fun
        n = length functors
        coords = map (\y -> (row,y+offset)) [0..(n-1)]

-- | If @fsds1@ and @fsds2@ are two 'OrderedFSDList' for which 
-- there exists a pair @(x,y)@ of nonnegative integers such that
--
--      - @filter (\x-> x/=Nothing) (map fsd_tail_position fsds1)@ 
--        is equal to @filter (\x-> x/=Nothing) (map fsd_head_position fsds2)@ which
--        is equal to @[Just (x,0), ..., Just (x,y)]@,
--
-- then @(fsds_amalg fsds1 fsds2)@ is an 'OrderedFSDList' which contains
--
--  - the @fsd@ in @fsds1@ for which @(fsd_tail_position fsd)@ is equal to @Nothing@
--  
--  - the @fsd@ in @fsds2@ for which @(fsd_head_position fsd)@ is equal to @Nothing@
--
--  - @(fsd_combine fsd1 fsd2)@ where @fsd1@ is in @fsds1@ and @fsd2@ is in @fsds2@
--    and @(fsd_combinable fsd1 fsd2)@ is @True@.
fsds_amalg :: OrderedFSDList -> OrderedFSDList -> OrderedFSDList
fsds_amalg fsds [] = fsds
fsds_amalg [] fsds = fsds
fsds_amalg (x:xs) (y:ys) = case (fsd_tail_position x, fsd_head_position y) 
                                of (Nothing, _) -> x:(fsds_amalg xs (y:ys))
                                   (_, Nothing) -> y:(fsds_amalg (x:xs) ys)
                                   _ -> (fsd_combine x y:(fsds_amalg xs ys))

-- | 'nt_to_functor_strings' takes a 'NaturalTransformation' of one of the 5 types
-- of the 'NaturalTransformation' which we can format (see 'NatFormatting') and returns
-- and 'OrderedFSDList' containing a list of 'FunctorStringData' representing all the 
-- strings in the string diagram for the given natural transformation.
nt_to_functor_strings :: NaturalTransformation -> OrderedFSDList
nt_to_functor_strings (NaturalTransformation n d s b o) 
    = nt_to_functor_strings_helper (NaturalTransformation n d s b o) 0 0 0 0 
nt_to_functor_strings (NatTransVerticalComposite (OneGlobelet (CompositeFunctor _ []) (CompositeFunctor _ [])) []) = []
nt_to_functor_strings (NatTransVerticalComposite (OneGlobelet (Functor _i d _b o) _ ) []) = [fsd]
    where fsd = FunctorStringData [FunctorElement (0,0), FunctorElement (1,0)] d o
nt_to_functor_strings (NatTransHorizontalComposite g nats) 
    = nt_to_functor_strings_helper (NatTransHorizontalComposite g nats) 0 0 0 0
nt_to_functor_strings (NatTransVerticalComposite _bg nts) = foldl fsds_amalg a as
    where
        (a:as) = zipWith5 nt_to_functor_strings_helper nts [0..((length nts) -1)] (repeat 0) (repeat 0) (repeat 0)

-- | A helper function for 'nt_to_functor_strings'.
nt_to_functor_strings_helper :: NaturalTransformation -> Int -> Int -> Int -> Int -> [FunctorStringData]
nt_to_functor_strings_helper (NaturalTransformation _n _d _s b _o) row top_offset bot_offset offset= fsds1 ++ fsds2
    where 
        source_fun_fsds = func_to_fsds (glob1_source b) row top_offset
        target_fun_fsds = func_to_fsds (glob1_target b) (row+1) bot_offset
        fsds1 = map (\fsd -> fsd_append fsd (NatElement (row,offset))) source_fun_fsds
        fsds2 = map (\fsd -> fsd_prepend (NatElement (row,offset)) fsd) target_fun_fsds
nt_to_functor_strings_helper (NatTransVerticalComposite (OneGlobelet (CompositeFunctor _ []) (CompositeFunctor _ [])) []) _ _ _ _ = []
nt_to_functor_strings_helper (NatTransVerticalComposite (OneGlobelet (Functor _i d _b o) _ ) []) row top_offset bot_offset _ = [fsd]
    where
        fsd = FunctorStringData [FunctorElement (row,top_offset), FunctorElement (row+1,bot_offset)] d o
nt_to_functor_strings_helper (NatTransHorizontalComposite _g nats) row top_offset bot_offset offset
    = concat $ zipWith5 nt_to_functor_strings_helper nats (repeat row) toffs boffs offs
    where
        toffs = map ((top_offset+).sum) $ inits $ map nat_source_length nats
        boffs = map ((bot_offset+).sum) $ inits $ map nat_target_length nats
        offs = map ((offset+).sum) $ inits $ map (fromEnum.is_basic_nt) nats
nt_to_functor_strings_helper _ _ _ _ _ = error $ "Error: The function nt_to_functor_strings_helper "
                                                    ++ "should only be defined for a row of natural transformations, "
                                                    ++ "i.e. it is only defined for natural transformations of types "
                                                    ++ "1 through 4 given in the documentation for NatFormatting."

-- | 'fsd_get_mid' returns the placement of the midpoint of a list of 'FunctorStringElement's.
-- Here the length between two consecutive 'FunctorStringElement' in the list is gotten using
-- 'fsd_lengths'.
--
-- It returns @(n,f)@ where @n@ is the index of the segment where the midpoint is located
-- (with the first segment being indexed by @0@) and
-- @f@ is on the interval [0,1) and specifies how far into this segment the midpoint is located.
fsd_get_mid :: [FunctorStringElement] -> (Int, Float)
fsd_get_mid loe = fsd_get_mid_helper mid 0 ls
    where
        ls = fsd_lengths loe
        mid = div (sum ls) 2
        fsd_get_mid_helper :: Int -> Int -> [Int] -> (Int,Float)
        fsd_get_mid_helper rem_len pos (x:xs) = if rem_len<x 
                                                then (pos,(fromIntegral rem_len)/(fromIntegral x))
                                                else fsd_get_mid_helper (rem_len-x) (pos+1) xs
        fsd_get_mid_helper _ _  [] = error "Error: the midpoint shouldn't be the endpoint or past the endpoint."

-- | 'fsd_lengths' gives the lengths of the segments, and is used in 'fsd_get_mid'.
-- Segments of the form @NatElement@ -- @FunctorStringElement@
--or @FunctorStringElement@ -- @NatElement@ have length 2, while
-- segments of the form @FunctorStringElement@ -- @FunctorStringElement@ have length 4.
fsd_lengths :: [FunctorStringElement] -> [Int]
fsd_lengths loes = zipWith seg_length loes (tail loes)

-- | 'seg_length' gives the length between two 'FunctorStringElement's, as described
-- in 'fsd_lengths'.
seg_length :: FunctorStringElement -> FunctorStringElement -> Int
seg_length (NatElement _) (FunctorElement _)= 2
seg_length (FunctorElement _) (NatElement _)= 2
seg_length (FunctorElement _) (FunctorElement _) = 4
seg_length _ _ = error $ "Error: The list of FunctorStringElements of a FunctorStringData "
                          ++ "should not have two consecutive NatElements"

-- | A helper function computing the coordinates of a control point under
-- the position described by a 'FunctorElement'.
fe_from_top_offset :: NatFormatting -> (Int,Int) -> Maybe TikzCoordinate
fe_from_top_offset nf (x,y) = do (a,b) <- nf_pos_to_coord nf (x,y)
                                 return $ Canvas a (b-0.5)

-- | A helper function computing the coordinates of a control point over
-- the position described by a 'FunctorElement'.
fe_from_bot_offset :: NatFormatting -> (Int,Int) -> Maybe TikzCoordinate
fe_from_bot_offset nf (x,y) = do (a,b) <- nf_pos_to_coord nf (x,y)
                                 return $ Canvas a (b+0.5)

-- | 'fse_fse_to_curve_op' takes a 'NatFormatting' and two consecutive 'FunctorStringElement's
-- in the 'fsd_list_of_elements' of a 'FunctorStringData' and creates a 'TikzPathOperation'
-- drawing the part of the string between the two points described by the
-- two 'FunctorStringElement's.
fse_fse_to_curve_op :: NatFormatting -> FunctorStringElement -> FunctorStringElement -> Maybe TikzPathOperation
fse_fse_to_curve_op nf (FunctorElement x1) (FunctorElement x2) = do c1 <- fe_from_top_offset nf x1
                                                                    c2 <- fe_from_bot_offset nf x2
                                                                    return $ PathOpCurveToTwoControls (pos_to_named_coord x2) c1 c2
fse_fse_to_curve_op nf (FunctorElement x1) (NatElement x2) = do c <- fe_from_top_offset nf x1
                                                                return $ PathOpCurveToOneControl (nt_pos_to_named_coord x2) c
fse_fse_to_curve_op nf (NatElement _x1) (FunctorElement x2) = do c <- fe_from_bot_offset nf x2
                                                                 return $ PathOpCurveToOneControl (pos_to_named_coord x2) c
fse_fse_to_curve_op _ _ _ = error $ "Error: The list of FunctorStringElements of a FunctorStringData "
                                    ++ "should not have two consecutive NatElements"

-- | 'fsd_to_tikz_path' takes a 'NatFormatting' and a 'FunctorStringData'
-- and gives the 'TikzPath' which draws the string in the string diagram
-- represented by the 'FunctorStringData'.
fsd_to_tikz_path :: NatFormatting -> FunctorStringData -> TikzPath
fsd_to_tikz_path nf (FunctorStringData fses ds opts) = catMaybes $ a:b:rest
    where
        a = Just $ PathOpOption "draw"
        b = Just $ PathOpMoveTo $ fse_get_named_coord $ head $ fses
        i = fsd_get_mid fses
        rest = fsd_to_tikz_path_helper i nf fses opts ds

-- | A helper function for 'fsd_to_tikz_path'.
fsd_to_tikz_path_helper :: (Int,Float) -> NatFormatting -> [FunctorStringElement] -> String -> String -> [Maybe TikzPathOperation]
fsd_to_tikz_path_helper (0,pos) nf (fse1:fse2:rest) opt ds = a:b:continuation
    where 
        a = fse_fse_to_curve_op nf fse1 fse2
        b = Just $ PathOpRelativeNode ("pos="++(show pos)++",auto,"++opt) ds
        continuation = zipWith (fse_fse_to_curve_op nf) (fse2:rest) rest
fsd_to_tikz_path_helper (n,pos) nf (fse1:fse2:rest) opt ds = a:fsd_to_tikz_path_helper (n-1,pos) nf (fse2:rest) opt ds
    where
        a = fse_fse_to_curve_op nf fse1 fse2
fsd_to_tikz_path_helper _ _ _ _ _ = error $ "Error. The list of FunctorStringElements of a completed "
                                       ++ "FunctorStringData should have at least two elements."
