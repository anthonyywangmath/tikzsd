{-|
Module      : Internal.TwoCatOfCats
Description : Defines categories, functors, and natural transformations
Copyright   : Anthony Wang, 2021
License     : MIT
Maintainer  : anthony.y.wang.math@gmail.com

@Internal.TwoCatOfCats@ defines data structures used to represent the categories,
    functors and natural transformations which are to be rendered by @tikzsd@
    into TikZ picture code.

Currently, the structures defined in this module include data relating
    how they are displayed by @tikzsd@, while the functions in the module
    relate purely to category theory.
I plan on rewriting this module so that it is only about category theory,
    separating out the display aspects in another module.
In the future, this may allow greater flexibility in how objects are displayed.
-}
module Internal.TwoCatOfCats where

import Prelude hiding (Functor)
import Control.Monad.Trans.Except (Except, throwE)

-- | A representation of a category.
data Category = Category
    { cat_id :: !String -- ^ the name of the category
    , cat_displayString :: !String -- ^ LaTeX code for displaying the category 
    } deriving (Eq)

instance Show Category where
    show c = "Category "++ (cat_displayString c)

-- | A @ZeroGlobelet@ is the structure representing the boundary of a functor (i.e.
--    1-morphism).
-- It consists of a source 'Category' and a target 'Category'.
data ZeroGlobelet = ZeroGlobelet
    { glob0_source :: !Category -- ^ source category
    , glob0_target :: !Category -- ^ target category
    } deriving (Eq)

instance Show ZeroGlobelet where
    show g = "(" ++ (show $ glob0_source g) ++", " ++ (show $ glob0_target g) ++")"

-- | A representation of a functor between categories.
data Functor = 
    -- | A data structure specifying a new functor.
    -- We shall call 'Functor's of the form @(Functor id ds bg o)@
    -- /basic functors/.
    Functor
    { func_id :: !String -- ^ the name of the functor
    , func_displayString :: !String -- ^ LaTeX code for displaying the functor
    , func_boundaryGlobelet :: !ZeroGlobelet -- ^ the boundary of the functor
    , func_options :: !String -- ^ LaTeX for discribing options in TikZ
    } |
    -- | A data structure for specifying compositions of functors.
    -- A 'Functor' of the form @(CompositeFunctor bg fl)@ is assumed to satisfy the
    -- axiom that the functors in the list @fl@ can be composed and that
    -- @bg@ is a valid boundary for the composition.
    -- We shall call such functors /composite functors/.
    --
    -- Explicitly, the target of each functor in @fl@ is the source of the next functor
    --  in the list.
    -- Moreover, if @fl@ is empty, then the source and target of @bg@ must be the same.
    -- (This represents an identity functor).
    -- Otherwise, the source of @bg@ is equal to the source of the first functor in @fl@,
    --    and the target of @bg@ is equal to the target of the last functor in @fl@.
    --
    -- See 'identityFunctor', 'func_compose' and 'func_compose_with_error' for 
    -- functions which compose functors safely.
    CompositeFunctor
    { cfs_boundaryGlobelet :: !ZeroGlobelet -- ^ The boundary globelet of the composite functor.
    , cfs_functorList :: ![Functor] -- ^ The list of functors to compose.
                                    -- The composition order is from left to right, so the list
                                    -- [F,G] represents a composition of F : A -> B and G : B -> C.
    } deriving Show

-- | A boundary globelet function which works for both basic and composite functors.
func_boundary :: Functor -> ZeroGlobelet
func_boundary (Functor _i _d bg _o) = bg
func_boundary (CompositeFunctor bg _fs) = bg

-- | 'func_source' gives the source category of a functor.
func_source :: Functor -> Category
func_source = glob0_source.func_boundary

-- | 'func_target' gives the target category of a functor.
func_target :: Functor -> Category
func_target = glob0_target.func_boundary

-- | 'identityFunctor' takes a category @C@ and returns the identity functor of
-- that category.
-- The identity functor is represented by a composite functor whose underlying
--     @cfg_functorList@ is empty.
identityFunctor :: Category -> Functor
identityFunctor c = CompositeFunctor (ZeroGlobelet c c) []

-- | 'func_composable' takes a list of functors and returns @True@
-- if the list of functors can be composed (without specifying additional information),
-- and @False@ otherwise.
-- 'func_composable' will return @False@ on an empty list, since a source (or target) category
-- needs to be specified.
-- Function composition is left to right, so [F,G] represents a composition of F : A -> B and
-- G : B -> C.
func_composable :: [Functor] -> Bool
func_composable [] = False --an empty functor list is not composable without a source category
                           --Use identityFunctor
func_composable fs = null $ func_composable_errors 0 fs

-- | A helper function for 'func_compose_with_error'.
func_composable_errors :: Int -> [Functor] -> [Int]
func_composable_errors _ (_:[]) = []
func_composable_errors n (f1:f2:fs)
    | func_target(f1) == func_source(f2) = func_composable_errors (n+1) (f2:fs)
    | otherwise                          = n:(func_composable_errors (n+1) (f2:fs))
func_composable_errors _ [] = error $ "func_composable_errors should not be called with an empty "
                                    ++ "second argument"

-- | 'func_compose' takes a list of functors and returns @Just@ their composite if
-- 'func_composable' is @True@ and @Nothing@ if 'func_composable' is @False@.
func_compose :: [Functor] -> Maybe Functor
func_compose fs
    | func_composable fs  = let bd = ZeroGlobelet (func_source $ head fs) (func_target $ last fs)
                                sfs = foldMap func_to_single_list fs
                                in Just(CompositeFunctor bd sfs)
    | otherwise           = Nothing


-- | 'FuncCompositionError' is the type of error which can be thrown by 'func_compose_with_error'.
-- 
-- @(FuncCompositionError [])@ represents an empty composition, which cannot
--  be composed without first specifying a source or target category.
--
-- Otherwise, an error is given by @(FuncCompositionError list)@ where @list@ is a list of positions 
-- where the functors do not compose.
-- @n@ is in @list@ if the functors in positions @n@ and @n+1@ do not compose.
-- (Indexing starts at 0).
data FuncCompositionError = FuncCompositionError [Int] 

-- | 'func_compose_with_error' takes a list of functors and composes them.
-- It throws a 'FuncCompositionError' describing why composition failed if 
-- the functors in the list could not be composed.
func_compose_with_error :: [Functor] -> Except FuncCompositionError Functor
func_compose_with_error [] = throwE $ FuncCompositionError []
func_compose_with_error fs 
    | null errs = let bd = ZeroGlobelet (func_source $ head fs) (func_target $ last fs)
                      sfs = foldMap func_to_single_list fs
                      in return $ CompositeFunctor bd sfs
    | otherwise = throwE $ FuncCompositionError errs
    where errs = func_composable_errors 0 fs

-- | 'func_to_single_composition' of @f@
-- gives a functor of the form @(CompositeFunctor bd fl)@ which is equal to @f@,
-- where @fl@ is a list of basic functors.
func_to_single_composition :: Functor -> Functor
func_to_single_composition f = CompositeFunctor (func_boundary f) (func_to_single_list f)

-- | 'func_to_single_list' @f@ returns the empty list if @f@
-- is an identity functor.
-- Otherwise, it returns a list of basic functors whose composition is equal to @f@.
func_to_single_list :: Functor -> [Functor]
func_to_single_list (Functor i ds bg o) = [(Functor i ds bg o)]
func_to_single_list (CompositeFunctor _bg fl) = (concat list_of_composed)
    where 
        list_of_composed = map func_to_single_list fl

-- | 'func_reduced_length' returns the length of 'func_to_single_list'.
-- For example @func_reduced_length@ of an identity functor is 0,
-- while @func_reduced_length@ of a basic functor is 1.
func_reduced_length :: Functor -> Int
func_reduced_length = length . func_to_single_list

-- | Two basic functors are deemed equal if @func_id@ and @func_boundaryGlobelet@ are equal.
-- After this identification, two 'Functor's are equal if they describe the same
-- morphism in the free category generated by the basic functors.
instance Eq Functor where
    f1 == f2 = helperCompare1 (func_to_single_composition f1) (func_to_single_composition f2)
        where
            helperCompare1 (CompositeFunctor bg1 f1s) (CompositeFunctor bg2 f2s) 
                = (bg1==bg2) && (length f1s == length f2s) && (foldl (&&) True (zipWith helperCompare2 f1s f2s))
            helperCompare1 _ _ = error $ "func_to_single_composition should be "
                                        ++"a CompositeFunctor so this shouldn't be matched."
            helperCompare2 (Functor i1 _d1 bg1 _o1) (Functor i2 _d2 bg2 _o2) =
                (i1 == i2) && (bg1 == bg2) 
            helperCompare2 _ _ = error $ "func_to_single_composition should be a CompositeFunctor of basic functors "
                                            ++ "so this shouldn't be matched."

-- | 'is_identity_func' of a functor is @True@ if the functor is an identity functor
-- and @False@ otherwise.
is_identity_func :: Functor -> Bool
is_identity_func f = func_reduced_length f == 0

-- | 'is_basic_func' of a functor is @True@ if the functor is a basic functor,
-- and @False@ otherwise.
is_basic_func :: Functor -> Bool
is_basic_func (Functor _ _ _ _) = True
is_basic_func _ = False

-- | 'OneGlobelet' is the structure representing the boundary of a natural transformation
-- (i.e. 2-morphism).
-- @(OneGlobelet s t)@ represents a globelet with source functor @s@ and target functor @t@,
--  and is assumed to satisfy the following axiom:
--
-- @func_boundary s == func_boundary t@
--
-- See 'funcs_to_globelet' for safe construction of @OneGlobelet@s.
data OneGlobelet  = OneGlobelet
    { glob1_source :: !Functor -- ^ source functor
    , glob1_target :: !Functor -- ^ target functor
    } deriving Show
   
-- | @(funcs_globeletable source target)@ is @True@ if the 'Functor's
--  @source@ and @target@ have the same boundary globelet, i.e. they have 
--  the same source category and the same target category,
--  and @False@ otherwise.
funcs_globeletable :: Functor -> Functor -> Bool
funcs_globeletable north south = func_boundary north == func_boundary south
    
-- | @(funcs_to_globelet source target)@ is @Just@ the
-- 'OneGlobelet' with source @source@ and target @target@ if
-- @(funcs_globeletable source target)=True@, and @Nothing@ otherwise.
funcs_to_globelet :: Functor -> Functor -> Maybe OneGlobelet
funcs_to_globelet north south
    | funcs_globeletable north south = Just (OneGlobelet north south)
    | otherwise                      = Nothing

-- | 'glob1_pos' of a @OneGlobelet@ is equal to the source category
-- of the source functor of the @OneGlobelet@.
-- Equivalently, it is equal to the source category of the target functor
-- of the @OneGlobelet@.
-- We shall refer to this category as the /positive pole/
-- of the @OneGlobelet@.
glob1_pos :: OneGlobelet -> Category
glob1_pos = func_source.glob1_source

-- | 'glob1_neg' of a @OneGlobelet@ is equal to the target category
-- of the target functor of the @OneGlobelet@.
-- Equivalently, it is equal to the target category of the source functor
-- of the @OneGlobelet@.
-- We shall refer to this category as the /negative pole/
-- of the @OneGlobelet@.
glob1_neg :: OneGlobelet -> Category
glob1_neg = func_target.glob1_target

-- | A representation of a natural transformation between functors.
data NaturalTransformation = 
    -- | A data structure specifying a new natural transformation.
    -- We shall call natural transformations of the form
    -- @(NaturalTransformation id ds ss bg o)@ /basic natural transformations/.
    NaturalTransformation
    { nt_id :: !String -- ^ name of the natural transformation
    , nt_displayString :: !String -- ^ LaTeX code for displaying the natural transformation
    , nt_shapeString :: !String -- ^ LaTeX code for the shape of the node used for the natural transformation
    , nt_boundaryGlobelet :: !OneGlobelet -- ^ boundary globelet of the natural transformation
    , nt_options :: !String -- ^ LaTeX code for options when displaying the natural transformation
    } |
    -- | A data structure for specifying a horizontal composition of natural transformations.
    -- A 'NaturalTransformation' of the form @(NatTransHorizontalComposite bg ntl)@
    -- is assumed to satisfy the axiom that @ntl@ is a list of natural transformations which
    -- can be horizontally composed, with @bg@ the boundary for the composition.
    --
    -- Explicitly, @ntl@ is a non-empty list of @NaturalTransformation@s
    -- such that the negative pole of each natural transformation in the list is the positive pole
    -- of the next element in the list.
    -- The source of @bg@ is equal to the composition of the source functors of the natural transformations
    -- in @ntl@
    -- and the target of @bg@ is equal to the composition of the target functors of the natural
    -- transformations in @ntl@.
    --
    -- See the 'nat_horz_compose' and 'nat_horz_compose_with_error' functions for safe horizontal
    -- composition of natural transformations.
    NatTransHorizontalComposite
    { nt_horz_comp_boundaryGlobelet :: !OneGlobelet -- ^ the boundary globelet of the horizontal composition
    , nt_horz_comp_list :: ![NaturalTransformation] -- ^ the list of natural transformations to be horizontally
                                                    -- composed. Composition is from left to right.
    } |
    -- | A data structure for specifying a vertical composition of natural transformations.
    -- A 'NaturalTransformation' of the form @(NatTransVerticalComposite bg ntl)@
    -- is assumed to satisfy the axiom that @ntl@ is a list of natural transformations
    -- which can be vertically composed, with @bg@ a valid boundary for the composition.
    --
    -- Explicitly, @ntl@ is a list of @NaturalTransformation@s
    -- such that the target functor of each natural transformation in the list is
    -- equal to the source functor of the next natural transformation.
    -- If @ntl@ is empty, then the source and target functors of @bg@ must be equal.
    -- (This represents an identity natural transformation.)
    -- Otherwise, the source functor of @bg@ is equal to the source functor of the first
    -- natural transformation in @ntl@ and the target functor of @bg@ is equal to the target
    -- functor of the last natural transformation in @ntl@.
    --
    -- See 'identityNaturalTransformation' and 'nat_vert_compose' for functions which
    -- vertically compose natural transformations safely.
    NatTransVerticalComposite
    { nt_vert_comp_boundaryGlobelet :: !OneGlobelet -- ^ the boundary globelet of the vertical composition
    , nt_vert_comp_list :: ![NaturalTransformation] -- ^ the list of natural transformations to be vertically composed.
                                                    -- Composition is from left to right.
    } deriving Show

-- | 'nat_boundary' of a natural transformation is its boundary globelet.
nat_boundary :: NaturalTransformation -> OneGlobelet
nat_boundary (NaturalTransformation _i _ds _ss bg _o) = bg
nat_boundary (NatTransHorizontalComposite bg _nl) = bg
nat_boundary (NatTransVerticalComposite bg _nl) = bg

-- | 'nat_source' of a natural transformation is its source functor.
nat_source :: NaturalTransformation -> Functor
nat_source = glob1_source.nat_boundary

-- | 'nat_target' of a natural transforamtion is its target functor.
nat_target :: NaturalTransformation -> Functor
nat_target = glob1_target.nat_boundary

-- | 'nat_pos' of a natural transformation is its /positive pole/
-- which we define to be the positive pole of its boundary globelet,
-- i.e. the source of its source, or equivalently, the source of its target.
nat_pos :: NaturalTransformation -> Category
nat_pos = glob1_pos.nat_boundary

-- | 'nat_neg' of a natural transformation is its /negative pole/
-- which we define to be the negative pole of its boundary globelet,
-- i.e. the target of its target, or equivalently, the target of its source.
nat_neg :: NaturalTransformation -> Category
nat_neg = glob1_neg.nat_boundary

-- | 'identityNaturalTransformation' takes a 'Functor' as an argument and returns
-- its identity natural transformation.
-- The identity natural transformation is represented by a vertical composition
-- whose @nt_vert_comp_list@ is empty.
identityNaturalTransformation :: Functor -> NaturalTransformation 
identityNaturalTransformation f = NatTransVerticalComposite (OneGlobelet f f) []

-- | 'nat_horz_composable' takes a list of natural transformations and returns
-- @True@ if the natural transformations can be horizontally composed,
-- and @False@ otherwise.
nat_horz_composable :: [NaturalTransformation] -> Bool
nat_horz_composable ns = case func_compose (map nat_source ns) of
                              Nothing -> False
                              _       -> True

-- | 'nat_horz_compose' takes a list of natural transformations and returns
-- @Just@ their horizontal composition if they are horizontally composable,
-- and @Nothing@ otherwise.
nat_horz_compose :: [NaturalTransformation] -> Maybe NaturalTransformation
nat_horz_compose ns = do source_functor <- func_compose (map nat_source ns)
                         target_functor <- func_compose (map nat_target ns)
                         let boundary_globelet = OneGlobelet source_functor target_functor
                         return (NatTransHorizontalComposite boundary_globelet ns)

-- | 'NatHorzCompositionError' is the possible error thrown by 'nat_horz_compose_with_error'.
--
-- @(NatHorzCompositionError [])@ represents an empty horizontal composition.
--
-- Otherwise, an error is given by @(NatHorzCompositionError list)@ where
-- @list@ is a list of positions where the natural transformations do not 
--  horizontally compose.
-- @n@ is in @list@ if the negative pole of the natural transformation in position
-- @n@ is not equal to the positive pole of the natural transformation in position
-- @n+1@.
-- (Indexing starts at 0).
data NatHorzCompositionError = NatHorzCompositionError [Int]

-- | A helper function for 'nat_horz_compose_with_error'.
nat_horz_composable_errors :: Int -> [NaturalTransformation] -> [Int]
nat_horz_composable_errors _ (_:[]) = []
nat_horz_composable_errors n (nat1:nat2:nats)
    | nat_neg(nat1) == nat_pos(nat2) = nat_horz_composable_errors (n+1) (nat2:nats)
    | otherwise                      = n:(nat_horz_composable_errors (n+1) (nat2:nats))
nat_horz_composable_errors _ [] = error $ "nat_horz_compose_errors should not be called with "
                                            ++ "an empty second argument."

-- | 'nat_horz_compose_with_error' takes a list of natural transformations and returns their
-- horizontal composition.
-- It throws a 'NatHorzCompositionError' describing why composition failed if the
-- list of natural transformations cannot be horizontally composed.
-- Composition is from left to right.
nat_horz_compose_with_error :: [NaturalTransformation] -> Except NatHorzCompositionError NaturalTransformation
nat_horz_compose_with_error [] = throwE $ NatHorzCompositionError []
nat_horz_compose_with_error  nats
    | null errs = let pos = nat_pos $ head nats
                      neg = nat_neg $ last nats
                      podes = ZeroGlobelet pos neg
                      bd_north = CompositeFunctor podes $ foldMap (func_to_single_list.nat_source) nats
                      bd_south = CompositeFunctor podes $ foldMap (func_to_single_list.nat_target) nats
                      bd = OneGlobelet bd_north bd_south
                      in return (NatTransHorizontalComposite bd nats)
    | otherwise = throwE $ NatHorzCompositionError errs
    where errs = nat_horz_composable_errors 0 nats

-- | 'nat_vert_composable' takes a list of natural transformations and returns @True@ if the list
--  of natural transformations can be vertically composed (without specifying additional information),
--  and @False@ otherwise.
-- 'nat_vert_composable' will return @False@ on an empty list since a source
-- (or target) functor needs to be specified.
nat_vert_composable :: [NaturalTransformation] -> Bool
nat_vert_composable [] = False 
nat_vert_composable (_:[]) = True
nat_vert_composable (n1:n2:ns)
    | nat_target n1 == nat_source n2 = nat_vert_composable (n2:ns)
    | otherwise                      = False

-- | 'nat_vert_compose' takes a list of natural transformations and returns @Just@ their vertical
-- composition if the list is vertically composable, and @Nothing@ otherwise.
nat_vert_compose :: [NaturalTransformation] -> Maybe NaturalTransformation
nat_vert_compose ns
    | nat_vert_composable ns = let bd = OneGlobelet (nat_source $ head ns) (nat_target $ last ns)
                                   in Just (NatTransVerticalComposite bd ns)
    | otherwise              = Nothing

-- | 'nat_source_length' of a natural transformation is the 'func_reduced_length' of its source
-- functor.
nat_source_length :: NaturalTransformation -> Int
nat_source_length = func_reduced_length . nat_source

-- | 'nat_target_length' of a natural transformation is the 'func_reduced_length' of its target
-- functor.
nat_target_length :: NaturalTransformation -> Int
nat_target_length = func_reduced_length . nat_target

-- | 'is_basic_nt' of a natural transformation is @True@ if the natural transformation is a
-- basic natural transformation, and false otherwise.
is_basic_nt :: NaturalTransformation -> Bool
is_basic_nt (NaturalTransformation {}) = True
is_basic_nt _ = False

-- | 'is_identity_nt' of a natural transformation is @True@ if the natural transformation
-- is the identity of some functor, and @False@ otherwise.
is_identity_nt :: NaturalTransformation -> Bool
is_identity_nt (NatTransVerticalComposite _ []) = True
is_identity_nt _ = False
