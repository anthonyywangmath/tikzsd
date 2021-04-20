{-|
Module      : TwoCatOfCats
Description : Safe version of Internal.TwoCatOfCats
Copyright   : Anthony Wang, 2021
License     : MIT
Maintainer  : anthony.y.wang.math@gmail.com

@TwoCatOfCats@ is a safe version of @Internal.TwoCatOfCats@.
Please refer to the documentation for that module.

This module does not export the unsafe constructors
    in @Internal.TwoCatOfCats@, but does export the safe versions.
For example, @CompositeFunctor :: ZeroGlobelet -> [Functor] -> Functor@ is not exported,
    but @func_compose :: [Functor] -> Maybe Functor@ is.
This is because one can use @CompositeFunctor@ to construct invalid composite functors.
See the documentation in @Internal.TwoCatOfCats@ for more details.
-}
module TwoCatOfCats (
    Category (..),

    ZeroGlobelet (..),

    Functor (Functor, func_id, func_displayString),
    func_boundary, func_source, func_target,
    identityFunctor,
    func_composable,
    func_compose,
    FuncCompositionError (..),
    func_compose_with_error,
    func_to_single_composition, func_to_single_list,
    func_reduced_length,
    is_basic_func,
    is_identity_func,

    OneGlobelet,
    funcs_globeletable,
    funcs_to_globelet,
    glob1_pos,
    glob1_neg,

    NaturalTransformation (NaturalTransformation, nt_id, nt_displayString),
    nat_boundary, nat_source, nat_target,
    identityNaturalTransformation,
    nat_horz_compose,
    NatHorzCompositionError (..),
    nat_horz_compose_with_error,
    nat_vert_compose,
    nat_source_length, nat_target_length, 
    is_basic_nt,
    is_identity_nt
) where

import Prelude hiding (Functor)
import Internal.TwoCatOfCats
