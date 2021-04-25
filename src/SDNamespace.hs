{-|
Module      : SDNamespace
Description : Defines the state @SDNamespace@ along with associated functions modifying and using this state.
Copyright   : Anthony Wang, 2021
License     : MIT
Maintainer  : anthony.y.wang.math@gmail.com

This module defines @SDNamespace@, a state which keeps track of the defined 
categories, functors and natural transformations.

The most important function in this module is 'handle_sdc', which takes
a 'SDCommand' and does the corresponding action, i.e.
adding it to the 'SDNamespace' state for define actions, and reading from
the 'SDNamespace' state and writing an output file for draw actions.
-}
{-# LANGUAGE RankNTypes #-}
module SDNamespace where

import Prelude hiding (Functor)
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State.Strict (State, runState, get, put, modify)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT, maybeToExceptT)
import Control.Monad.Except (Except, ExceptT, mapExceptT, runExceptT, withExceptT, throwError, withExcept)
import Control.Applicative ((<$>),(<|>))
import Control.Lens.Type (Lens')
import Control.Lens.Getter (view)
import Control.Lens.Setter (set, over)
import Control.Lens.Combinators (lens)
import Control.Lens.Tuple (_1,_2,_3)
import qualified Data.Map.Strict as Map
import Data.Functor.Identity (runIdentity)
import Data.Maybe (isNothing, fromJust)
import Data.List (partition, intercalate)
import Data.Either (isLeft)
import System.IO (stderr, hPutStrLn)
import TwoCatOfCats
import SDParser
import Internal.FormattingData
import TikzObjects
import TikzStringDiagram

-- | A class for 'Category', 'Functor' and 'NaturalTransformation',
--  defining some functions useful for common handling of all three data structures.
--
--  Minimal complete definition : 'get_id', 'struct_str' and 'sdns_lens'.
class Structure a where
    get_id :: a -> String -- ^ gets an id string.
                          -- For functors it is defined for basic functors and identity functors.
                          -- For natural transformations, it is defined for basic natural
                          -- transformations, identity natural transformations of basic functors,
                          -- and identity natural transformations of identity functors of
                          -- categories.
    struct_str :: a->String -- ^ a string saying which type of structure the object is
    sdns_lens :: a -> Lens' SDNamespace (Namespace a) -- ^ A lens for getting the corresponding namespace in an 'SDNamespace'
    insertion_error_msg :: a -> String  -- ^ derived from 'get_id' and 'struct_str'
    insertion_error_msg s = "The "++(struct_str s) ++" id "++(get_id s)
                                ++" is already in the "++(struct_str s) ++" namespace. Skipping."
instance Structure Category where
    get_id = cat_id
    struct_str _ = "category"
    sdns_lens _ = category
instance Structure Functor where
    get_id f
        | is_basic_func f = func_id f
        | is_identity_func f = get_id $ func_source f
    get_id _ = error "Error: get_id is not defined for the given functor."
    struct_str _ = "functor"
    sdns_lens _ = functor
instance Structure NaturalTransformation where
    get_id nt 
        | is_basic_nt nt = nt_id nt
        | is_identity_nt nt = get_id $ nat_source nt
    get_id _ = error "Error: get_id is not defined for the given natural transformation."
    struct_str _ = "natural transformation"
    sdns_lens _ = nat_trans

-- | @(Namespace a)@ is a type synonym for a @Map@ from @String@ to @a@.
-- In our case @a@ is either 'Category', 'Functor' or 'NaturalTransformation',
-- and the map will map an id string to the corresponding defined structure.
type Namespace a = Map.Map String a

-- | 'SDNamespace' consists of a 3-tuple of 'Namespace's, one for 'Category', one for
-- 'Functor' and one for 'NaturalTransformation'.
--
-- All functors in the functor 'Namespace' are assumed to be basic functors.
-- All natural transformations in the natural transformation 'Namespace' are assumed to be
-- basic natural transformations.
type SDNamespace = (Namespace Category, Namespace Functor, Namespace NaturalTransformation)

-- | An 'SDNamespace' where all 'Namespace's are empty.
empty_sdns :: SDNamespace
empty_sdns = (Map.empty, Map.empty, Map.empty)

-- | A lens from an 'SDNamespace' into the 'Namespace' of defined categories.
category :: Lens' SDNamespace (Namespace Category)
category = _1

-- | A lens from an 'SDNamespace' into the 'Namespace' of defined functors.
functor :: Lens' SDNamespace (Namespace Functor)
functor = _2

-- | A lens from an 'SDNamespace' into the 'Namespace' of defined natural transformations.
nat_trans :: Lens' SDNamespace (Namespace NaturalTransformation)
nat_trans = _3

-- | The obvious action: given a lens from an object of type @a@ to an object of type @b@
-- and a state action taking a state of type @b@ and outputting a new state of type @b@
-- along with an object of type @c@, we get a new state action by taking a state of type
-- @a@, using the lens to view this to get an object of type @b@, running the given state action
-- to get a new object of type @b@ and an object of type @c@, and finally, using the lens
-- to set this new object of type @b@ back into the original state of type @a@.
processing :: (Lens' a b) -> (State b c) -> (State a c)
processing lns st = do a_obj <- get
                       let b_obj = view lns a_obj
                       let (out,b_obj') = runState st b_obj
                       put (set lns b_obj' a_obj)
                       return out

-- | Viewing the lens @get@ action as a state action which does not change the underlying state.
lens_get :: (Lens' a b) -> (State a b)
lens_get lns = do a_obj <- get
                  return (view lns a_obj)

-- | 'sdns_lookup' takes a @String@ and a lens from 'SDNamespace' into one of its component
-- 'Namespace's, and attempts to lookup the string in that namespace.
sdns_lookup :: (Structure a) => String -> Lens' SDNamespace (Namespace a) -> MaybeT (State SDNamespace) a
sdns_lookup str lns = do ns <- lift $ lens_get lns
                         MaybeT $ return (Map.lookup str ns)

-- | A helper function for 'insert_action'.
insert_action' :: (Structure a) => a -> State (Namespace a) (IO ())
insert_action' obj = do let key = get_id obj
                        curr_state <- get
                        case Map.member key curr_state of 
                            True -> return $ hPutStrLn stderr $ insertion_error_msg obj
                            False -> modify (Map.insert key obj) >> return (return ());

-- | @(insert_action obj)@ is the @State (SDNamespace a) (IO ())@
--  which adds the key value pair @(get_id obj, obj)@ to the correct namespace if
--  @(get_id obj)@ is not already a key in the namespace, 
--  and gives the IO action printing an error message to stderr otherwise.
insert_action :: (Structure a) => a -> State SDNamespace (IO ())
insert_action s = processing (sdns_lens s) (insert_action' s)

-- | A partial function on 'SDCommand's which were constructed using 'DefineCat'.
-- See 'handle_sdc'.
handle_def_cat :: SDCommand -> State SDNamespace (IO ())
handle_def_cat (DefineCat cid ds) = insert_action (Category cid ds)
handle_def_cat _ = error $ "Error! handle_def_cat should only be called by handle_sdc,"
                                ++ " which should only call handle_def_cat when handling"
                                ++ " DefineCat SDCommands"

-- | A partial function on 'SDCommand's which were constructed using 'DefineFunc'.
-- See 'handle_sdc'.
handle_def_fun :: SDCommand -> State SDNamespace (IO ())
handle_def_fun (DefineFunc f_id ds source_id target_id opts) = 
    do source <- runMaybeT $ sdns_lookup source_id category
       target <- runMaybeT $ sdns_lookup target_id category
       case (source, target) of 
            (Nothing, Nothing) -> return $ (hPutStrLn stderr source_not_found_error) 
                                            >> (hPutStrLn stderr target_not_found_error)
            (Nothing, _)       -> return $ hPutStrLn stderr $ source_not_found_error 
            (_, Nothing)       -> return $ hPutStrLn stderr $ target_not_found_error
            (Just s, Just t)   -> insert_action $ Functor f_id ds (ZeroGlobelet s t) opts
    where 
        source_not_found_error = "The category " ++ source_id ++ " could not be found.\n\t"
                                    ++"When giving the source in the definition of the functor "++f_id
        target_not_found_error = "The category " ++ target_id ++ " could not be found.\n\t"
                                    ++"When giving the target in the definition of the functor "++f_id
handle_def_fun _ = error $ "Error! handle_def_fun should only be called by handle_sdc,"
                                ++ " which should only call handle_def_fun when handling"
                                ++ " DefineFunc SDCommands"

-- | A partial function on 'SDCommand's which were constructed using 'DefineNat'.
-- See 'handle_sdc'.
handle_def_nat :: SDCommand -> State SDNamespace (IO ())
handle_def_nat (DefineNat ntid ds source target opts shape) =
    do source_f <- runExceptT $ read_functor_line source
       target_f <- runExceptT $ read_functor_line target
       case (source_f, target_f) of 
            (Left err, _) -> return $ do hPutStrLn stderr $ error_msg err
                                         hPutStrLn stderr $ "\tWhen describing the source in the definition of "++ntid
            (_, Left err)     -> return $ do hPutStrLn stderr $ error_msg err
                                             hPutStrLn stderr $ "\tWhen describing the target in the definition of "++ntid
            (Right (s,_), Right (t,_)) -> case (is_identity_func s) && (is_identity_func t) of
                True -> return $ hPutStrLn stderr $ source_target_identity_error
                False -> let bg = funcs_to_globelet s t in
                    case bg of Nothing -> return $ hPutStrLn stderr $ boundary_not_globelet_error
                               Just b  -> insert_action $ NaturalTransformation ntid ds shape b opts
    where
        boundary_not_globelet_error 
            = "The source and target in the definition of the natural transformation "
               ++ ntid ++ " do not have the same source/target."
        source_target_identity_error 
            = concat ["Currently, creating a natural transformation "
                     ,"whose source and target are both identity functors is not supported, "
                     ,"as this will result in a TikZ node for the natural transformation "
                     ,"with no in strings and no out strings. "
                     ,"\nExplicitly define an identity functor instead, "
                     ,"so that there is an in string and an out string"]
handle_def_nat _ 
    = error $ "Error! handle_def_nat should only be called by handle_sdc,"
               ++ " which should only call handle_def_nat when handling"
               ++ " DefineNat SDCommands"

-- | A lens from a 'Category' to its string of options.
cat_opt_lens :: Lens' Category String
cat_opt_lens = lens (const "") const

-- | A lens from a 'Functor' to its string of options.
func_opt_lens :: Lens' Functor String
func_opt_lens = lens get_opts change_opts
    where
        get_opts (Functor _i _d _b o) = o
        get_opts _ = error "currently can only get options of a basic functor"
        change_opts (Functor i d b _o) new_o = Functor i d b new_o
        change_opts _ _ = error "currently can only change options of a basic functor"

-- | A lens from a 'NaturalTransformation' to its string of options.
nat_opt_lens :: Lens' NaturalTransformation String
nat_opt_lens = lens get_opts change_opts
    where
        get_opts (NaturalTransformation _i _d _s _b o) = o
        get_opts _ = error "currently can only get options of a basic natural transformation"
        change_opts (NaturalTransformation i d s b _o) new_o = NaturalTransformation i d s b new_o
        change_opts _ _ = error "currently can only change options of a basic natural transformation"

-- | @(sdns_lookup_add str lns1 added lns2)@ looks up @str@ from the 'Namespace' extracted using @lns1@ 
-- from the 'SDNamespace', then modifies the
--looked-up object @o@ by adding @","++added@ to the end of @lns2@ of the object @o@.
sdns_lookup_add :: (Structure a)=> String -> Lens' SDNamespace (Namespace a)-> String -> Lens' a String 
    -> MaybeT (State SDNamespace) a
sdns_lookup_add str lns1 "" _ = sdns_lookup str lns1
sdns_lookup_add str lns1 added lns2 
    = do obj <- sdns_lookup str lns1
         return $ (over lns2 (`mappend` (',':added))) obj

-- | @(sdns_chain_lookup_func id opt)@ attempts to lookup the @id@ in the functor
-- @Namespace@ and adds the options @opt@ to the options of the looked up functor.
-- If it cannot find @id@ in the functor namespace, it looks up @id@ in the category 'Namespace' and returns
-- the identity functor of the resulting category.
sdns_chain_lookup_func :: String -> String -> MaybeT (State SDNamespace) Functor
sdns_chain_lookup_func eid opt 
    = sdns_lookup_add eid functor opt func_opt_lens 
      <|> (identityFunctor <$> sdns_lookup eid category)

-- | @(sdns_chain_lookup_nat id opt)@ attemps to lookup the @id@ in the natural transformation
-- @Namespace@ and adds the options @opt@ to the options of the looked up natural transformation.
-- If it cannot find @id@ in the natural transformation namespace, it looks up @id@ in the
-- functor namespace, and returns the identity natural transformation of the functor if it finds it
-- there.
-- If it cannot find @id@ in either the natural transformation or functor namespaces, it looks up
-- @id@ in the category namespace, and returns the identity natural transformation of the identity
-- functor of the category if it finds it there.
sdns_chain_lookup_nat :: String -> String -> MaybeT (State SDNamespace) NaturalTransformation
sdns_chain_lookup_nat eid opt 
    =  sdns_lookup_add eid nat_trans opt nat_opt_lens 
       <|> (identityNaturalTransformation <$> sdns_lookup eid functor)
       <|> (identityNaturalTransformation <$> identityFunctor <$> sdns_lookup eid category)

-- | A class for errors which have error messages.
class Error a where
    error_msg :: a->String

-- | 'FunctorReadError' is the type of error that can be thrown by 'read_functor_line'.
-- Either some of the functors in the lookup do no exist,
--  or the list of functors do not compose.
--
-- 'LookupFunctorError' @list@ says that there are functors which cannot be found
-- in the 'SDNamespace'.
-- Here @list@ is a list of pairs @(n,id)@ where @n@ is the position in the functor line
-- where the given @id@ cannot be found.
--
-- 'ComposeFunctorError' @list@ says that the composition could not be determined.
-- Here, @list@ is empty if the functor line has no functors, i.e. it specifies an empty
-- composition.
-- Otherwise, it is a list of 4-tuples @(n1,id1,n2,id2)@ where @n1@ is the position of @id1@
-- and @n2@ is the position of @id2@, and the functors specified by @id1@ and @id2@ do not compose.
data FunctorReadError = LookupFunctorError [(Int,String)] | ComposeFunctorError [(Int,String,Int,String)] 

instance Error FunctorReadError where
    error_msg (LookupFunctorError places) 
        = "The id(s) " ++ (intercalate ", " $ map lfe_msg_helper places)
           ++" could not be found in either the"
           ++" functor or category namespaces."
    error_msg (ComposeFunctorError []) 
        = "Cannot form a composition of an empty list of functors. "
           ++"Categories can be used to denote their identity functors."
    error_msg (ComposeFunctorError places) 
        = "The functors " ++ (intercalate ", " $ map cfe_msg_helper places)
           ++" cannot be composed."

-- | A helper function used to define 'error_msg' of a 'LookupFunctorError'
lfe_msg_helper :: (Int,String) -> String
lfe_msg_helper (n, str) = concat [str," in position ", show n]

-- | A helper function used to define 'error_msg' of a 'ComposeFunctorError'
cfe_msg_helper :: (Int,String,Int,String) -> String
cfe_msg_helper (n1, str1, n2, str2) 
    = concat [str1, " at position ", show n1, " and ", str2, " at position ", show n2]

-- | 'read_functor_line' takes a list of 'CompElement's representing a collection of functors or empty
-- spaces, either throws a 'FunctorReadError' or returns a pair @(f,ff)@ where @f@ is the composite 'Functor' and
-- @ff@ is the 'FunctorFormatting' associated to @f@ described by the spacing in the original list of
-- @CompElement@s.
-- See the user's manual for how @ff@ and @f@ are determined.
read_functor_line :: [CompElement] -> ExceptT FunctorReadError (State SDNamespace) (Functor, FunctorFormatting)
read_functor_line list 
    = do items <- list_ce_to_funcs list
         (c,l) <- mapExceptT (return.runIdentity) $ compose_funcs items
         return (c, FunctorFormatting (length list) l)

-- | 'list_ce_to_funcs' is used in 'read_functor_line'.
-- It takes a list of 'CompElement's
-- either throws a 'LookupFunctorError' or gives a list of @(n,id,f)@ for each
-- non-@Empty@ 'CompElement' in the list,
-- where @n@ is the index in the original list the non-@Empty@ 'CompElement' resides
--      (with indexing starting at @0@), 
--  @id@ is the @id@ of the functor,
--  and @f@ is the functor from the 'SDNamespace' corresponding to the 'CompElement'.
list_ce_to_funcs :: [CompElement] -> ExceptT FunctorReadError (State SDNamespace) [(Int, String, Functor)]
list_ce_to_funcs list 
    = let list_with_pos = zip [0..] list
          (posits, list_ne) = unzip $ filter (\(_x,_y) -> _y /= SDParser.Empty) list_with_pos
          ids = map ce_id list_ne
      in do funcs <- lift $ mapM (\(CompElement cid opts) -> runMaybeT $ sdns_chain_lookup_func cid opts) list_ne
            let list_with_pos_id = zip3 posits ids funcs
            let (lookup_errors,lookup_good) = partition (\(_x,_y,_z) -> isNothing _z) list_with_pos_id
            if null lookup_errors 
            then return $ map (\(x,y,Just z) -> (x,y,z)) lookup_good
            else throwError $ LookupFunctorError $ map (\(_x,_y,_z)-> (_x,_y)) lookup_errors

-- | 'compose_funcs' is used in 'read_functor_line'.
-- It takes an object of type @[(Int,String,Functor)]@ which is outputted by 'list_ce_to_funcs',
-- and attempts to compose the functors in the list.
-- It either throws a 'ComposeFunctorError' or gives a pair @(f,l)@
-- where @f@ is the composite functor
-- and @l@ is the list of the positions of the non-identity functors in the original list.
--
-- Here all the functors in the original list are assumed to either be of the form @(Functor i d b o)@
-- or be an identity functor.
compose_funcs::[(Int,String,Functor)] -> Except FunctorReadError (Functor,[Int])
compose_funcs list 
    = do comp <- withExcept mExcept $ func_compose_with_error $ map (view _3) list
         return (comp, map (view _1) $ filter (not.is_identity_func.(view _3)) list)
    where
        mExcept (FuncCompositionError errs) 
            = let lefts = map (\x-> list !! x) errs
                  rights = map (\x-> list !! (x+1)) errs
              in ComposeFunctorError $ zipWith (\(x,y,_z)-> (\(a,b,_c)-> (x, y, a, b))) lefts rights

-- | 'NatTransReadError' is the type of error which can be thrown by 'read_nat_trans'.
--
-- - @(LookupNatTransError m list)@ is an error thrown when reading a line representing a horizontal
-- composition of basic natural transformations and identity natural transformations. 
-- It says that some id could not be found in the
-- 'SDNamespace'. 
-- The number @m@ is the line number where the error occurs
-- is a list of pairs @(n,id)@ where @n@ is the position in the line where @id@
-- could not be found in the 'SDNamespace'.
--
-- - @(ImputationError m n)@ is an error thrown when imputing identity natural transformations on line @m@.
-- It says that the natural transformation in position @n@ on this line could not be imputed due
-- to the target functor of the previously specified lines is not the composition of enough basic functors.
--
-- - @(HorzComposeNatTransError m list)@ is an error thrown when reading a line representing a
-- horizontal composition of basic natural transformations and identity natural transformations.
-- It says that the looked up functors could not be horizontally composed.
-- The number @m@ is the line number where the error occurs, and
-- @list@ is a list of 4-tuples @(n1,str1,n2,str2)@, where 
--  @n1@ is the position in the line of @id1@ and @n2@ is the position in the line of @id2@
--  and the natural transformations specified by @id1@ and @id2@ cannot be horizontally composed.
-- An empty list corresponding to an empty composition.
--
-- - @NoLinesError@ is an error which is thrown when there are no lines when specifying the
-- natural transformation.
--
-- - @FirstLineImputationError@ is an error which is thrown when the first line in the specification
-- of a natural transformation contains empty places, meaning that these places cannot be imputed.
--
-- - @(FRE m fre)@ is an error which is thrown when line @m@ is a line specifying a functor,
-- and @fre@ is a 'FunctorReadError' thrown when reading this line.
--
-- - @(TwoConsecutiveFunctorsError m)@ says that line @m-1@ and line @m@ are both used to specify a 
-- functor.
--
-- - @(IncompatibleLinesError m)@ says that line @m@ is incompatible with the previously specified
-- lines.
-- If line @m@ is a line specifying a functor, this means that the target of the natural
-- transformation specified by the previous line is not equal to the functor specfied by line @m@.
-- If line @m@ is a line specifying a natural transformation, it says that the target of the natural
-- transformation specified by the previous lines is not equal to the source of the natural
-- transformation specified by line @m@.
data NatTransReadError = LookupNatTransError Int [(Int, String)]
                       | ImputationError Int Int
                       | HorzComposeNatTransError Int [(Int,String,Int,String)]
                       | NoLinesError
                       | FirstLineImputationError
                       | FRE Int FunctorReadError
                       | TwoConsecutiveFunctorsError Int
                       | IncompatibleLinesError Int

instance Error NatTransReadError where
    error_msg (LookupNatTransError line places) 
        = "On line "++(show line)++" the id(s) "
           ++(intercalate ", " $ map lnte_msg_helper places) 
           ++" could not be found in the natural transformation, "
           ++"functor or category namespaces."
    error_msg (ImputationError line position) 
        = "On line "++(show line)++", position "
           ++(show position)++ " could not be imputed: "
           ++"the target functor of the previous lines"
           ++" does not have enough basic functors."
    error_msg (HorzComposeNatTransError line []) 
        = "On line "++(show line)++", cannot form an empty horizontal composition "
           ++"of natural transformations."
        -- in the current iteration of the program, I don't think this will ever be matched, as
        -- every parsed SDDrawNat line should be a list of length at least 1.
    error_msg (HorzComposeNatTransError line places) 
        = "On line "++(show line)++", the natural transformations " 
           ++ (intercalate ", "$ map hcnte_msg_helper places)
           ++" cannot be horizontally composed."
    error_msg NoLinesError 
        = "Error: empty natural transformation."
    error_msg FirstLineImputationError 
        = "Cannot impute functors on in a natural transformation without specifying a source."
    error_msg (FRE line fre) 
        = "On line "++(show line)++": "++ error_msg fre
    error_msg (TwoConsecutiveFunctorsError line) 
        = "Error: two consecutive functor lines "++ (show $ line-1)
           ++" and " ++(show line) 
           ++ "in the specification of a natural transformation."
    error_msg (IncompatibleLinesError line) 
        = "Line "++(show line)++" is incompatible with the previously specified lines."

-- | A helper function in defining 'error_msg' of a 'LookupNatTransError'.
lnte_msg_helper :: (Int,String) -> String
lnte_msg_helper = lfe_msg_helper

-- | A helper function in defining 'error_msg' of a 'HorzComposeNatTransError'.
hcnte_msg_helper :: (Int, String, Int, String) -> String
hcnte_msg_helper = cfe_msg_helper

-- | 'list_ce_to_nt' takes a list of 'CompElement's representing natural transformations
--and the current line number
--and either throws a 'LookupNatTransError' or returns a list of @Maybe NaturalTransformation@
--gotten by mapping @Empty@ to @Nothing@
--and @(CompElement id opts)@ to @Just@ the corresponding natural transformation from 'SDNamespace'.
list_ce_to_nt :: [CompElement] -> Int -> ExceptT NatTransReadError (State SDNamespace) [Maybe NaturalTransformation]
list_ce_to_nt list m 
    = do found_list <- lift $ mapM (runExceptT.ce_to_nt) list
         let lookup_errs = filter (isLeft.snd) $ zip [0..] found_list
         if null lookup_errs
         then return $ map (\(Right r) -> r) found_list
         else throwError $ LookupNatTransError m $ map (\(n,Left eid)->(n,eid)) lookup_errs

-- | 'ce_to_nt' of a 'CompElement' gives @Nothing@ if the 'CompElement' is @Nothing@,
-- gives @Just@ the corresponding natural transformation from 'SDNamespace', or
-- throws the id of the 'CompElement' as an error if it could not be found in the 'SDNamespace'.
ce_to_nt :: CompElement -> ExceptT String (State SDNamespace) (Maybe NaturalTransformation)
ce_to_nt Empty = return Nothing
ce_to_nt (CompElement cid opts) = Just <$> (maybeToExceptT cid $ sdns_chain_lookup_nat cid opts)

-- | 'impute_missing_nat' takes the current line number, and a list of @Maybe NaturalTransformation@ and 
-- a functor @f@ which is putatively the source of the horizontal composition of the
-- 'NaturalTransformation's in the list.
-- It replaces the @Nothing@s in the list of @Maybe NaturalTransformations@
-- by the identity natural transformation of the corresponding basic functor in the correct position in @f@,
-- throwing an 'ImputationError' if this cannot be done.
impute_missing_nat :: Int -> [Maybe NaturalTransformation] -> Functor -> Except NatTransReadError [NaturalTransformation]
impute_missing_nat line nats func = impute' nats (func_to_single_list func) 0
    where
        impute' :: [Maybe NaturalTransformation] -> [Functor] -> Int -> Except NatTransReadError [NaturalTransformation]
        impute' [] _ _ = return []
        impute' (Nothing:_) [] n 
            = throwError $ ImputationError line n
        impute' (Nothing:ns) (f:fs) n 
            = impute' ns fs (n+1) >>= (\x -> return ((identityNaturalTransformation f):x))
        impute' ((Just nat):ns) fs n
            = impute' ns (drop (nat_source_length nat) fs) (n+1) >>= (\x -> return (nat:x))
 
-- | 'horz_compose_nats' takes the current line number and a list of pairs @(id,nt)@ where @id@ is
-- the id of the natural transformation @nt@ and attempts to take a horizontal composition of the
-- natural transformations.
-- It throws a 'HorzComposeNatTransError' if the natural transformations are not horizontally
-- composable.
horz_compose_nats :: Int -> [(String,NaturalTransformation)] -> Except NatTransReadError NaturalTransformation
horz_compose_nats line list = withExcept mExcept $ nat_horz_compose_with_error $ map snd list
    where
        mExcept (NatHorzCompositionError errs) 
            = let lefts = map (\x -> list !! x) errs
                  rights = map (\x -> list !! (x+1)) errs
              in HorzComposeNatTransError line $ zipWith3 
                  (\(x,_)-> (\(a,_)-> (\n -> (n,x,n+1,a)))) lefts rights errs

-- | 'get_first_fff' takes a list of 'SDDrawLine's and returns the source
-- of the functor they represent along with a 'FunctorFormatting' which is used
-- to format this source functor.
--
-- It throws a 'NoLinesError' if there are no lines in the list, and
-- a 'FirstLineImputationError' if the first 'SDDrawLine' is specifying a line for a natural
-- transformation, and @Empty@ is in the list of 'CompElement's.
-- It also throws errors if the first line cannot be read.
-- (i.e. if the first line is a functor line, then it can throw an 'FRE' error.
-- If the first line is a natural transformation line, then it can throw a 
-- 'LookupNatTransError' or a 'HorzComposeNatTransError').
get_first_fff :: [SDDrawLine] -> ExceptT NatTransReadError (State SDNamespace) (Functor, FunctorFormatting)
get_first_fff [] = throwError NoLinesError
get_first_fff ((SDDrawFun ces):_) = withExceptT (FRE 0) $ read_functor_line ces
get_first_fff ((SDDrawNat ces):_) 
    = do elems <- list_ce_to_nt ces 0
         let ids = map ce_id ces
         let (bads,goods) = partition isNothing elems
         if null $ bads  
         then 
            do first_nat <- mapExceptT (return.runIdentity) $ horz_compose_nats 0 
                                $ zip ids $ map fromJust goods
               let f = nat_source first_nat
               return $ (f, default_ff f)
         else throwError FirstLineImputationError

-- | 'combine_sddl' is a helper function for 'read_nat_trans'.
-- It takes
-- 
-- - The current target functor of the previous lines
--
-- - A @Bool@ which is true if the previous line was a functor line, and false if it was a natural
-- transformation line
--
-- - The current line number
--
-- - The current 'SDDrawLine' for this line
--
-- and it outputs a tuple of
--
-- - The target functor of the new natural transformation
--
-- - A @Bool@ which is true if this line was a functor line and false otherwise
--
-- - The next line number
--
-- - The empty list if the current line was a functor line.
-- A singleton list containing the horizontal composition of the current line
-- for a natural transformation line.
--
-- - The empty list if the current line was a natural transformation line.
-- A singleton list corresponding to the functor formatting of the current line
-- if it is a functor line.
--
-- It throws a 'NatTransReadError' if there was an error in processing this line.
combine_sddl :: Functor -> Bool -> Int -> SDDrawLine -> ExceptT NatTransReadError 
                    (State SDNamespace) (Functor,Bool,Int,[NaturalTransformation],[FunctorFormatting])
combine_sddl _ True n (SDDrawFun _) = throwError $ TwoConsecutiveFunctorsError n
combine_sddl fun False n (SDDrawFun ces) 
    = do (f,ff) <- withExceptT (FRE n) $ read_functor_line ces
         if f == fun 
         then return (fun, True, n+1,[], [ff])
         else throwError $ IncompatibleLinesError n
combine_sddl fun tf n (SDDrawNat ces) 
    = do elems <- list_ce_to_nt ces n
         nats <- mapExceptT (return.runIdentity) $ impute_missing_nat n elems fun
         c_nat <- mapExceptT (return.runIdentity) $ 
                      horz_compose_nats n $ zip (map get_id nats) nats
         case nat_source c_nat == fun of 
              True -> if tf 
                      then return (nat_target c_nat, False,n+1, [c_nat], [])
                      else return (nat_target c_nat, False, n+1,[c_nat], [default_ff $ nat_source c_nat])
              False -> throwError $ IncompatibleLinesError n

-- | 'read_nat_trans' takes a list of 'SDDrawLine's and returns a pair @(nt,nf)@
-- where @nt@ is the 'NaturalTransformation' specified by this list of 'SDDrawLine's,
-- and @nf@ is the 'NatFormatting' specified by the list, used to format @nt@.
--
-- It throws a 'NatTransReadError' if there was an error in processing the list.
read_nat_trans :: [SDDrawLine] -> ExceptT NatTransReadError (State SDNamespace) (NaturalTransformation,NatFormatting)
read_nat_trans sdls 
    = do (f,_) <- get_first_fff sdls
         (fun, tf, _, nats, ffs) <- foldM helper (f,False,0,[],[]) sdls
         mapExceptT (return.runIdentity) $ check_nonempty nats
         let final_ffs = add_final_ff fun tf ffs
         return (fromJust $ nat_vert_compose nats, final_ffs)
    where
        helper :: (Functor,Bool,Int,[NaturalTransformation],[FunctorFormatting]) -> SDDrawLine 
                    -> ExceptT NatTransReadError (State SDNamespace) 
                                (Functor,Bool,Int,[NaturalTransformation],[FunctorFormatting])
        helper (f,b,i,nts,ffs) sddl = do (next_f,next_b, next_i, new_nts, new_ffs) <- combine_sddl f b i sddl
                                         return (next_f,next_b,next_i,nts++new_nts,ffs++new_ffs)
        check_nonempty :: [NaturalTransformation] -> Except NatTransReadError ()
        check_nonempty [] = throwError NoLinesError
        check_nonempty _ = return ()
        add_final_ff :: Functor -> Bool -> [FunctorFormatting] -> [FunctorFormatting]
        add_final_ff fun False ffs = ffs++[default_ff fun]
        add_final_ff _ True ffs = ffs

-- | A partial function on 'SDCommand's which were constructed using 'DrawNat'.
-- See 'handle_sdc'.
handle_draw_nat :: SDCommand -> State SDNamespace (IO ())
handle_draw_nat (DrawNat fn opts ces) 
    = do e_or_nt_nf <- runExceptT $ read_nat_trans ces
         case e_or_nt_nf of 
              Left e -> return $ hPutStrLn stderr $ (error_msg e) 
                                     ++"\n\tWhen drawing the natural transformation "
                                     ++"with output file "++fn
              Right (nt,nf) -> do let tikzsd = make_tikzsd nt nf opts
                                  return $ writeFile fn (showLatex tikzsd)
handle_draw_nat _ 
    = error $ "Error! handle_draw_nat should only be called by handle_sdc,"
                ++ " which should only call handle_draw_nat when handling"
                ++ " DrawNat SDCommands"

-- | 'handle_sdc' takse an 'SDCommand' and does the corresponding action.
-- For define commands, it adds the corresponding object to the 'SDNamespace' state.
-- For 'DrawNat' commands, it writes the LaTeX code for the specified string diagram to the
-- specified file.
--
-- It prints an error message to @stderr@ describing the problem if the command could not be
-- executed.
--
-- This function is broken up into the partially defined functions 'handle_def_cat',
-- 'handle_def_fun', 'handle_def_nat' and 'handle_draw_nat'.
handle_sdc :: SDCommand -> State SDNamespace (IO ())
handle_sdc (DefineCat cid ds) 
    = handle_def_cat (DefineCat cid ds)
handle_sdc (DefineFunc fid ds source_id target_id opts) 
    = handle_def_fun (DefineFunc fid ds source_id target_id opts)
handle_sdc (DefineNat ntid ds source target opts shape) 
    = handle_def_nat (DefineNat ntid ds source target opts shape)
handle_sdc (DrawNat fn opts ces) 
    = handle_draw_nat (DrawNat fn opts ces)
