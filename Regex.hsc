--
--
-- Module      :  Text.Regex.Posix
--
-- Copyright   :  (c) The University of Glasgow 2002
--                (c) Don Stewart 2004
--
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--

--  
-- | Interface to the POSIX regular expression library.
--

module Regex (

    -- * The @Regex@ type
    Regex,      -- abstract

    -- * Compiling a regular expression
    regcomp,    -- :: FastString -> Int -> IO Regex

    -- ** Flags for regcomp
    regExtended,    -- (flag to regcomp) use extended regex syntax
    regIgnoreCase,  -- (flag to regcomp) ignore case when matching
    regNewline,     -- (flag to regcomp) '.' doesn't match newline
    regNosub,       -- unused

    -- * Matching a regular expression
    regexec,        -- :: Regex -> Ptr CChar -> Int
                    -- -> IO (Maybe ((Int,Int), [(Int,Int)]))
    matchRegex

  ) where

# include <sys/types.h>
# include <regex.h>

import Foreign.C            (CSize, CInt, CChar, CString)
import Foreign.C.String     (withCString)
import Foreign.ForeignPtr   (withForeignPtr, ForeignPtr, 
                             addForeignPtrFinalizer, mallocForeignPtrBytes)
import Foreign              (Int,Int64,Ptr,FunPtr
                            ,Storable(peekByteOff),allocaBytes,plusPtr)
import System.IO.Unsafe     (unsafePerformIO)

import qualified Data.FastPackedString as P

type CRegex = ()

-- | A compiled regular expression
newtype Regex = Regex (ForeignPtr CRegex)

-- ---------------------------------------------------------------------
-- | Compiles a regular expression
--
regcomp :: P.FastString    -- ^ The regular expression to compile
        -> Int        -- ^ Flags (summed together)
        -> IO Regex   -- ^ Returns: the compiled regular expression

regcomp ps flags = do
    regex_fptr <- mallocForeignPtrBytes (#const sizeof(regex_t))
    r <- P.unsafeUseAsCString ps $ \cstr ->
            withForeignPtr regex_fptr $ \p ->
                c_regcomp p cstr (fromIntegral flags)
    if (r == 0)
        then do
             addForeignPtrFinalizer ptr_regfree regex_fptr
             return (Regex regex_fptr)
        else ioError $ userError $ "Error in pattern: " ++ (show ps)

-- ---------------------------------------------------------------------
-- | Matches a regular expression against a buffer, returning the buffer
-- indicies of the match, and any submatches
--
regexec :: Regex                -- ^ Compiled regular expression
        -> Ptr CChar            -- ^ The buffer to match against
        -> Int                  -- ^ Offset in buffer to start searching from
        -> IO (Maybe ((Int,Int), [(Int,Int)]))
        -- ^ Returns: 'Nothing' if the regex did not match the
        -- or Just the start and end indicies of the match and submatches

regexec (Regex regex_fptr) ptr i = do
    withForeignPtr regex_fptr $ \regex_ptr -> do
        nsub <- (#peek regex_t, re_nsub) regex_ptr
        let nsub_int = fromIntegral (nsub :: CSize)
        allocaBytes ((1 + nsub_int)*(#const sizeof(regmatch_t))) $ \p_match-> do
            -- add one because index zero covers the whole match
            r <- cregexec regex_ptr (ptr `plusPtr` i) 
                                    (1 + nsub) p_match 0{-no flags -}

            if (r /= 0) then return Nothing else do 
                match       <- indexOfMatch p_match
                sub_matches <- mapM (indexOfMatch) $ take nsub_int $ tail $
                        iterate (`plusPtr` (#const sizeof(regmatch_t))) p_match

                return (Just (match, sub_matches))

indexOfMatch :: Ptr CRegMatch -> IO (Int, Int)
indexOfMatch p_match = do
    start <- (#peek regmatch_t, rm_so) p_match :: IO (#type regoff_t)
    end   <- (#peek regmatch_t, rm_eo) p_match :: IO (#type regoff_t)
    let s = fromIntegral start; e = fromIntegral end
    return (s, e)

-- -----------------------------------------------------------------------------
-- The POSIX regex C interface

regExtended :: Int
regExtended =  1
regIgnoreCase  :: Int
regIgnoreCase  =  2
regNosub :: Int
regNosub =  4
regNewline :: Int
regNewline =  8

type CRegMatch = ()

foreign import ccall unsafe "regex.h regcomp"
    c_regcomp :: Ptr CRegex -> CString -> CInt -> IO CInt

foreign import ccall  unsafe "regex.h &regfree"
    ptr_regfree :: FunPtr (Ptr CRegex -> IO ())

foreign import ccall unsafe "regex.h regexec"
    cregexec :: Ptr CRegex 
             -> Ptr CChar 
             -> CSize -> Ptr CRegMatch -> CInt -> IO CInt

------------------------------------------------------------------------

-- | Match a regular expression against a string
matchRegex
   :: Regex	-- ^ The regular expression
   -> String	-- ^ The string to match against
   -> Bool
matchRegex p str = unsafePerformIO $ withCString str $ \cstr -> do
    m <- regexec p cstr 0
    return $ case m of
        Nothing -> False
        Just  _ -> True
