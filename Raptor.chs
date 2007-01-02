
module Raptor where

import Foreign
import Foreign.C

import System.Posix.IO (stdOutput)
import System.Posix.Types (Fd)
import System (getArgs)

data Parser
-- data Statement
-- data URI



-- stop c2hs chocking on __attribute__(deprecated):
#define RAPTOR_DEPRECATED
#define __APPLE_CC__


#include <raptor.h>


{#context lib="raptor" prefix="raptor"#}

{#enum raptor_identifier_type as IdType {} deriving (Show)#}

{#enum raptor_uri_source as UriSource {} deriving (Show)#}

{#pointer raptor_uri as URI newtype#}

{#pointer *raptor_statement as StatementPtr -> Statement#}

data Statement = Statement { subject :: Ptr (),
                             subject_type :: IdType,
                             predicate :: Ptr (),
                             predicate_type :: IdType,
                             object :: Ptr (),
                             object_type :: IdType,
                             object_literal_datatype :: Ptr URI,
                             object_literal_language :: CString
                           } deriving (Show)

instance Storable Statement where
    sizeOf _ = {#sizeof raptor_statement#}
    alignment _ = alignment (undefined :: CDouble)
    peek p = do
      subject <- {#get raptor_statement->subject#} p
      subject_type <- {#get raptor_statement->subject_type#} p >>= return . toEnum . fromIntegral
      predicate <- {#get raptor_statement->predicate#} p
      predicate_type <- {#get raptor_statement->predicate_type#} p >>= return . toEnum . fromIntegral
      object <- {#get raptor_statement->object#} p
      object_type <- {#get raptor_statement->object_type#} p >>= return . toEnum . fromIntegral
      object_literal_datatype <- {#get raptor_statement->object_literal_datatype#} p
      object_literal_language <- {#get raptor_statement->object_literal_language#} p >>= return . castPtr
      return $ Statement subject subject_type predicate predicate_type object object_type object_literal_datatype object_literal_language
    poke p (Statement subject subject_type predicate predicate_type object object_type object_literal_datatype object_literal_language) = undefined

type Handler a = Ptr a -> Ptr Statement -> IO ()
foreign import ccall "wrapper"
   mkHandler :: (Handler a) -> IO (FunPtr (Handler a))

foreign import ccall "raptor.h raptor_init" initRaptor :: IO ()
foreign import ccall "raptor.h raptor_new_parser" new_parser :: Ptr CChar -> IO (Ptr Parser)
foreign import ccall "raptor.h raptor_set_statement_handler" set_statement_handler :: Ptr Parser -> Ptr a -> FunPtr (Handler a) -> IO () 
foreign import ccall "raptor.h raptor_uri_filename_to_uri_string" uri_filename_to_uri_string :: CString -> IO CString
foreign import ccall "raptor.h raptor_new_uri" new_uri :: Ptr CChar -> IO (Ptr URI)
foreign import ccall "raptor.h raptor_uri_copy" uri_copy :: Ptr URI -> IO (Ptr URI)
foreign import ccall "raptor.h raptor_parse_file" parse_file :: Ptr Parser -> Ptr URI -> Ptr URI -> IO ()

foreign import ccall "raptor.h raptor_print_statement_as_ntriples" print_statement_as_ntriples :: Ptr Statement -> Ptr CFile -> IO ()

foreign import ccall "stdio.h fdopen" fdopen :: Fd -> CString -> IO (Ptr CFile)
foreign import ccall "stdio.h fputc" fputc :: CChar -> Ptr CFile -> IO ()

print_triple :: Ptr CFile -> Handler a
print_triple outfile _user_data s = do print_statement_as_ntriples s outfile
                                       fputc (castCharToCChar '\n') outfile
                                       peek s >>= print
main = do
  [filename] <- getArgs
  outfile <- withCString "w" $ fdopen stdOutput

  initRaptor
  rdf_parser <- withCString "guess" new_parser 
  mkHandler (print_triple outfile) >>= set_statement_handler rdf_parser nullPtr
  uri <- withCString filename uri_filename_to_uri_string >>= new_uri
  base_uri <- uri_copy uri
  parse_file rdf_parser uri base_uri
  return ()
