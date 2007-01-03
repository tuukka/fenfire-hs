
module Raptor where

import Foreign
import Foreign.C

import System.Posix.IO (stdOutput)
import System.Posix.Types (Fd)
import System (getArgs)

import Data.IORef

-- stop c2hs choking on __attribute__(deprecated):
#ifndef RAPTOR_DEPRECATED
#define RAPTOR_DEPRECATED
#define __APPLE_CC__
#endif

#include <raptor.h>


cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . cIntConv

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv  = fromIntegral


{#context lib="raptor" prefix="raptor"#}

{#enum raptor_identifier_type as IdType {} deriving (Show)#}

{#enum raptor_uri_source as UriSource {} deriving (Show)#}

{#pointer raptor_uri as URI newtype#}

{#pointer *statement as Statement newtype#}

{#pointer *parser as Parser newtype#}

type Triple = (Identifier, Identifier, Identifier)

data Identifier = Uri String | Blank String | Literal String
                  deriving (Show)

mkIdentifier value format = do
  value' <- value
  format' <- format
  f (castPtr value') (cToEnum format')
    where f v IDENTIFIER_TYPE_RESOURCE = do
                              cstr <- {#call uri_as_string#} (castPtr v) 
                              str <- peekCString (castPtr cstr) 
                              return $ Uri str
          f v IDENTIFIER_TYPE_LITERAL = peekCString v >>= return . Literal
          f v IDENTIFIER_TYPE_ANONYMOUS = peekCString v >>= return . Blank
          f _ _ = error "Raptor.mkIdentifier: deprecated identifier type"

getSubject s = mkIdentifier ({#get statement->subject#} s)
                            ({#get statement->subject_type#} s)

getPredicate s = mkIdentifier ({#get statement->predicate#} s)
                              ({#get statement->predicate_type#} s)

getObject s = mkIdentifier ({#get statement->object#} s)
                           ({#get statement->object_type#} s)

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

collect_triple :: IORef [Triple] -> Handler a
collect_triple result _user_data t = do
                                       s <- getSubject t
                                       p <- getPredicate t
                                       o <- getObject t
                                       modifyIORef result ((s,p,o):)

filenameToTriples filename = do 
  result <- newIORef []

  initRaptor
  rdf_parser <- withCString "guess" new_parser 
  handler <- mkHandler (collect_triple result)
  set_statement_handler rdf_parser nullPtr handler
  uri <- withCString filename uri_filename_to_uri_string >>= new_uri
  base_uri <- uri_copy uri
  parse_file rdf_parser uri base_uri

  readIORef result

filenameToStdout filename = do
  outfile <- withCString "w" $ fdopen stdOutput

  initRaptor
  rdf_parser <- withCString "guess" new_parser 
  mkHandler (print_triple outfile) >>= set_statement_handler rdf_parser nullPtr
  uri <- withCString filename uri_filename_to_uri_string >>= new_uri
  base_uri <- uri_copy uri
  parse_file rdf_parser uri base_uri
  return ()
