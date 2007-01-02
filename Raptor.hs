
module Raptor where

import Foreign
import Foreign.C

import System.Posix.IO (stdOutput)
import System.Posix.Types (Fd)
import System (getArgs)

data Parser
data Statement
data URI

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

main = do
  [filename] <- getArgs
  outfile <- withCString "w" $ fdopen stdOutput

  initRaptor
  rdf_parser <- withCString "rdfxml" new_parser 
  mkHandler (print_triple outfile) >>= set_statement_handler rdf_parser nullPtr
  uri <- withCString filename uri_filename_to_uri_string >>= new_uri
  base_uri <- uri_copy uri
  parse_file rdf_parser uri base_uri
  return ()
