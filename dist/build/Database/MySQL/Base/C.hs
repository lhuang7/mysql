{-# LINE 1 "Database/MySQL/Base/C.hsc" #-}
{-# LANGUAGE CPP, EmptyDataDecls, ForeignFunctionInterface #-}
{-# LINE 2 "Database/MySQL/Base/C.hsc" #-}

-- |
-- Module:      Database.MySQL.Base.C
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Direct bindings to the C @mysqlclient@ API.
module Database.MySQL.Base.C
    (
    -- * Connection management
      mysql_init
    , mysql_options
    , mysql_ssl_set
    , mysql_real_connect
    , mysql_close
    , mysql_ping
    , mysql_autocommit
    , mysql_change_user
    , mysql_select_db
    , mysql_set_character_set
    -- ** Connection information
    , mysql_thread_id
    , mysql_get_server_info
    , mysql_get_host_info
    , mysql_get_proto_info
    , mysql_character_set_name
    , mysql_get_ssl_cipher
    , mysql_stat
    -- * Querying
    , mysql_real_query
    , mysql_insert_id
    -- ** Escaping
    , mysql_real_escape_string
    -- ** Results
    , mysql_field_count
    , mysql_affected_rows
    , mysql_store_result
    , mysql_use_result
    , mysql_fetch_lengths
    , mysql_fetch_lengths_nonblock
    , mysql_fetch_row
    , mysql_fetch_row_nonblock
    -- * Working with results
    , mysql_free_result
    , mysql_free_result_nonblock
    , mysql_fetch_fields
    , mysql_fetch_fields_nonblock
    , mysql_data_seek
    , mysql_row_seek
    , mysql_row_tell
    -- ** Multiple results
    , mysql_next_result
    -- * Transactions
    , mysql_commit
    , mysql_rollback
    -- * General information
    , mysql_get_client_info
    , mysql_get_client_version
    -- * Error handling
    , mysql_errno
    , mysql_error
    ) where


{-# LINE 69 "Database/MySQL/Base/C.hsc" #-}

{-# LINE 70 "Database/MySQL/Base/C.hsc" #-}

import Data.ByteString.Unsafe (unsafeUseAsCString)
import Database.MySQL.Base.Types
import Foreign.C.String (CString, withCString)
#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types (CChar(..), CInt(..), CUInt(..), CULLong(..), CULong(..))
#else
import Foreign.C.Types (CInt, CUInt, CULLong, CULong)
#endif
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)

foreign import ccall safe mysql_init
    :: Ptr MYSQL                -- ^ should usually be 'nullPtr'
    -> IO (Ptr MYSQL)

mysql_options :: Ptr MYSQL -> Option -> IO CInt
mysql_options ptr opt =
    case opt of
      ConnectTimeout secs ->
        withIntegral secs $ go (0)
{-# LINE 91 "Database/MySQL/Base/C.hsc" #-}
      Compress ->
        go (1) nullPtr
{-# LINE 93 "Database/MySQL/Base/C.hsc" #-}
      NamedPipe ->
        go (2) nullPtr
{-# LINE 95 "Database/MySQL/Base/C.hsc" #-}
      InitCommand cmd ->
        unsafeUseAsCString cmd $ go (3)
{-# LINE 97 "Database/MySQL/Base/C.hsc" #-}
      ReadDefaultFile path ->
        withCString path $ go (4)
{-# LINE 99 "Database/MySQL/Base/C.hsc" #-}
      ReadDefaultGroup group ->
        unsafeUseAsCString group $ go (5)
{-# LINE 101 "Database/MySQL/Base/C.hsc" #-}
      CharsetDir path ->
        withCString path $ go (6)
{-# LINE 103 "Database/MySQL/Base/C.hsc" #-}
      CharsetName cs ->
        withCString cs $ go (7)
{-# LINE 105 "Database/MySQL/Base/C.hsc" #-}
      LocalInFile b ->
        withBool b $ go (8)
{-# LINE 107 "Database/MySQL/Base/C.hsc" #-}
      Protocol proto ->
        withIntegral (fromEnum proto) $ go (9)
{-# LINE 109 "Database/MySQL/Base/C.hsc" #-}
      SharedMemoryBaseName name ->
        unsafeUseAsCString name $ go (10)
{-# LINE 111 "Database/MySQL/Base/C.hsc" #-}
      ReadTimeout secs ->
        withIntegral secs $ go (11)
{-# LINE 113 "Database/MySQL/Base/C.hsc" #-}
      WriteTimeout secs ->
        withIntegral secs $ go (12)
{-# LINE 115 "Database/MySQL/Base/C.hsc" #-}
      UseRemoteConnection ->
        go (14) nullPtr
{-# LINE 117 "Database/MySQL/Base/C.hsc" #-}
      UseEmbeddedConnection ->
        go (15) nullPtr
{-# LINE 119 "Database/MySQL/Base/C.hsc" #-}
      GuessConnection ->
        go (16) nullPtr
{-# LINE 121 "Database/MySQL/Base/C.hsc" #-}
      ClientIP ip ->
        unsafeUseAsCString ip $ go (17)
{-# LINE 123 "Database/MySQL/Base/C.hsc" #-}
      SecureAuth b ->
        withBool b $ go (18)
{-# LINE 125 "Database/MySQL/Base/C.hsc" #-}
      ReportDataTruncation b ->
        withBool b $ go (19)
{-# LINE 127 "Database/MySQL/Base/C.hsc" #-}
      Reconnect b ->
        withBool b $ go (20)
{-# LINE 129 "Database/MySQL/Base/C.hsc" #-}
      SSLVerifyServerCert b ->
        withBool b $ go (21)
{-# LINE 131 "Database/MySQL/Base/C.hsc" #-}
      -- Other options are accepted by mysql_real_connect, so ignore them.
      _ -> return 0
  where
    go = mysql_options_ ptr
    withBool b = with (if b then 1 else 0 :: CUInt)
    withIntegral i = with (fromIntegral i :: CUInt)

foreign import ccall safe "mysql.h mysql_options" mysql_options_
    :: Ptr MYSQL -> CInt -> Ptr a -> IO CInt

foreign import ccall unsafe "mysql_signals.h _hs_mysql_real_connect"
        mysql_real_connect
    :: Ptr MYSQL -- ^ Context (from 'mysql_init').
    -> CString   -- ^ Host name.
    -> CString   -- ^ User name.
    -> CString   -- ^ Password.
    -> CString   -- ^ Database.
    -> CInt      -- ^ Port.
    -> CString   -- ^ Unix socket.
    -> CULong    -- ^ Flags.
    -> IO (Ptr MYSQL)

foreign import ccall safe mysql_ssl_set
    :: Ptr MYSQL
    -> CString                  -- ^ Key.
    -> CString                  -- ^ Cert.
    -> CString                  -- ^ CA.
    -> CString                  -- ^ CA path.
    -> CString                  -- ^ Ciphers.
    -> IO MyBool

foreign import ccall unsafe "mysql_signals.h _hs_mysql_close" mysql_close
    :: Ptr MYSQL -> IO ()

foreign import ccall unsafe "mysql_signals.h _hs_mysql_ping" mysql_ping
    :: Ptr MYSQL -> IO CInt

foreign import ccall safe mysql_thread_id
    :: Ptr MYSQL -> IO CULong

foreign import ccall unsafe "mysql_signals.h _hs_mysql_autocommit" mysql_autocommit
    :: Ptr MYSQL -> MyBool -> IO MyBool

foreign import ccall unsafe "mysql_signals.h _hs_mysql_change_user" mysql_change_user
    :: Ptr MYSQL
    -> CString                  -- ^ user
    -> CString                  -- ^ password
    -> CString                  -- ^ database
    -> IO MyBool

foreign import ccall unsafe "mysql_signals.h _hs_mysql_select_db" mysql_select_db
    :: Ptr MYSQL
    -> CString
    -> IO CInt

foreign import ccall safe mysql_get_server_info
    :: Ptr MYSQL -> IO CString

foreign import ccall safe mysql_get_host_info
    :: Ptr MYSQL -> IO CString

foreign import ccall safe mysql_get_proto_info
    :: Ptr MYSQL -> IO CUInt

foreign import ccall safe mysql_character_set_name
    :: Ptr MYSQL -> IO CString

foreign import ccall safe mysql_set_character_set
    :: Ptr MYSQL -> CString -> IO CInt

foreign import ccall safe mysql_get_ssl_cipher
    :: Ptr MYSQL -> IO CString

foreign import ccall unsafe "mysql_signals.h _hs_mysql_stat" mysql_stat
    :: Ptr MYSQL -> IO CString

foreign import ccall unsafe "mysql_signals.h _hs_mysql_real_query" mysql_real_query
    :: Ptr MYSQL -> CString -> CULong -> IO CInt

foreign import ccall safe mysql_insert_id
    :: Ptr MYSQL -> IO CULLong

foreign import ccall safe mysql_field_count
    :: Ptr MYSQL -> IO CUInt

foreign import ccall safe mysql_affected_rows
    :: Ptr MYSQL -> IO CULLong

foreign import ccall unsafe "mysql_signals.h _hs_mysql_store_result" mysql_store_result
    :: Ptr MYSQL -> IO (Ptr MYSQL_RES)

foreign import ccall unsafe "mysql_signals.h _hs_mysql_use_result"  mysql_use_result
    :: Ptr MYSQL -> IO (Ptr MYSQL_RES)

foreign import ccall unsafe "mysql_signals.h _hs_mysql_free_result" mysql_free_result
    :: Ptr MYSQL_RES -> IO ()

foreign import ccall safe "mysql.h mysql_free_result" mysql_free_result_nonblock
    :: Ptr MYSQL_RES -> IO ()

foreign import ccall safe mysql_fetch_fields
    :: Ptr MYSQL_RES -> IO (Ptr Field)

foreign import ccall safe "mysql.h mysql_fetch_fields" mysql_fetch_fields_nonblock
    :: Ptr MYSQL_RES -> IO (Ptr Field)

foreign import ccall safe mysql_data_seek
    :: Ptr MYSQL_RES -> CULLong -> IO ()

foreign import ccall safe mysql_row_seek
    :: Ptr MYSQL_RES -> MYSQL_ROW_OFFSET -> IO MYSQL_ROW_OFFSET

foreign import ccall safe mysql_row_tell
    :: Ptr MYSQL_RES -> IO MYSQL_ROW_OFFSET

foreign import ccall unsafe "mysql_signals.h _hs_mysql_next_result" mysql_next_result
    :: Ptr MYSQL -> IO CInt

foreign import ccall unsafe "mysql_signals.h _hs_mysql_commit" mysql_commit
    :: Ptr MYSQL -> IO MyBool

foreign import ccall unsafe "mysql_signals.h _hs_mysql_rollback" mysql_rollback
    :: Ptr MYSQL -> IO MyBool

foreign import ccall unsafe "mysql_signals.h _hs_mysql_fetch_row" mysql_fetch_row
    :: Ptr MYSQL_RES -> IO MYSQL_ROW

foreign import ccall safe "mysql.h mysql_fetch_row" mysql_fetch_row_nonblock
    :: Ptr MYSQL_RES -> IO MYSQL_ROW

foreign import ccall safe mysql_fetch_lengths
    :: Ptr MYSQL_RES -> IO (Ptr CULong)

foreign import ccall safe "mysql.h mysql_fetch_lengths" mysql_fetch_lengths_nonblock
    :: Ptr MYSQL_RES -> IO (Ptr CULong)

foreign import ccall safe mysql_real_escape_string
    :: Ptr MYSQL -> CString -> CString -> CULong -> IO CULong

foreign import ccall safe mysql_get_client_info :: CString

foreign import ccall safe mysql_get_client_version :: CULong

foreign import ccall safe mysql_errno
    :: Ptr MYSQL -> IO CInt

foreign import ccall safe mysql_error
    :: Ptr MYSQL -> IO CString
