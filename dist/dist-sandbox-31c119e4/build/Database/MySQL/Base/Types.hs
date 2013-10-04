{-# LINE 1 "Database/MySQL/Base/Types.hsc" #-}
{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls, ForeignFunctionInterface #-}
{-# LINE 2 "Database/MySQL/Base/Types.hsc" #-}

-- |
-- Module:      Database.MySQL.Base.C
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with the direct bindings to the C @mysqlclient@
-- API.

module Database.MySQL.Base.Types
    (
    -- * Types
    -- * High-level types
      Type(..)
    , Seconds
    , Protocol(..)
    , Option(..)
    , Field(..)
    , FieldFlag
    , FieldFlags
    -- * Low-level types
    , MYSQL
    , MYSQL_RES
    , MYSQL_ROW
    , MYSQL_ROWS
    , MYSQL_ROW_OFFSET
    , MyBool
    -- * Field flags
    , hasAllFlags
    , flagNotNull
    , flagPrimaryKey
    , flagUniqueKey
    , flagMultipleKey
    , flagUnsigned
    , flagZeroFill
    , flagBinary
    , flagAutoIncrement
    , flagNumeric
    , flagNoDefaultValue
    -- * Connect flags
    , toConnectFlag
    ) where


{-# LINE 49 "Database/MySQL/Base/Types.hsc" #-}

import Control.Applicative ((<$>), (<*>), pure)
import Data.Bits ((.|.), (.&.))
import Data.ByteString hiding (intercalate)
import Data.ByteString.Internal (create, memcpy)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)
import Data.Word (Word, Word8)
import Foreign.C.Types (CChar, CInt, CUInt, CULong)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..), peekByteOff)
import qualified Data.IntMap as IntMap

data MYSQL
data MYSQL_RES
data MYSQL_ROWS
type MYSQL_ROW = Ptr (Ptr CChar)
type MYSQL_ROW_OFFSET = Ptr MYSQL_ROWS
type MyBool = CChar

-- | Column types supported by MySQL.
data Type = Decimal
          | Tiny
          | Short
          | Long
          | Float
          | Double
          | Null
          | Timestamp
          | LongLong
          | Int24
          | Date
          | Time
          | DateTime
          | Year
          | NewDate
          | VarChar
          | Bit
          | NewDecimal
          | Enum
          | Set
          | TinyBlob
          | MediumBlob
          | LongBlob
          | Blob
          | VarString
          | String
          | Geometry
            deriving (Enum, Eq, Show, Typeable)

toType :: CInt -> Type
toType v = IntMap.findWithDefault oops (fromIntegral v) typeMap
  where
    oops = error $ "Database.MySQL: unknown field type " ++ show v
    typeMap = IntMap.fromList [
               ((0), Decimal),
{-# LINE 107 "Database/MySQL/Base/Types.hsc" #-}
               ((1), Tiny),
{-# LINE 108 "Database/MySQL/Base/Types.hsc" #-}
               ((2), Short),
{-# LINE 109 "Database/MySQL/Base/Types.hsc" #-}
               ((3), Long),
{-# LINE 110 "Database/MySQL/Base/Types.hsc" #-}
               ((4), Float),
{-# LINE 111 "Database/MySQL/Base/Types.hsc" #-}
               ((5), Double),
{-# LINE 112 "Database/MySQL/Base/Types.hsc" #-}
               ((6), Null),
{-# LINE 113 "Database/MySQL/Base/Types.hsc" #-}
               ((7), Timestamp),
{-# LINE 114 "Database/MySQL/Base/Types.hsc" #-}
               ((8), LongLong),
{-# LINE 115 "Database/MySQL/Base/Types.hsc" #-}
               ((10), Date),
{-# LINE 116 "Database/MySQL/Base/Types.hsc" #-}
               ((11), Time),
{-# LINE 117 "Database/MySQL/Base/Types.hsc" #-}
               ((12), DateTime),
{-# LINE 118 "Database/MySQL/Base/Types.hsc" #-}
               ((13), Year),
{-# LINE 119 "Database/MySQL/Base/Types.hsc" #-}
               ((14), NewDate),
{-# LINE 120 "Database/MySQL/Base/Types.hsc" #-}
               ((15), VarChar),
{-# LINE 121 "Database/MySQL/Base/Types.hsc" #-}
               ((16), Bit),
{-# LINE 122 "Database/MySQL/Base/Types.hsc" #-}
               ((246), NewDecimal),
{-# LINE 123 "Database/MySQL/Base/Types.hsc" #-}
               ((247), Enum),
{-# LINE 124 "Database/MySQL/Base/Types.hsc" #-}
               ((248), Set),
{-# LINE 125 "Database/MySQL/Base/Types.hsc" #-}
               ((249), TinyBlob),
{-# LINE 126 "Database/MySQL/Base/Types.hsc" #-}
               ((250), MediumBlob),
{-# LINE 127 "Database/MySQL/Base/Types.hsc" #-}
               ((251), LongBlob),
{-# LINE 128 "Database/MySQL/Base/Types.hsc" #-}
               ((252), Blob),
{-# LINE 129 "Database/MySQL/Base/Types.hsc" #-}
               ((253), VarString),
{-# LINE 130 "Database/MySQL/Base/Types.hsc" #-}
               ((254), String),
{-# LINE 131 "Database/MySQL/Base/Types.hsc" #-}
               ((255), Geometry)
{-# LINE 132 "Database/MySQL/Base/Types.hsc" #-}
              ]

-- | A description of a field (column) of a table.
data Field = Field {
      fieldName :: ByteString   -- ^ Name of column.
    , fieldOrigName :: ByteString -- ^ Original column name, if an alias.
    , fieldTable :: ByteString -- ^ Table of column, if column was a field.
    , fieldOrigTable :: ByteString -- ^ Original table name, if table was an alias.
    , fieldDB :: ByteString        -- ^ Database for table.
    , fieldCatalog :: ByteString   -- ^ Catalog for table.
    , fieldDefault :: Maybe ByteString   -- ^ Default value.
    , fieldLength :: Word          -- ^ Width of column (create length).
    , fieldMaxLength :: Word    -- ^ Maximum width for selected set.
    , fieldFlags :: FieldFlags        -- ^ Div flags.
    , fieldDecimals :: Word -- ^ Number of decimals in field.
    , fieldCharSet :: Word -- ^ Character set number.
    , fieldType :: Type
    } deriving (Eq, Show, Typeable)

newtype FieldFlags = FieldFlags CUInt
    deriving (Eq, Typeable)

instance Show FieldFlags where
    show f = '[' : z ++ "]"
      where z = intercalate "," . catMaybes $ [
                          flagNotNull ??? "flagNotNull"
                        , flagPrimaryKey ??? "flagPrimaryKey"
                        , flagUniqueKey ??? "flagUniqueKey"
                        , flagMultipleKey ??? "flagMultipleKey"
                        , flagUnsigned ??? "flagUnsigned"
                        , flagZeroFill ??? "flagZeroFill"
                        , flagBinary ??? "flagBinary"
                        , flagAutoIncrement ??? "flagAutoIncrement"
                        , flagNumeric ??? "flagNumeric"
                        , flagNoDefaultValue ??? "flagNoDefaultValue"
                        ]
            flag ??? name | f `hasAllFlags` flag = Just name
                          | otherwise            = Nothing

type FieldFlag = FieldFlags

instance Monoid FieldFlags where
    mempty = FieldFlags 0
    {-# INLINE mempty #-}
    mappend (FieldFlags a) (FieldFlags b) = FieldFlags (a .|. b)
    {-# INLINE mappend #-}

flagNotNull, flagPrimaryKey, flagUniqueKey, flagMultipleKey :: FieldFlag
flagNotNull = FieldFlags 1
{-# LINE 181 "Database/MySQL/Base/Types.hsc" #-}
flagPrimaryKey = FieldFlags 2
{-# LINE 182 "Database/MySQL/Base/Types.hsc" #-}
flagUniqueKey = FieldFlags 4
{-# LINE 183 "Database/MySQL/Base/Types.hsc" #-}
flagMultipleKey = FieldFlags 8
{-# LINE 184 "Database/MySQL/Base/Types.hsc" #-}

flagUnsigned, flagZeroFill, flagBinary, flagAutoIncrement :: FieldFlag
flagUnsigned = FieldFlags 32
{-# LINE 187 "Database/MySQL/Base/Types.hsc" #-}
flagZeroFill = FieldFlags 64
{-# LINE 188 "Database/MySQL/Base/Types.hsc" #-}
flagBinary = FieldFlags 128
{-# LINE 189 "Database/MySQL/Base/Types.hsc" #-}
flagAutoIncrement = FieldFlags 512
{-# LINE 190 "Database/MySQL/Base/Types.hsc" #-}

flagNumeric, flagNoDefaultValue :: FieldFlag
flagNumeric = FieldFlags 32768
{-# LINE 193 "Database/MySQL/Base/Types.hsc" #-}
flagNoDefaultValue = FieldFlags 4096
{-# LINE 194 "Database/MySQL/Base/Types.hsc" #-}

hasAllFlags :: FieldFlags -> FieldFlags -> Bool
FieldFlags a `hasAllFlags` FieldFlags b = a .&. b == b
{-# INLINE hasAllFlags #-}

peekField :: Ptr Field -> IO Field
peekField ptr = do
  flags <- FieldFlags <$> ((\hsc_ptr -> peekByteOff hsc_ptr 100)) ptr
{-# LINE 202 "Database/MySQL/Base/Types.hsc" #-}
  Field
   <$> peekS (((\hsc_ptr -> peekByteOff hsc_ptr 0))) (((\hsc_ptr -> peekByteOff hsc_ptr 72)))
{-# LINE 204 "Database/MySQL/Base/Types.hsc" #-}
   <*> peekS (((\hsc_ptr -> peekByteOff hsc_ptr 8))) (((\hsc_ptr -> peekByteOff hsc_ptr 76)))
{-# LINE 205 "Database/MySQL/Base/Types.hsc" #-}
   <*> peekS (((\hsc_ptr -> peekByteOff hsc_ptr 16))) (((\hsc_ptr -> peekByteOff hsc_ptr 80)))
{-# LINE 206 "Database/MySQL/Base/Types.hsc" #-}
   <*> peekS (((\hsc_ptr -> peekByteOff hsc_ptr 24))) (((\hsc_ptr -> peekByteOff hsc_ptr 84)))
{-# LINE 207 "Database/MySQL/Base/Types.hsc" #-}
   <*> peekS (((\hsc_ptr -> peekByteOff hsc_ptr 32))) (((\hsc_ptr -> peekByteOff hsc_ptr 88)))
{-# LINE 208 "Database/MySQL/Base/Types.hsc" #-}
   <*> peekS (((\hsc_ptr -> peekByteOff hsc_ptr 40))) (((\hsc_ptr -> peekByteOff hsc_ptr 92)))
{-# LINE 209 "Database/MySQL/Base/Types.hsc" #-}
   <*> (if flags `hasAllFlags` flagNoDefaultValue
       then pure Nothing
       else Just <$> peekS (((\hsc_ptr -> peekByteOff hsc_ptr 48))) (((\hsc_ptr -> peekByteOff hsc_ptr 96))))
{-# LINE 212 "Database/MySQL/Base/Types.hsc" #-}
   <*> (uint <$> ((\hsc_ptr -> peekByteOff hsc_ptr 56)) ptr)
{-# LINE 213 "Database/MySQL/Base/Types.hsc" #-}
   <*> (uint <$> ((\hsc_ptr -> peekByteOff hsc_ptr 64)) ptr)
{-# LINE 214 "Database/MySQL/Base/Types.hsc" #-}
   <*> pure flags
   <*> (uint <$> ((\hsc_ptr -> peekByteOff hsc_ptr 104)) ptr)
{-# LINE 216 "Database/MySQL/Base/Types.hsc" #-}
   <*> (uint <$> ((\hsc_ptr -> peekByteOff hsc_ptr 108)) ptr)
{-# LINE 217 "Database/MySQL/Base/Types.hsc" #-}
   <*> (toType <$> ((\hsc_ptr -> peekByteOff hsc_ptr 112)) ptr)
{-# LINE 218 "Database/MySQL/Base/Types.hsc" #-}
 where
   uint = fromIntegral :: CUInt -> Word
   peekS :: (Ptr Field -> IO (Ptr Word8)) -> (Ptr Field -> IO CUInt)
         -> IO ByteString
   peekS peekPtr peekLen = do
     p <- peekPtr ptr
     l <- peekLen ptr
     create (fromIntegral l) $ \d -> memcpy d p (fromIntegral l)

instance Storable Field where
    sizeOf _    = (128)
{-# LINE 229 "Database/MySQL/Base/Types.hsc" #-}
    alignment _ = alignment (undefined :: Ptr CChar)
    peek = peekField

type Seconds = Word

data Protocol = TCP
              | Socket
              | Pipe
              | Memory
                deriving (Eq, Read, Show, Enum, Typeable)

data Option =
            -- Options accepted by mysq_options.
              ConnectTimeout Seconds
            | Compress
            | NamedPipe
            | InitCommand ByteString
            | ReadDefaultFile FilePath
            | ReadDefaultGroup ByteString
            | CharsetDir FilePath
            | CharsetName String
            | LocalInFile Bool
            | Protocol Protocol
            | SharedMemoryBaseName ByteString
            | ReadTimeout Seconds
            | WriteTimeout Seconds
            | UseRemoteConnection
            | UseEmbeddedConnection
            | GuessConnection
            | ClientIP ByteString
            | SecureAuth Bool
            | ReportDataTruncation Bool
            | Reconnect Bool
            | SSLVerifyServerCert Bool
            -- Flags accepted by mysql_real_connect.
            | FoundRows
            | IgnoreSIGPIPE
            | IgnoreSpace
            | Interactive
            | LocalFiles
            | MultiResults
            | MultiStatements
            | NoSchema
              deriving (Eq, Read, Show, Typeable)

toConnectFlag :: Option -> CULong
toConnectFlag Compress  = 32
{-# LINE 276 "Database/MySQL/Base/Types.hsc" #-}
toConnectFlag FoundRows = 2
{-# LINE 277 "Database/MySQL/Base/Types.hsc" #-}
toConnectFlag IgnoreSIGPIPE = 4096
{-# LINE 278 "Database/MySQL/Base/Types.hsc" #-}
toConnectFlag IgnoreSpace = 256
{-# LINE 279 "Database/MySQL/Base/Types.hsc" #-}
toConnectFlag Interactive = 1024
{-# LINE 280 "Database/MySQL/Base/Types.hsc" #-}
toConnectFlag LocalFiles = 128
{-# LINE 281 "Database/MySQL/Base/Types.hsc" #-}
toConnectFlag MultiResults = 131072
{-# LINE 282 "Database/MySQL/Base/Types.hsc" #-}
toConnectFlag MultiStatements = 65536
{-# LINE 283 "Database/MySQL/Base/Types.hsc" #-}
toConnectFlag NoSchema = 16
{-# LINE 284 "Database/MySQL/Base/Types.hsc" #-}
toConnectFlag _        = 0
