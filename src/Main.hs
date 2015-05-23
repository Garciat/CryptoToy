import System.IO (openFile, IOMode(..), stdin, stdout)
import System.Environment (getArgs)
import Crypto.Cipher
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS

data Commands = MakeKey | MakeIV | Run deriving (Read)

data Direction = Encrypt | Decrypt deriving (Read)

cbcFunction dir =
  case dir of
    Encrypt -> cbcEncrypt
    Decrypt -> cbcDecrypt

cbcAES ctx dir ivRaw plainText = cbcFunction dir ctx iv plainText
  where iv = maybe (error "invalid IV") id $ makeIV ivRaw

initAES256 :: ByteString -> AES256
initAES256 = either (error . show) cipherInit . makeKey

zeroPadMult n bs = BS.concat [bs, zeroes]
  where
    zeroes = BS.replicate (n - BS.length bs `mod` n) 0

randomByteString n = do
  urandomH <- openFile "/dev/urandom" ReadMode
  bs <- BS.hGet urandomH n
  return bs

outputRandomBytes n = do
  bs <- randomByteString n
  BS.hPut stdout bs

readFileN path n = do
  handle <- openFile path ReadMode
  BS.hGet handle n

encdec keyPath ivPath dir = do
  key <- readFileN keyPath 32
  iv <- readFileN ivPath 16
  inpRaw <- BS.hGetContents stdin
  let inp = zeroPadMult 16 inpRaw
  let aes = initAES256 key
  let out = cbcAES aes dir iv inp
  BS.hPut stdout out

main = do
  (commandArg : args) <- getArgs
  let command = read commandArg
  case command of
    MakeKey -> outputRandomBytes 32
    MakeIV -> outputRandomBytes 16
    Run -> do
      let (directionArg : keyPath : ivPath : _) = args
      encdec keyPath ivPath (read directionArg)
