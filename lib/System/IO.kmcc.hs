{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
import qualified Prelude as P
import qualified Control.Concurrent as C
import qualified Control.Monad as C (zipWithM)
import qualified System.IO as S
import BasicDefinitions

-- Curryable instance for S.Handle
type instance HsEquivalent S.Handle = S.Handle

instance ToHs S.Handle where
  to = P.return

instance FromHs S.Handle where
  from = P.id
  
instance ShowFree S.Handle where
  showsFreePrec _ _ = showsStringCurry "<<Handle>>"

instance NormalForm S.Handle where
  nfWith _ !x = P.return (P.Right x)
  
instance Narrowable S.Handle where
  narrow = P.error "narrowing a Handle is not possible"
  narrowConstr = P.error "narrowing a Handle is not possible"
  
instance HasPrimitiveInfo S.Handle where
  primitiveInfo = NoPrimitive

instance Unifiable S.Handle where
  unifyWith _ _ _ = P.error "unifying a Handle is not possible"

  lazyUnifyVar _ _ = P.error "unifying a Handle is not possible"
  
instance Curryable S.Handle
  

-- type declarations for handles  
type Handle_Det# = S.Handle
type Handle_ND# = S.Handle

-- foreign instance for Handle
instance ForeignType Handle_Det where
  type Foreign Handle_Det = S.Handle
  toForeign = P.id
  fromForeign = P.id

-- foreign instance for IOMode
instance ForeignType IOMode_Det where
  type Foreign IOMode_Det = S.IOMode
  toForeign ReadMode_Det = S.ReadMode
  toForeign WriteMode_Det = S.WriteMode
  toForeign AppendMode_Det = S.AppendMode
  
  fromForeign S.ReadMode = ReadMode_Det
  fromForeign S.WriteMode = WriteMode_Det
  fromForeign S.AppendMode = AppendMode_Det
  fromForeign _ = P.error "invalid IOMode conversion"
  
-- foreign instance for SeekMode
instance ForeignType SeekMode_Det where
  type Foreign SeekMode_Det = S.SeekMode
  toForeign AbsoluteSeek_Det = S.AbsoluteSeek
  toForeign RelativeSeek_Det = S.RelativeSeek
  toForeign SeekFromEnd_Det = S.SeekFromEnd
  
  fromForeign S.AbsoluteSeek = AbsoluteSeek_Det
  fromForeign S.RelativeSeek = RelativeSeek_Det
  fromForeign S.SeekFromEnd = SeekFromEnd_Det

-- function defintions
iOdothandleuscoreeq_Det# = liftForeign2 (P.==)
iOdothandleuscoreeq_ND# = liftConvert2 iOdothandleuscoreeq_Det#

iOdotstdin_Det# = S.stdin
iOdotstdin_ND# = P.return iOdotstdin_Det#

iOdotstdout_Det# = S.stdout
iOdotstdout_ND# = P.return iOdotstdout_Det#

iOdotstderr_Det# = S.stderr
iOdotstderr_ND# = P.return iOdotstderr_Det#

iOdotprimuscoreopenFile_Det# = liftForeign2 S.openFile
iOdotprimuscoreopenFile_ND# = liftConvertIO2 iOdotprimuscoreopenFile_Det#

iOdotprimuscorehClose_Det# = liftForeign1 S.hClose
iOdotprimuscorehClose_ND# = liftConvertIO1 iOdotprimuscorehClose_Det#

iOdotprimuscorehFlush_Det# = liftForeign1 S.hFlush
iOdotprimuscorehFlush_ND# = liftConvertIO1 iOdotprimuscorehFlush_Det#

iOdotprimuscorehIsEOF_Det# = liftForeign1 S.hIsEOF
iOdotprimuscorehIsEOF_ND# = liftConvertIO1 iOdotprimuscorehIsEOF_Det#

iOdotprimuscorehSeek_Det# x y z = fromForeign P.$ S.hSeek (toForeign x) (toForeign y) (toForeign z)
iOdotprimuscorehSeek_ND# = P.return P.$ from iOdotprimuscorehSeek_Det#

iOdotprimuscorehWaitForInput_Det# x y = fromForeign P.$ S.hWaitForInput x (P.fromInteger y)
iOdotprimuscorehWaitForInput_ND# = liftConvertIO2 iOdotprimuscorehWaitForInput_Det#

iOdotprimuscorehWaitForInputs_Det# handles timeout = fromForeign P.$ (selectHandle (toForeign handles) (P.fromInteger timeout) P.>>= P.return P.. P.toInteger)
iOdotprimuscorehWaitForInputs_ND# = liftConvertIO2 iOdotprimuscorehWaitForInputs_Det#

-- run every handle in its own thread
selectHandle :: [S.Handle] -> P.Int -> P.IO P.Int
selectHandle handles timeout = do
  mvar <- C.newEmptyMVar
  threads <- C.zipWithM
              (\ i h -> C.forkIO (waitOnHandle h i timeout mvar))
              [0 ..] handles
  inspectRes (P.length handles) mvar threads

-- return the handle id if it receives input, otherwise return Nothing
waitOnHandle :: S.Handle -> P.Int -> P.Int -> C.MVar (P.Maybe P.Int) -> P.IO ()
waitOnHandle h v timeout mvar = do
  ready <- S.hWaitForInput h timeout
  C.putMVar mvar (if ready then P.Just v else P.Nothing)

-- check if an id has been returned, otherwise return -1
inspectRes :: P.Int -> C.MVar (P.Maybe P.Int) -> [C.ThreadId] -> P.IO P.Int
inspectRes 0 _    _       = P.return (-1)
inspectRes n mvar threads = do
  res <- C.takeMVar mvar
  case res of
    P.Nothing -> inspectRes (n P.- 1) mvar threads
    P.Just v  -> P.mapM_ C.killThread threads P.>> P.return v

iOdotprimuscorehGetChar_Det# = liftForeign1 S.hGetChar
iOdotprimuscorehGetChar_ND# = liftConvertIO1 iOdotprimuscorehGetChar_Det#

iOdotprimuscorehPutChar_Det# = liftForeign2 S.hPutChar
iOdotprimuscorehPutChar_ND# = liftConvertIO2 iOdotprimuscorehPutChar_Det#

iOdotprimuscorehIsReadable_Det# = liftForeign1 S.hIsReadable
iOdotprimuscorehIsReadable_ND# = liftConvertIO1 iOdotprimuscorehIsReadable_Det#

iOdotprimuscorehIsWritable_Det# = liftForeign1 S.hIsWritable
iOdotprimuscorehIsWritable_ND# = liftConvertIO1 iOdotprimuscorehIsWritable_Det#

iOdotprimuscorehIsTerminalDevice_Det# = liftForeign1 S.hIsTerminalDevice
iOdotprimuscorehIsTerminalDevice_ND# = liftConvertIO1 iOdotprimuscorehIsTerminalDevice_Det#