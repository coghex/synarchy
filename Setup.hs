import Control.Exception (SomeException, displayException, try)
import Control.Monad (when)
import Distribution.Simple (UserHooks (postBuild), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (interpretSymbolicPathLBI)
import Distribution.Types.LocalBuildInfo (buildDir)
import System.Directory (doesDirectoryExist, findExecutable)
import System.Info (os)
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess)

main :: IO ()
main =
    defaultMainWithHooks
        simpleUserHooks
            { postBuild = \args flags pkg lbi -> do
                postBuild simpleUserHooks args flags pkg lbi
                clearMacOSQuarantine (interpretSymbolicPathLBI lbi (buildDir lbi))
            }

clearMacOSQuarantine :: FilePath -> IO ()
clearMacOSQuarantine dir =
    when (os == "darwin") $ do
        exists <- doesDirectoryExist dir
        when exists $ do
            mxattr <- findExecutable "xattr"
            case mxattr of
                Nothing ->
                    hPutStrLn stderr "Setup.hs: xattr not found; built artifacts may remain quarantined"
                Just xattr -> do
                    result <- try (callProcess xattr ["-dr", "com.apple.quarantine", dir])
                    case result of
                        Left err ->
                            hPutStrLn stderr $
                                "Setup.hs: xattr cleanup failed; built artifacts may remain quarantined: "
                                    <> displayException (err :: SomeException)
                        Right () ->
                            pure ()
