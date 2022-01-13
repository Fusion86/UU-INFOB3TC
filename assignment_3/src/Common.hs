module Common where

import Debug.Pretty.Simple (pTraceOpt)
import Text.Pretty.Simple (pPrintOpt, defaultColorOptionsDarkBg)
import Text.Pretty.Simple.Internal.Printer
  ( CheckColorTty (CheckColorTty),
    OutputOptions (OutputOptions),
    StringOutputStyle (EscapeNonPrintable),
  )

pPrint :: Show a => a -> IO ()
pPrint =
  pPrintOpt
    CheckColorTty
    -- The outputOptionsPageWidth is set to a very large number because it otherwise prints each element on a new line.
    -- Which is a bit awkward when printing a list with 600 items.
    (OutputOptions 2 99999999 True True 0 (Just defaultColorOptionsDarkBg) EscapeNonPrintable)

trace :: String -> a -> a
trace =
  pTraceOpt
    CheckColorTty
    -- The outputOptionsPageWidth is set to a very large number because it otherwise prints each element on a new line.
    -- Which is a bit awkward when printing a list with 600 items.
    (OutputOptions 2 99999999 True True 0 (Just defaultColorOptionsDarkBg) EscapeNonPrintable)

dbg :: Show a => String -> a -> a
dbg str a = trace (str ++ ": " ++ show a) a
