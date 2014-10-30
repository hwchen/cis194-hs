-- test program for main, to compile

import Log
import LogAnalysis

main = do
--    log <- testParse parse 5523 "error.log" --5523
--    let output = inOrder (build log)
--   print output 
    output <- testWhatWentWrong parse whatWentWrong "error.log"
    print output
