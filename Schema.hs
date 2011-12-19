module Schema where

import Command

schema :: ArgType
schema = ATEither [
  ATSeq [
    ATDocumented (ATToken "echo ") "Write arguments to the standard output",
    ATSet [
      ATDocumented (ATToken "-n ") "Do not print trailing newline"],
      ATList ATString],
  ATSeq [
    ATDocumented (ATToken "cat ") "Concatenate and print files",
    ATSet [
      ATDocumented (ATToken "-b ") "Number the non-blank output lines, starting at 1",
      ATDocumented (ATToken "-e ") "Display non-printing characters (like -v) and EOL",
      ATDocumented (ATToken "-n ") "Number the output lines, starting at 1",
      ATDocumented (ATToken "-s ") "Squeeze multiple adjacent empty lines",
      ATDocumented (ATToken "-t ") "Display non-printing characters (like -v) and tabs",
      ATDocumented (ATToken "-u ") "Disable output buffering",
      ATDocumented (ATToken "-v ") "Display non-printing characters"],
    ATList ATFile],
  ATSeq [
    ATDocumented (ATToken "diff ") "Compare files line by line",
    ATSet [
      ATDocumented (ATEither [ATToken "-i ", ATToken "--ignore-case "]) "Ignore case differences in file contents.",
      ATDocumented (ATEither [ATToken "-w ", ATToken "--ignore-all-space "]) "Ignore all white space.",
      ATDocumented (ATEither [ATToken "-u ",
                              ATSeq [ATToken "-U", atMaybe tokWS, ATInt, tokWS],
                              ATSeq [ATToken "--unified", atMaybe (ATSeq [ATToken "=", ATInt]), tokWS]])
                    "Output NUM (default 3) lines of unified context."
    ],
    ATList ATFile]]