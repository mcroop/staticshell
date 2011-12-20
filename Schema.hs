module Schema where

import Command

programSchema :: ArgType
programSchema = ATEither [
  ATDocumented (ATSeq [
    ATToken "echo ",
    ATSet [
      ATDocumented (ATToken "-n ") "Do not print trailing newline"],
      ATList ATString]) "Write arguments to the standard output",
  ATDocumented (ATSeq [
    ATToken "cat ",
    ATSet [
      ATDocumented (ATToken "-b ") 
        "Number the non-blank output lines, starting at 1",
      ATDocumented (ATToken "-e ") 
        "Display non-printing characters (like -v) and EOL",
      ATDocumented (ATToken "-n ") "Number the output lines, starting at 1",
      ATDocumented (ATToken "-s ") "Squeeze multiple adjacent empty lines",
      ATDocumented (ATToken "-t ") 
        "Display non-printing characters (like -v) and tabs",
      ATDocumented (ATToken "-u ") "Disable output buffering",
      ATDocumented (ATToken "-v ") "Display non-printing characters"],
    ATList ATFile]) "Concatenate and print files",
  ATSeq [ATToken "QQ ", ATInt, ATString, ATInt, ATString],
  ATDocumented (ATSeq [
    ATToken "diff ",
    ATSet [
      ATDocumented (ATEither [ATToken "-i ", ATToken "--ignore-case "]) 
        "Ignore case differences in file contents.",
      ATDocumented (ATEither [ATToken "-w ", ATToken "--ignore-all-space "]) 
        "Ignore all white space.",
      ATDocumented (ATEither [ATToken "-u ",
                              ATSeq [ATToken "-U", atMaybe tokWS, 
                                     ATInt, 
                                     tokWS],
                              ATSeq [ATToken "--unified", 
                                     atMaybe (ATSeq [ATToken "=", ATInt]), 
                                     tokWS]])
                    "Output NUM (default 3) lines of unified context."
    ],
    ATList ATFile]) "Compare files line by line"
  ]

schema :: ArgType
schema = ATSeq [
  programSchema,
  atMaybe (ATDocumented (ATSeq [ATToken "< ", ATFile]) "Read stdin from file"),
  ATList (ATSeq [ATDocumented (ATToken "| ") "Pipeline", programSchema]),
  atMaybe (ATDocumented (ATSeq [ATToken "> ", ATFile]) "Write stdout to file")]
