# Run hlint on modified files
affected_files = git.added_files + git.modified_files

haskell_files = affected_files.select { |file| file.end_with?('.hs') }

hlint.lint(haskell_files, true, hint: "HLint.hs", extension: "hs")

