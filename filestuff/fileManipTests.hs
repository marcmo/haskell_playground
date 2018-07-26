import System.FilePath.FindCompat

test = do
  xs <- find always (fileType ==? Directory &&? fileName /=? ".") "."
  print xs

