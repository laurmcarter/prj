
import Database.SQLite.Simple

main :: IO ()
main = do
  conn <- open "test.db"
  return ()

