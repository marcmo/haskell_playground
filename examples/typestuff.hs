
newtype EmployeeId = EmployeeId Int
data Employee eid  = Employee EmployeeId eid

readEmployees  :: FilePath -> IO [Employee (Maybe EmployeeId)]
readEmployees = undefined
assignId       :: Employee (Maybe EmployeeId) -> IO (Employee EmployeeId)
assignId = undefined
writeEmployees :: FilePath -> [Employee EmployeeId] -> IO ()
writeEmployees = undefined

test inFile outFile = readEmployees inFile >>= mapM assignId >>= writeEmployees outFile

