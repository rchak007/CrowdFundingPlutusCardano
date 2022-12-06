-- create tmp
-- start :: Contract w s e a
start :: Contract () EmptySchema Text ()
start = logInfo @String "This is the start"