type RuleObject a b = a -> Maybe (IO [b])

grow :: (Applyable a b) => [a] -> (a -> IO (RuleObject a b)) -> IO [b]
grow lsystem ruleGenerator = grow' lsystem ruleGenerator defaultRule []

growN :: (Applyable a a) => [a] -> (a -> IO (RuleObject a a)) -> IO [a]

grow' :: (Applyable a b) => [a] -> (a -> IO (RuleObject a b)) -> [RuleObject a b] -> IO [b]
grow' [] _ _ _ = return []
grow' (x:xs) ruleGenerator rules = case findRule x of
                                       Nothing -> do newRule <- ruleGenerator x
                                                          rest <- grow' xs ruleGenerator (newRule:rules)
                                                          case newRule x of
                                                              Nothing -> return $ defaultRule x
                                                              Just ys -> return $ ys ++ rest
                                       Just result -> do rest <- grow' xs ruleGenerator rules
                                                         ys <- result
                                                         return $ res ++ ys
                                                              