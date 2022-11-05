createRobot (name, attack, hp) = \message -> message (name, attack, hp)

getName robot = robot name
  where
    name (n, _, _) = n

getAttack robot = robot attack
  where
    attack (_, a, _) = a

getHP robot = robot hp
  where
    hp (_, _, h) = h

setName robot name = robot (\(_, a, hp) -> createRobot (name, a, hp))

setAttack robot attack = robot (\(n, _, hp) -> createRobot (n, attack, hp))

setHP robot hp = robot (\(n, a, _) -> createRobot (n, a, hp))

printRobot robot =
  robot
    ( \(n, a, hp) ->
        n
          ++ " attack:"
          ++ (show a)
          ++ " hp:"
          ++ (show hp)
    )

damage robot attackDamage =
  robot
    (\(n, a, h) -> createRobot (n, a, h - attackDamage))

fight fighter defender = damage defender attack
  where
    attack = if getHP fighter > 0 then getAttack fighter else 0

main :: IO ()
main = do
  print (printRobot fastRobotRound3)
  print (printRobot slowRobotRound3)
  where
    fastRobot = createRobot ("Fastbot", 15, 40)
    slowRobot = createRobot ("Slowbot", 20, 30)
    
    fastRobotRound3 = fight slowRobotRound3 fastRobotRound2
    fastRobotRound2 = fight slowRobotRound2 fastRobotRound1
    fastRobotRound1 = fight slowRobotRound1 fastRobot 
    slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
    slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
    slowRobotRound1 = fight fastRobot slowRobot 
    
