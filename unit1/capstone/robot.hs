robot (name,attack,hp) = \message -> message (name,attack,hp)
name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHp aRobot = aRobot hp

setName aRobot newName = aRobot (\(name, attack, hp) -> robot (newName, attack, hp))
setAttack aRobot newAttack = aRobot (\(name, attack, hp) -> robot (name, newAttack, hp))
setHp aRobot newHp = aRobot (\(name, attack, hp) -> robot (name, attack, newHp))

printRobot aRobot = aRobot (\(name, attack, hp) -> "Name: " ++ name 
                                                    ++ " Attack: " ++ (show attack) 
                                                    ++ " HP: " ++ (show hp))


damage aRobot doneDamage = aRobot (\(name, attack, hp) -> robot(name, attack, hp - doneDamage))

fight aRobot defender = damage defender attack
    where attack = if getHp aRobot > 10
                   then getAttack aRobot
                   else 0